package sigmastate

import java.math.BigInteger
import java.util
import java.util.Objects

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{strategy, everywherebu}
import org.ergoplatform.ValidationException
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.validation.ValidationException
import scalan.{Nullable, RType}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Digest32, Blake2b256}
import scalan.util.CollectionUtil._
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.CryptoConstants
import sigmastate.serialization.{OpCodes, ConstantStore, _}
import sigmastate.serialization.OpCodes._
import sigmastate.TrivialProp.{FalseProp, TrueProp}
import sigmastate.Values.ErgoTree.substConstants
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.lang.Terms._
import sigmastate.utxo._
import special.sigma.Extensions._
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigma.util.Extensions.ByteOps

import scala.language.implicitConversions
import scala.reflect.ClassTag
import sigmastate.lang.DefaultSigmaBuilder._
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.transformers.ProveDHTupleSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import special.sigma.{AnyValue, AvlTree, PreHeader, Header, _}
import sigmastate.lang.SourceContext
import special.collection.Coll

import scala.collection.mutable

object Values {

  type SigmaTree = Tree[SigmaNode, SValue]
  type SValue = Value[SType]
  type Idn = String

  trait Value[+S <: SType] extends SigmaNode {
    def companion: ValueCompanion /*=
      sys.error(s"Companion object is not defined for AST node ${this.getClass}")*/

    /** Unique id of the node class used in serialization of ErgoTree. */
    def opCode: OpCode = companion.opCode

    /** The type of the value represented by this node. If the value is an operation it is
      * the type of operation result. */
    def tpe: S

    /** Every value represents an operation and that operation can be associated with a function type,
      * describing functional meaning of the operation, kind of operation signature.
      * Thus we can obtain global operation identifiers by combining Value.opName with Value.opType,
      * so that if (v1.opName == v2.opName) && (v1.opType == v2.opType) then v1 and v2 are functionally
      * point-wise equivalent.
      * This in particular means that if two _different_ ops have the same opType they _should_ have
      * different opNames.
      * Thus defined op ids are used in a Cost Model - a table of all existing primitives coupled with
      * performance parameters.
      * */
    def opType: SFunc

    def opName: String = this.getClass.getSimpleName

    def opId: OperationId = OperationId(opName, opType)

    /** Parser has some source information like line,column in the text. We need to keep it up until RuntimeCosting.
    * The way to do this is to add Nullable property to every Value. Since Parser is always using SigmaBuilder
    * to create nodes,
    * Adding additional (implicit source: SourceContext) parameter to every builder method would pollute its API
    * and also doesn't make sence during deserialization, where Builder is also used.
    * We can assume some indirect mechanism to pass current source context into every mkXXX method of Builder.
    * We can pass it using `scala.util.DynamicVariable` by wrapping each mkXXX call into `withValue { }` calls.
    * The same will happen in Typer.
    * We can take sourceContext from untyped nodes and use it while creating typed nodes.
    * And we can make sourceContext of every Value writeOnce value, i.e. it will be Nullable.Null by default,
    * but can be set afterwards, but only once.
    * This property will not participate in equality and other operations, so will be invisible for existing code.
    * But Builder can use it to set sourceContext if it is present.
    */
    private[sigmastate] var _sourceContext: Nullable[SourceContext] = Nullable.None
    def sourceContext: Nullable[SourceContext] = _sourceContext
    def sourceContext_=(srcCtx: Nullable[SourceContext]): Unit =
      if (_sourceContext.isEmpty) {
        _sourceContext = srcCtx
      } else {
        sys.error("_sourceContext can be set only once")
      }
  }

  object Value {
    type PropositionCode = Byte

    implicit def liftByte (n: Byte) : Value[SByte.type]  = ByteConstant(n)
    implicit def liftShort(n: Short): Value[SShort.type] = ShortConstant(n)
    implicit def liftInt  (n: Int)  : Value[SInt.type]   = IntConstant(n)
    implicit def liftLong (n: Long) : Value[SLong.type]  = LongConstant(n)

    implicit def liftByteArray(arr: Array[Byte]): Value[SByteArray] = ByteArrayConstant(arr)

    implicit def liftBigInt(arr: BigInt): Value[SBigInt.type] = BigIntConstant(arr)

    implicit def liftGroupElement(g: GroupElement): Value[SGroupElement.type] = GroupElementConstant(g)
    implicit def liftECPoint(g: EcPointType): Value[SGroupElement.type] = GroupElementConstant(g)

    implicit def liftSigmaProp(g: SigmaProp): Value[SSigmaProp.type] = SigmaPropConstant(g)
    implicit def liftSigmaBoolean(sb: SigmaBoolean): Value[SSigmaProp.type] = SigmaPropConstant(SigmaDsl.SigmaProp(sb))

    def apply[S <: SType](tS: S)(const: tS.WrappedType): Value[S] = tS.mkConstant(const)

    object Typed {
      def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
    }
    def notSupportedError(v: SValue, opName: String) =
      throw new IllegalArgumentException(s"Method $opName is not supported for node $v")
  }

  trait ValueCompanion extends SigmaNodeCompanion {
    import ValueCompanion._
    /** Unique id of the node class used in serialization of ErgoTree. */
    def opCode: OpCode

    override def toString: Idn = s"${this.getClass.getSimpleName}(${opCode.toUByte})"

    def typeName: String = this.getClass.getSimpleName.replace("$", "")

    def init() {
      if (this.opCode != 0 && _allOperations.contains(this.opCode))
        throw sys.error(s"Operation $this already defined")
      _allOperations += (this.opCode -> this)
    }

    init()

  }
  object ValueCompanion {
    private val _allOperations: mutable.HashMap[Byte, ValueCompanion] = mutable.HashMap.empty
    lazy val allOperations = _allOperations.toMap
  }

  trait EvaluatedValue[+S <: SType] extends Value[S] {
    val value: S#WrappedType
    def opType: SFunc = {
      val resType = tpe match {
        case ct : SCollection[_] =>
          SCollection(ct.typeParams.head.ident)
        case ft @ SFunc(_, _, _) =>
          ft.getGenericType
        case _ => tpe
      }
      SFunc(Vector(), resType)
    }
  }

  trait Constant[+S <: SType] extends EvaluatedValue[S] {}

  case class ConstantNode[S <: SType](value: S#WrappedType, tpe: S) extends Constant[S] {
    assert(Constant.isCorrectType(value, tpe), s"Invalid type of constant value $value, expected type $tpe")
    override def companion: ValueCompanion = Constant
    override def opCode: OpCode = companion.opCode
    override def opName: String = s"Const"

    override def equals(obj: scala.Any): Boolean = (obj != null) && (this.eq(obj.asInstanceOf[AnyRef]) || (obj match {
      case c: Constant[_] => tpe == c.tpe && Objects.deepEquals(value, c.value)
      case _ => false
    }))

    override def hashCode(): Int = util.Arrays.deepHashCode(Array(value.asInstanceOf[AnyRef], tpe))

    override def toString: String = tpe.asInstanceOf[SType] match {
      case SGroupElement if value.isInstanceOf[GroupElement] =>
        s"ConstantNode(${showECPoint(value.asInstanceOf[GroupElement])},$tpe)"
      case SGroupElement  =>
        sys.error(s"Invalid value in Constant($value, $tpe)")
      case SInt => s"IntConstant($value)"
      case SLong => s"LongConstant($value)"
      case SBoolean if value == true => "TrueLeaf"
      case SBoolean if value == false => "FalseLeaf"
      case _ => s"ConstantNode($value,$tpe)"
    }
  }

  object Constant extends ValueCompanion {
    override def opCode: OpCode = ConstantCode
    def apply[S <: SType](value: S#WrappedType, tpe: S): Constant[S] = ConstantNode(value, tpe)
    def unapply[S <: SType](v: EvaluatedValue[S]): Option[(S#WrappedType, S)] = v match {
      case ConstantNode(value, tpe) => Some((value, tpe))
      case _ => None
    }

    /** Checks that the type of the value corresponds to the descriptor `tpe`.
      * If the value has complex structure only root type constructor is checked.
      * NOTE, this is surface check with possible false positives, but it is ok
      * when used in assertions, like `assert(isCorrestType(...))`, see `ConstantNode`.
      */
    def isCorrectType[T <: SType](value: Any, tpe: T): Boolean = value match {
      case c: Coll[_] => tpe match {
        case STuple(items) => c.tItem == RType.AnyType && c.length == items.length
        case tpeColl: SCollection[_] => true
        case _ => sys.error(s"Collection value $c has unexpected type $tpe")
      }
      case _: Option[_] => tpe.isOption
      case _: Tuple2[_,_] => tpe.isTuple && tpe.asTuple.items.length == 2
      case _: Boolean => tpe == SBoolean
      case _: Byte => tpe == SByte
      case _: Short => tpe == SShort
      case _: Int => tpe == SInt
      case _: Long => tpe == SLong
      case _: BigInt => tpe == SBigInt
      case _: String => tpe == SString
      case _: GroupElement => tpe.isGroupElement
      case _: SigmaProp => tpe.isSigmaProp
      case _: AvlTree => tpe.isAvlTree
      case _: Box => tpe.isBox
      case _: PreHeader => tpe == SPreHeader
      case _: Header => tpe == SHeader
      case _: Context => tpe == SContext
      case _: Function1[_,_] => tpe.isFunc
      case _: Unit => tpe == SUnit
      case _ => false
    }
  }

  /** Placeholder for a constant in ErgoTree. Zero based index in ErgoTree.constants array. */
  case class ConstantPlaceholder[S <: SType](id: Int, override val tpe: S) extends Value[S] {
    def opType = SFunc(SInt, tpe)
    override def companion: ValueCompanion = ConstantPlaceholder
  }
  object ConstantPlaceholder extends ValueCompanion {
    override def opCode: OpCode = ConstantPlaceholderCode
  }

  trait NotReadyValue[S <: SType] extends Value[S] {
  }

  /** Base class for references to context variables. */
  trait ContextVariable[S <: SType] extends NotReadyValue[S] {
  }

  /** Reference a context variable by id. */
  trait TaggedVariable[T <: SType] extends ContextVariable[T] {
    val varId: Byte
  }

  case class TaggedVariableNode[T <: SType](varId: Byte, override val tpe: T)
    extends TaggedVariable[T] {
    override def companion = TaggedVariable
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }

  object TaggedVariable extends ValueCompanion {
    override def opCode: OpCode = TaggedVariableCode
    def apply[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
      TaggedVariableNode(varId, tpe)
  }

  case class UnitConstant() extends EvaluatedValue[SUnit.type] {
    override def tpe = SUnit
    val value = ()
    override def companion: ValueCompanion = UnitConstant
  }
  object UnitConstant extends ValueCompanion {
    override val opCode = UnitConstantCode
  }

  type BoolValue = Value[SBoolean.type]
  type ByteValue = Value[SByte.type]
  type ShortValue = Value[SShort.type]
  type IntValue = Value[SInt.type]
  type LongValue = Value[SLong.type]
  type StringValue = Value[SString.type]
  type BigIntValue = Value[SBigInt.type]
  type BoxValue = Value[SBox.type]
  type GroupElementValue = Value[SGroupElement.type]
  type SigmaPropValue = Value[SSigmaProp.type]
  type AvlTreeValue = Value[SAvlTree.type]

  type ByteConstant = Constant[SByte.type]
  type ShortConstant = Constant[SShort.type]
  type IntConstant = Constant[SInt.type]
  type LongConstant = Constant[SLong.type]
  type StringConstant = Constant[SString.type]
  type BigIntConstant = Constant[SBigInt.type]
  type BoxConstant = Constant[SBox.type]
  type GroupElementConstant = Constant[SGroupElement.type]
  type SigmaPropConstant = Constant[SSigmaProp.type]
  type AvlTreeConstant = Constant[SAvlTree.type]

  object ByteConstant {
    def apply(value: Byte): Constant[SByte.type] = Constant[SByte.type](value, SByte)
    def unapply(v: SValue): Option[Byte] = v match {
      case Constant(value: Byte, SByte) => Some(value)
      case _ => None
    }
  }
  object ShortConstant {
    def apply(value: Short): Constant[SShort.type]  = Constant[SShort.type](value, SShort)
    def unapply(v: SValue): Option[Short] = v match {
      case Constant(value: Short, SShort) => Some(value)
      case _ => None
    }
  }
  object IntConstant {
    def apply(value: Int): Constant[SInt.type]  = Constant[SInt.type](value, SInt)
    def unapply(v: SValue): Option[Int] = v match {
      case Constant(value: Int, SInt) => Some(value)
      case _ => None
    }

    def Zero = IntConstant(0)
    def One = IntConstant(1)
  }
  object LongConstant {
    def apply(value: Long): Constant[SLong.type]  = Constant[SLong.type](value, SLong)
    def unapply(v: SValue): Option[Long] = v match {
      case Constant(value: Long, SLong) => Some(value)
      case _ => None
    }
  }
  object BigIntConstant {
    def apply(value: BigInt): Constant[SBigInt.type] = Constant[SBigInt.type](value, SBigInt)
    def apply(value: BigInteger): Constant[SBigInt.type] = Constant[SBigInt.type](SigmaDsl.BigInt(value), SBigInt)
    def apply(value: Long): Constant[SBigInt.type] = Constant[SBigInt.type](SigmaDsl.BigInt(BigInteger.valueOf(value)), SBigInt)
    def unapply(v: SValue): Option[BigInt] = v match {
      case Constant(value: BigInt, SBigInt) => Some(value)
      case _ => None
    }
  }

  object StringConstant {
    def apply(value: String): Constant[SString.type]  = Constant[SString.type](value, SString)
    def unapply(v: SValue): Option[String] = v match {
      case Constant(value: String, SString) => Some(value)
      case _ => None
    }
    def Empty = StringConstant("")
  }

  object BoxConstant {
    def apply(value: Box): Constant[SBox.type] = Constant[SBox.type](value, SBox)
    def unapply(v: SValue): Option[Box] = v match {
      case Constant(value: Box, SBox) => Some(value)
      case _ => None
    }
  }

  object GroupElementConstant {
    def apply(value: GroupElement): Constant[SGroupElement.type] = Constant[SGroupElement.type](value, SGroupElement)
    def unapply(v: SValue): Option[GroupElement] = v match {
      case Constant(value: GroupElement, SGroupElement) => Some(value)
      case _ => None
    }
  }

  val FalseSigmaProp = SigmaPropConstant(SigmaDsl.SigmaProp(TrivialProp.FalseProp))
  val TrueSigmaProp = SigmaPropConstant(SigmaDsl.SigmaProp(TrivialProp.TrueProp))

  implicit def boolToSigmaProp(b: BoolValue): SigmaPropValue = BoolToSigmaProp(b)

  object SigmaPropConstant {
    def apply(value: SigmaProp): Constant[SSigmaProp.type] = Constant[SSigmaProp.type](value, SSigmaProp)
    def apply(value: SigmaBoolean): Constant[SSigmaProp.type] = Constant[SSigmaProp.type](SigmaDsl.SigmaProp(value), SSigmaProp)
    def unapply(v: SValue): Option[SigmaProp] = v match {
      case Constant(value: SigmaProp, SSigmaProp) => Some(value)
      case _ => None
    }
  }

  object AvlTreeConstant {
    def apply(value: AvlTree): Constant[SAvlTree.type] = Constant[SAvlTree.type](value, SAvlTree)
    def unapply(v: SValue): Option[AvlTree] = v match {
      case Constant(value: AvlTree, SAvlTree) => Some(value)
      case _ => None
    }
  }

  implicit class AvlTreeConstantOps(val c: AvlTreeConstant) extends AnyVal {
    def createVerifier(proof: SerializedAdProof) =
      new BatchAVLVerifier[Digest32, Blake2b256.type](
        ADDigest @@ c.value.digest.toArray,
        proof,
        c.value.keyLength,
        c.value.valueLengthOpt)
  }

  object ContextConstant {
    def apply(value: ErgoLikeContext): Constant[SContext.type]  = Constant[SContext.type](value, SContext)
    def unapply(v: SValue): Option[ErgoLikeContext] = v match {
      case Constant(value: ErgoLikeContext, SContext) => Some(value)
      case _ => None
    }
  }

  object PreHeaderConstant {
    def apply(value: PreHeader): Constant[SPreHeader.type]  = Constant[SPreHeader.type](value, SPreHeader)
    def unapply(v: SValue): Option[PreHeader] = v match {
      case Constant(value: PreHeader, SPreHeader) => Some(value)
      case _ => None
    }
  }
  
  object HeaderConstant {
    def apply(value: Header): Constant[SHeader.type]  = Constant[SHeader.type](value, SHeader)
    def unapply(v: SValue): Option[Header] = v match {
      case Constant(value: Header, SHeader) => Some(value)
      case _ => None
    }
  }

  trait NotReadyValueByte extends NotReadyValue[SByte.type] {
    override def tpe = SByte
  }

  trait NotReadyValueShort extends NotReadyValue[SShort.type] {
    override def tpe = SShort
  }

  trait NotReadyValueInt extends NotReadyValue[SInt.type] {
    override def tpe = SInt
  }

  trait NotReadyValueLong extends NotReadyValue[SLong.type] {
    override def tpe = SLong
  }

  trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
    override def tpe = SBigInt
  }

  type TaggedBoolean = TaggedVariable[SBoolean.type]
  type TaggedByte = TaggedVariable[SByte.type]
  type TaggedShort = TaggedVariable[SShort.type]
  type TaggedInt = TaggedVariable[SInt.type]
  type TaggedLong = TaggedVariable[SLong.type]
  type TaggedBigInt = TaggedVariable[SBigInt.type]
  type TaggedBox = TaggedVariable[SBox.type]
  type TaggedGroupElement = TaggedVariable[SGroupElement.type]
  type TaggedSigmaProp = TaggedVariable[SSigmaProp.type]
  type TaggedAvlTree = TaggedVariable[SAvlTree.type]
  type TaggedByteArray = TaggedVariable[SCollection[SByte.type]]

  def TaggedBoolean(id: Byte): Value[SBoolean.type] = mkTaggedVariable(id, SBoolean)
  def TaggedByte(id: Byte): Value[SByte.type] = mkTaggedVariable(id, SByte)
  def TaggedShort(id: Byte): Value[SShort.type] = mkTaggedVariable(id, SShort)
  def TaggedInt(id: Byte): Value[SInt.type] = mkTaggedVariable(id, SInt)
  def TaggedLong(id: Byte): Value[SLong.type] = mkTaggedVariable(id, SLong)
  def TaggedBigInt(id: Byte): Value[SBigInt.type] = mkTaggedVariable(id, SBigInt)
  def TaggedBox(id: Byte): Value[SBox.type] = mkTaggedVariable(id, SBox)
  def TaggedGroupElement(id: Byte): Value[SGroupElement.type] =
    mkTaggedVariable(id, SGroupElement)
  def TaggedSigmaProp(id: Byte): TaggedSigmaProp = TaggedVariable(id, SSigmaProp)
  def TaggedAvlTree(id: Byte): Value[SAvlTree.type] = mkTaggedVariable(id, SAvlTree)
  def TaggedByteArray (id: Byte): Value[SCollection[SByte.type]] =
    mkTaggedVariable(id, SByteArray)

  trait EvaluatedCollection[T <: SType, C <: SCollection[T]] extends EvaluatedValue[C] {
    def elementType: T
  }

  type OptionConstant[T <: SType] = Constant[SOption[T]]
  object OptionConstant {
    def apply[T <: SType](value: Option[T#WrappedType], elementType: T): Constant[SOption[T]] =
      Constant[SOption[T]](value, SOption(elementType))
    def unapply[T <: SType](node: Value[SOption[T]]): Option[(Option[T#WrappedType], T)] = node match {
      case opt: Constant[SOption[a]] @unchecked if opt.tpe.isOption =>
        val v = opt.value.asInstanceOf[Option[T#WrappedType]]
        val t = opt.tpe.elemType.asInstanceOf[T]
        Some((v, t))
      case _ => None
    }
  }

  type CollectionConstant[T <: SType] = Constant[SCollection[T]]

  object CollectionConstant {
    def apply[T <: SType](value: Coll[T#WrappedType], elementType: T): Constant[SCollection[T]] =
      Constant[SCollection[T]](value, SCollection(elementType))
    def unapply[T <: SType](node: Value[SCollection[T]]): Option[(Coll[T#WrappedType], T)] = node match {
      case c: Constant[SCollection[a]] @unchecked if c.tpe.isCollection =>
        val v = c.value.asInstanceOf[Coll[T#WrappedType]]
        val t = c.tpe.elemType
        Some((v, t))
      case _ => None
    }
  }

  implicit class CollectionConstantOps[T <: SType](val c: CollectionConstant[T]) extends AnyVal {
    def toConcreteCollection: ConcreteCollection[T] = {
      val tElem = c.tpe.elemType
      val items = c.value.toArray.map(v => tElem.mkConstant(v.asInstanceOf[tElem.WrappedType]))
      ConcreteCollection(items, tElem)
    }
  }

  val ByteArrayTypeCode = (SCollectionType.CollectionTypeCode + SByte.typeCode).toByte

  object ByteArrayConstant {
    def apply(value: Coll[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value, SByte)
    def apply(value: Array[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value.toColl, SByte)
    def unapply(node: SValue): Option[Coll[Byte]] = node match {
      case coll: CollectionConstant[SByte.type] @unchecked => coll match {
        case CollectionConstant(arr, SByte) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object ShortArrayConstant {
    def apply(value: Coll[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value, SShort)
    def apply(value: Array[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value.toColl, SShort)
    def unapply(node: SValue): Option[Coll[Short]] = node match {
      case coll: CollectionConstant[SShort.type] @unchecked => coll match {
        case CollectionConstant(arr, SShort) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object IntArrayConstant {
    def apply(value: Coll[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value, SInt)
    def apply(value: Array[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value.toColl, SInt)
    def unapply(node: SValue): Option[Coll[Int]] = node match {
      case coll: CollectionConstant[SInt.type] @unchecked => coll match {
        case CollectionConstant(arr, SInt) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object LongArrayConstant {
    def apply(value: Coll[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value, SLong)
    def apply(value: Array[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value.toColl, SLong)
    def unapply(node: SValue): Option[Coll[Long]] = node match {
      case coll: CollectionConstant[SLong.type] @unchecked => coll match {
        case CollectionConstant(arr, SLong) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object BigIntArrayConstant {
    def apply(value: Coll[BigInt]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value, SBigInt)
    def apply(value: Array[BigInt]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value.toColl, SBigInt)
    def unapply(node: SValue): Option[Coll[BigInt]] = node match {
      case coll: CollectionConstant[SBigInt.type] @unchecked => coll match {
        case CollectionConstant(arr, SBigInt) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  val BoolArrayTypeCode = (SCollectionType.CollectionTypeCode + SBoolean.typeCode).toByte

  object BoolArrayConstant {
    def apply(value: Coll[Boolean]): CollectionConstant[SBoolean.type] = CollectionConstant[SBoolean.type](value, SBoolean)
    def apply(value: Array[Boolean]): CollectionConstant[SBoolean.type] = CollectionConstant[SBoolean.type](value.toColl, SBoolean)
    def unapply(node: SValue): Option[Coll[Boolean]] = node match {
      case coll: CollectionConstant[SBoolean.type] @unchecked => coll match {
        case CollectionConstant(arr, SBoolean) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  trait NotReadyValueByteArray extends NotReadyValue[SByteArray] {
    override def tpe = SByteArray
  }

  trait NotReadyValueAvlTree extends NotReadyValue[SAvlTree.type] {
    override def tpe = SAvlTree
  }

  case object GroupGenerator extends EvaluatedValue[SGroupElement.type] with ValueCompanion {
    override def opCode: OpCode = OpCodes.GroupGeneratorCode
    override def tpe = SGroupElement
    override val value = SigmaDsl.GroupElement(CryptoConstants.dlogGroup.generator)
    override def companion = this
  }


  trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
    override def tpe = SGroupElement
  }

  type BooleanConstant = Constant[SBoolean.type]

  object BooleanConstant {
    def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf
    def apply(value: Boolean): BooleanConstant = Constant[SBoolean.type](value, SBoolean)
    def unapply(v: SValue): Option[Boolean] = v match {
      case Constant(value: Boolean, SBoolean) => Some(value)
      case _ => None
    }
  }

  object TrueLeaf extends ConstantNode[SBoolean.type](true, SBoolean) with ValueCompanion {
    override def companion = this
    override def opCode: OpCode = TrueCode
  }

  object FalseLeaf extends ConstantNode[SBoolean.type](false, SBoolean) with ValueCompanion {
    override def companion = this
    override def opCode: OpCode = FalseCode
  }

  trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
    override def tpe = SBoolean
  }

  /** Algebraic data type of sigma proposition expressions.
    * Values of this type are used as values of SigmaProp type of SigmaScript and SigmaDsl
    */
  trait SigmaBoolean {
    /** Unique id of the node class used in serialization of SigmaBoolean. */
    val opCode: OpCode
  }

  object SigmaBoolean {
    val PropBytes = "propBytes"
    val IsValid = "isValid"
    object serializer extends SigmaSerializer[SigmaBoolean, SigmaBoolean] {
      val dhtSerializer = ProveDHTupleSerializer(ProveDHTuple.apply)
      val dlogSerializer = ProveDlogSerializer(ProveDlog.apply)

      override def serialize(data: SigmaBoolean, w: SigmaByteWriter): Unit = {
        w.put(data.opCode)
        data match {
          case dlog: ProveDlog   => dlogSerializer.serialize(dlog, w)
          case dht: ProveDHTuple => dhtSerializer.serialize(dht, w)
          case _: TrivialProp => // besides opCode no additional bytes
          case and: CAND =>
            w.putUShort(and.sigmaBooleans.length)
            for (c <- and.sigmaBooleans)
              serializer.serialize(c, w)
          case or: COR =>
            w.putUShort(or.sigmaBooleans.length)
            for (c <- or.sigmaBooleans)
              serializer.serialize(c, w)
          case th: CTHRESHOLD =>
            w.putUShort(th.k)
            w.putUShort(th.sigmaBooleans.length)
            for (c <- th.sigmaBooleans)
              serializer.serialize(c, w)
        }
      }

      override def parse(r: SigmaByteReader): SigmaBoolean = {
        val depth = r.level
        r.level = depth + 1
        val opCode = r.getByte()
        val res = opCode match {
          case FalseProp.opCode => FalseProp
          case TrueProp.opCode  => TrueProp
          case ProveDlogCode => dlogSerializer.parse(r)
          case ProveDHTupleCode => dhtSerializer.parse(r)
          case AndCode =>
            val n = r.getUShort()
            val children = (0 until n).map(_ => serializer.parse(r))
            CAND(children)
          case OrCode =>
            val n = r.getUShort()
            val children = (0 until n).map(_ => serializer.parse(r))
            COR(children)
          case AtLeastCode =>
            val k = r.getUShort()
            val n = r.getUShort()
            val children = (0 until n).map(_ => serializer.parse(r))
            CTHRESHOLD(k, children)
        }
        r.level = r.level - 1
        res
      }
    }
  }

  trait NotReadyValueBox extends NotReadyValue[SBox.type] {
    def tpe = SBox
  }

  case class Tuple(items: IndexedSeq[Value[SType]]) extends EvaluatedValue[STuple] with EvaluatedCollection[SAny.type, STuple] {
    override def companion = Tuple
    override def elementType = SAny
    lazy val tpe = STuple(items.map(_.tpe))
    lazy val value = {
      val xs = items.cast[EvaluatedValue[SAny.type]].map(_.value)
      Colls.fromArray(xs.toArray(SAny.classTag.asInstanceOf[ClassTag[SAny.WrappedType]]))(RType.AnyType)
    }
  }

  object Tuple extends ValueCompanion {
    override def opCode: OpCode = TupleCode
    def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)
  }

  trait OptionValue[T <: SType] extends Value[SOption[T]] {
  }

  case class SomeValue[T <: SType](x: Value[T]) extends OptionValue[T] {
    override def companion = SomeValue
    val tpe = SOption(x.tpe)
    def opType = SFunc(x.tpe, tpe)
  }
  object SomeValue extends ValueCompanion {
    override val opCode = SomeValueCode
  }

  case class NoneValue[T <: SType](elemType: T) extends OptionValue[T] {
    override def companion = NoneValue
    val tpe = SOption(elemType)
    def opType = SFunc(elemType, tpe)
  }
  object NoneValue extends ValueCompanion {
    override val opCode = NoneValueCode
  }

  case class ConcreteCollection[V <: SType](items: IndexedSeq[Value[V]], elementType: V)
    extends EvaluatedCollection[V, SCollection[V]] {
    private val isBooleanConstants = elementType == SBoolean && items.forall(_.isInstanceOf[Constant[_]])
    override def companion =
      if (isBooleanConstants) ConcreteCollectionBooleanConstant
      else ConcreteCollection

    val tpe = SCollection[V](elementType)

    lazy val value = {
      val xs = items.cast[EvaluatedValue[V]].map(_.value)
      val tElement = Evaluation.stypeToRType(elementType)
      Colls.fromArray(xs.toArray(elementType.classTag.asInstanceOf[ClassTag[V#WrappedType]]))(tElement)
    }
  }
  object ConcreteCollection extends ValueCompanion {
    override def opCode: OpCode = ConcreteCollectionCode
    def apply[V <: SType](items: Value[V]*)(implicit tV: V): ConcreteCollection[V] =
      ConcreteCollection(items.toIndexedSeq, tV)

    def apply[V <: SType](items: => Seq[Value[V]])(implicit tV: V): ConcreteCollection[V] =
      ConcreteCollection(items.toIndexedSeq, tV)
  }
  object ConcreteCollectionBooleanConstant extends ValueCompanion {
    override def opCode: OpCode = ConcreteCollectionBooleanConstantCode
  }

  trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]

  implicit class CollectionOps[T <: SType](val coll: Value[SCollection[T]]) extends AnyVal {
    def length: Int = matchCase(_.items.length, _.value.length, _.items.length)
    def items = matchCase(_.items, _ => sys.error(s"Cannot get 'items' property of node $coll"), _.items)
//    def isEvaluatedCollection =
//      coll.evaluated && matchCase(_.items.forall(_.evaluated), _ => true, _.items.forall(_.evaluated))
    def matchCase[R](
        whenConcrete: ConcreteCollection[T] => R,
        whenConstant: CollectionConstant[T] => R,
        whenTuple: Tuple => R
    ): R = coll match {
      case cc: ConcreteCollection[T]@unchecked => whenConcrete(cc)
      case const: CollectionConstant[T]@unchecked => whenConstant(const)
      case tuple: Tuple => whenTuple(tuple)
      case _ => sys.error(s"Unexpected node $coll")
    }
    def toConcreteCollection: ConcreteCollection[T] =
      matchCase(
        cc => cc,
        _.toConcreteCollection,
        t => ConcreteCollection(t.items.map(_.asValue[T]), SAny.asInstanceOf[T])
      )
  }

  implicit class SigmaPropValueOps(val p: Value[SSigmaProp.type]) extends AnyVal {
    def isProven: Value[SBoolean.type] = SigmaPropIsProven(p)
    def propBytes: Value[SByteArray] = SigmaPropBytes(p)
    def treeWithSegregation: ErgoTree = ErgoTree.withSegregation(p)
  }

  implicit class SigmaBooleanOps(val sb: SigmaBoolean) extends AnyVal {
    def toSigmaProp: SigmaPropValue = SigmaPropConstant(sb)
    def isProven: Value[SBoolean.type] = SigmaPropIsProven(SigmaPropConstant(sb))
    def propBytes: Value[SByteArray] = SigmaPropBytes(SigmaPropConstant(sb))
    def toAnyValue: AnyValue = eval.Extensions.toAnyValue(sb)(SType.SigmaBooleanRType)
    def showToString: String = sb match {
      case ProveDlog(v) =>
        s"ProveDlog(${showECPoint(v)})"
      case ProveDHTuple(gv, hv, uv, vv) =>
        s"ProveDHTuple(${showECPoint(gv)}, ${showECPoint(hv)}, ${showECPoint(uv)}, ${showECPoint(vv)})"
      case _ => sb.toString
    }
  }

  implicit class BoolValueOps(val b: BoolValue) extends AnyVal {
    def toSigmaProp: SigmaPropValue = b match {
      case b if b.tpe == SBoolean => BoolToSigmaProp(b)
      case p if p.tpe == SSigmaProp => p.asSigmaProp
      case _ => sys.error(s"Expected SBoolean or SSigmaProp typed value, but was: $b")
    }
  }

  sealed trait BlockItem extends NotReadyValue[SType] {
    def id: Int
    def rhs: SValue
    def isValDef: Boolean
  }

  /** IR node for let-bound expressions `let x = rhs` which is ValDef, or `let f[T] = rhs` which is FunDef.
    * These nodes are used to represent ErgoTrees after common sub-expression elimination.
    * This representation is more compact in serialized form.
    * @param id unique identifier of the variable in the current scope. */
  case class ValDef(override val id: Int,
                    tpeArgs: Seq[STypeVar],
                    override val rhs: SValue) extends BlockItem {
    require(id >= 0, "id must be >= 0")
    override def companion = if (tpeArgs.isEmpty) ValDef else FunDef
    def tpe: SType = rhs.tpe
    def isValDef: Boolean = tpeArgs.isEmpty
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object ValDef extends ValueCompanion {
    def opCode: OpCode = ValDefCode
    def apply(id: Int, rhs: SValue): ValDef = ValDef(id, Nil, rhs)
  }
  object FunDef extends ValueCompanion {
    def opCode: OpCode = FunDefCode
    def unapply(d: BlockItem): Option[(Int, Seq[STypeVar], SValue)] = d match {
      case ValDef(id, targs, rhs) if !d.isValDef => Some((id, targs, rhs))
      case _ => None
    }
  }

  /** Special node which represents a reference to ValDef in was introduced as result of CSE. */
  case class ValUse[T <: SType](valId: Int, tpe: T) extends NotReadyValue[T] {
    override def companion = ValUse
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object ValUse extends ValueCompanion {
    override def opCode: OpCode = ValUseCode
  }

  /** The order of ValDefs in the block is used to assign ids to ValUse(id) nodes
    * For all i: items(i).id == {number of ValDefs preceded in a graph} with respect to topological order.
    * Specific topological order doesn't really matter, what is important is to preserve semantic linkage
    * between ValUse(id) and ValDef with the corresponding id.
    * This convention allow to valid serializing ids because we always serializing and deserializing
    * in a fixed well defined order.
    */
  case class BlockValue(items: IndexedSeq[BlockItem], result: SValue) extends NotReadyValue[SType] {
    override def companion = BlockValue
    def tpe: SType = result.tpe
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object BlockValue extends ValueCompanion {
    override def opCode: OpCode = BlockValueCode
  }
  /**
    * @param args parameters list, where each parameter has an id and a type.
    * @param body expression, which refers function parameters with ValUse.
    */
  case class FuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]) extends NotReadyValue[SFunc] {
    override def companion = FuncValue
    lazy val tpe: SFunc = SFunc(args.map(_._2), body.tpe)
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = SFunc(Vector(), tpe)
  }
  object FuncValue extends ValueCompanion {
    override def opCode: OpCode = FuncValueCode
    def apply(argId: Int, tArg: SType, body: SValue): FuncValue =
      FuncValue(IndexedSeq((argId,tArg)), body)
  }

  implicit class OptionValueOps[T <: SType](val p: Value[SOption[T]]) extends AnyVal {
    def get: Value[T] = OptionGet(p)
    def getOrElse(default: Value[T]): Value[T] = OptionGetOrElse(p, default)
    def isDefined: Value[SBoolean.type] = OptionIsDefined(p)
  }

  def GetVarBoolean(varId: Byte): GetVar[SBoolean.type] = GetVar(varId, SBoolean)
  def GetVarByte(varId: Byte): GetVar[SByte.type] = GetVar(varId, SByte)
  def GetVarShort(varId: Byte): GetVar[SShort.type] = GetVar(varId, SShort)
  def GetVarInt(varId: Byte): GetVar[SInt.type] = GetVar(varId, SInt)
  def GetVarLong(varId: Byte): GetVar[SLong.type] = GetVar(varId, SLong)
  def GetVarBigInt(varId: Byte): GetVar[SBigInt.type] = GetVar(varId, SBigInt)
  def GetVarBox(varId: Byte): GetVar[SBox.type] = GetVar(varId, SBox)
  def GetVarSigmaProp(varId: Byte): GetVar[SSigmaProp.type] = GetVar(varId, SSigmaProp)
  def GetVarByteArray(varId: Byte): GetVar[SCollection[SByte.type]] = GetVar(varId, SByteArray)

  /** This is alternative representation of ErgoTree expression when it cannot be parsed
    * due to `error`. This is used by the nodes running old versions of code to recognize
    * soft-fork conditions and skip validation of box propositions which are unparsable. */
  case class UnparsedErgoTree(bytes: mutable.WrappedArray[Byte], error: ValidationException)

  /** The root of ErgoScript IR. Serialized instances of this class are self sufficient and can be passed around.
    * ErgoTreeSerializer defines top-level serialization format of the scripts.
    * The interpretation of the byte array depend on the first `header` byte, which uses VLQ encoding up to 30 bits.
    * Currently we define meaning for only first byte, which may be extended in future versions.
    *   7  6  5  4  3  2  1  0
    * -------------------------
    * |  |  |  |  |  |  |  |  |
    * -------------------------
    *  Bit 7 == 1 if the header contains more than 1 byte (default == 0)
    *  Bit 6 - reserved for GZIP compression (should be 0)
    *  Bit 5 == 1 - reserved for context dependent costing (should be = 0)
    *  Bit 4 == 1 if constant segregation is used for this ErgoTree (default = 0)
    *                (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/264)
    *  Bit 3 == 1 if size of the whole tree is serialized after the header byte (default = 0)
    *  Bits 2-0 - language version (current version == 0)
    *
    *  Currently we don't specify interpretation for the second and other bytes of the header.
    *  We reserve the possibility to extend header by using Bit 7 == 1 and chain additional bytes as in VLQ.
    *  Once the new bytes are required, a new version of the language should be created and implemented.
    *  That new language will give an interpretation for the new bytes.
    *
    *  Consistency between fields is ensured by private constructor and factory methods in `ErgoTree` object.
    *  For performance reasons, ErgoTreeSerializer can be configured to perform additional constant segregation.
    *  In such a case after deserialization there may be more constants segregated. This is done for example to
    *  support caching optimization described in #264 mentioned above.
    *
    *  The default behavior of ErgoTreeSerializer is to preserve original structure of ErgoTree and check
    *  consistency. In case of any inconsistency the serializer throws exception.
    *  
    *  @param header      the first byte of serialized byte array which determines interpretation of the rest of the array
    *
    *  @param constants   If isConstantSegregation == true contains the constants for which there may be
    *                     ConstantPlaceholders in the tree.
    *                     If isConstantSegregation == false this array should be empty and any placeholder in
    *                     the tree will lead to exception.
    *
    *  @param root        On the right side it has valid expression of `SigmaProp` type. Or alternatively,
    *                     on the left side, it has unparsed bytes along with the ValidationException,
    *                     which caused the deserializer to fail.
    *                     `Right(tree)` if isConstantSegregation == true contains ConstantPlaceholder
    *                     instead of some Constant nodes. Otherwise, it may not contain placeholders.
    *                     It is possible to have both constants and placeholders in the tree, but for every placeholder
    *                     there should be a constant in `constants` array.
    */
  case class ErgoTree private(
    header: Byte,
    constants: IndexedSeq[Constant[SType]],
    root: Either[UnparsedErgoTree, SigmaPropValue]
  ) {
    require(isConstantSegregation || constants.isEmpty)
    require(version == 0 || hasSize, s"For newer version the size bit is required: $this")

    /** Then it throws the error from UnparsedErgoTree.
      * It does so on every usage of `proposition` because the lazy value remains uninitialized.
      */
    @deprecated("Use toProposition instead", "v2.1")
    lazy val proposition: SigmaPropValue = toProposition(isConstantSegregation)

    @inline def version: Byte = ErgoTree.getVersion(header)
    @inline def isRightParsed: Boolean = root.isRight
    @inline def isConstantSegregation: Boolean = ErgoTree.isConstantSegregation(header)
    @inline def hasSize: Boolean = ErgoTree.hasSize(header)
    @inline def bytes: Array[Byte] = DefaultSerializer.serializeErgoTree(this)

    /** Get proposition expression from this contract.
      * When root.isRight then
      *   if replaceConstants == false this is the same as `root.right.get`.
      *   Otherwise, it is equivalent to `root.right.get` where all placeholders are replaced by Constants.
      * When root.isLeft then
      *   throws the error from UnparsedErgoTree.
      *   It does so on every usage of `proposition` because the lazy value remains uninitialized.
      */
    def toProposition(replaceConstants: Boolean): SigmaPropValue = root match {
      case Right(tree) =>
        val prop = if (replaceConstants)
          substConstants(tree, constants).asSigmaProp
        else
          tree
        prop
      case Left(UnparsedErgoTree(_, error)) =>
        throw error
    }
  }

  object ErgoTree {
    /** Current version of ErgoTree serialization format (aka bite-code language version)*/
    val VersionFlag: Byte = 0

    /** Default value of ErgoTree.header byte */
    val DefaultHeader: Byte = (0 | VersionFlag).toByte

    /** Header flag to indicate that constant segregation should be applied. */
    val ConstantSegregationFlag: Byte = 0x10

    /** Header flag to indicate that whole size of ErgoTree should be saved before tree content. */
    val SizeFlag: Byte = 0x08

    /** Header mask to extract version bits. */
    val VersionMask: Byte = 0x07

    /** Default header with constant segregation enabled. */
    val ConstantSegregationHeader: Byte = (DefaultHeader | ConstantSegregationFlag).toByte

    @inline def isConstantSegregation(header: Byte): Boolean = (header & ConstantSegregationFlag) != 0
    @inline def hasSize(header: Byte): Boolean = (header & SizeFlag) != 0
    @inline def getVersion(header: Byte): Byte = (header & VersionMask).toByte

    def substConstants(root: SValue, constants: IndexedSeq[Constant[SType]]): SValue = {
      val store = new ConstantStore(constants)
      val substRule = strategy[Value[_ <: SType]] {
        case ph: ConstantPlaceholder[_] =>
          Some(store.get(ph.id))
        case _ => None
      }
      everywherebu(substRule)(root).fold(root)(_.asInstanceOf[SValue])
    }

    def apply(header: Byte, constants: IndexedSeq[Constant[SType]], root: SigmaPropValue): ErgoTree = {
      new ErgoTree(header, constants, Right(root))
    }

    val EmptyConstants: IndexedSeq[Constant[SType]] = IndexedSeq.empty[Constant[SType]]

    def withoutSegregation(root: SigmaPropValue): ErgoTree =
      ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, root)

    implicit def fromProposition(prop: SigmaPropValue): ErgoTree = {
      prop match {
        case SigmaPropConstant(_) => withoutSegregation(prop)
        case _ => withSegregation(prop)
      }
    }

    implicit def fromSigmaBoolean(pk: SigmaBoolean): ErgoTree = {
      withoutSegregation(pk.toSigmaProp)
    }

    /** Build ErgoTree via serialization of the value with ConstantSegregationHeader, constants segregated
      * from the tree and ConstantPlaceholders referring to the segregated constants.
      *
      * This method uses single traverse of the tree to:
      * 1) find and segregate all constants;
      * 2) replace constants with ConstantPlaceholders in the `tree`;
      * 3) write the `tree` to the Writer's buffer obtaining `treeBytes`;
      * 4) deserialize `tree` with ConstantPlaceholders.
      **/
    def withSegregation(headerFlags: Byte, value: SigmaPropValue): ErgoTree = {
      val constantStore = new ConstantStore()
      val byteWriter = SigmaSerializer.startWriter(constantStore)
      // serialize value and segregate constants into constantStore
      ValueSerializer.serialize(value, byteWriter)
      val extractedConstants = constantStore.getAll
      val r = SigmaSerializer.startReader(byteWriter.toBytes)
      r.constantStore = new ConstantStore(extractedConstants)
      // deserialize value with placeholders
      val valueWithPlaceholders = ValueSerializer.deserialize(r).asSigmaProp
      val header = (ErgoTree.ConstantSegregationHeader | headerFlags).toByte
      new ErgoTree(header, extractedConstants, Right(valueWithPlaceholders))
    }

    def withSegregation(value: SigmaPropValue): ErgoTree =
      withSegregation(0, value)
  }

}
