package sigmastate

import java.math.BigInteger
import java.util.{Arrays, Objects}

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, strategy}
import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import scalan.Nullable
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256, Digest32}
import scalan.util.CollectionUtil._
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{Context, CryptoConstants, CryptoFunctions}
import sigmastate.serialization.{ConstantStore, ErgoTreeSerializer, OpCodes, ValueSerializer}
import sigmastate.serialization.OpCodes._
import sigmastate.utxo.CostTable.Cost
import sigma.util.Extensions._
import sigmastate.lang.Terms._
import sigmastate.utxo._
import special.sigma.Extensions._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import sigmastate.lang.DefaultSigmaBuilder._
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import special.sigma.{Extensions, AnyValue, TestValue}
import sigmastate.lang.SourceContext


object Values {

  type SigmaTree = Tree[SigmaNode, SValue]
  type SValue = Value[SType]
  type Idn = String

  trait Value[+S <: SType] extends SigmaNode {
    def companion: ValueCompanion =
      sys.error(s"Companion object is not defined for AST node ${this.getClass}")

    /** Unique id of the node class used in serialization of ErgoTree. */
    val opCode: OpCode

    /** The type of the value represented by this node. If the value is an operation it is
      * the type of operation result. */
    def tpe: S

    lazy val bytes = DefaultSerializer.serializeWithSegregation(this)

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

  trait ValueCompanion extends SigmaNodeCompanion {
  }

  object Value {
    type PropositionCode = Byte

    implicit def liftByte (n: Byte) : Value[SByte.type]  = ByteConstant(n)
    implicit def liftShort(n: Short): Value[SShort.type] = ShortConstant(n)
    implicit def liftInt  (n: Int)  : Value[SInt.type]   = IntConstant(n)
    implicit def liftLong (n: Long) : Value[SLong.type]  = LongConstant(n)

    implicit def liftByteArray(arr: Array[Byte]): Value[SByteArray] = ByteArrayConstant(arr)

    implicit def liftBigInt(arr: BigInteger): Value[SBigInt.type] = BigIntConstant(arr)

    implicit def liftGroupElement(g: CryptoConstants.EcPointType): Value[SGroupElement.type] = GroupElementConstant(g)

    implicit def liftSigmaProp(g: SigmaBoolean): Value[SSigmaProp.type] = SigmaPropConstant(g)

    def apply[S <: SType](tS: S)(const: tS.WrappedType): Value[S] = tS.mkConstant(const)

    object Typed {
      def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
    }
    def notSupportedError(v: SValue, opName: String) =
      throw new IllegalArgumentException(s"Method $opName is not supported for node $v")
  }

  trait EvaluatedValue[+S <: SType] extends Value[S] {
    val value: S#WrappedType
    def opType: SFunc = {
      val resType = tpe match {
        case ct : SCollection[_] =>
          SCollection(ct.typeParams.head.ident)
        case ft @ SFunc(tD, tR, _) =>
          ft.getGenericType
        case _ => tpe
      }
      SFunc(Vector(), resType)
    }
  }

  trait Constant[+S <: SType] extends EvaluatedValue[S] {}

  case class ConstantNode[S <: SType](value: S#WrappedType, tpe: S) extends Constant[S] {
    override def companion: ValueCompanion = Constant

    override val opCode: OpCode = ConstantCode
    override def opName: String = s"Const"

    override def equals(obj: scala.Any): Boolean = obj match {
      case c: Constant[_] => Objects.deepEquals(value, c.value) && tpe == c.tpe
      case _ => false
    }

    override def hashCode(): Int = Arrays.deepHashCode(Array(value.asInstanceOf[AnyRef], tpe))

    override def toString: String = tpe.asInstanceOf[SType] match {
      case SGroupElement if value.isInstanceOf[ECPoint] =>
        s"ConstantNode(${showECPoint(value.asInstanceOf[ECPoint])},$tpe)"
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
    def apply[S <: SType](value: S#WrappedType, tpe: S): Constant[S] = ConstantNode(value, tpe)
    def unapply[S <: SType](v: EvaluatedValue[S]): Option[(S#WrappedType, S)] = v match {
      case ConstantNode(value, tpe) => Some((value, tpe))
      case _ => None
    }
  }

  /** Placeholder for a constant in ErgoTree. Zero based index in ErgoTree.constants array. */
  case class ConstantPlaceholder[S <: SType](id: Int, override val tpe: S) extends Value[S] {
    def opType = SFunc(SInt, tpe)
    override val opCode: OpCode = ConstantPlaceholderIndexCode
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
    override val opCode: OpCode = TaggedVariableCode
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }

  object TaggedVariable {
    def apply[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
      TaggedVariableNode(varId, tpe)
  }

  case class UnitConstant() extends EvaluatedValue[SUnit.type] {
    override val opCode = UnitConstantCode
    override def tpe = SUnit
    val value = ()
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
    def apply(value: BigInteger): Constant[SBigInt.type]  = Constant[SBigInt.type](value, SBigInt)
    def apply(value: Long): Constant[SBigInt.type]  = Constant[SBigInt.type](BigInt(value).underlying(), SBigInt)
    def unapply(v: SValue): Option[BigInteger] = v match {
      case Constant(value: BigInteger, SBigInt) => Some(value)
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
    def apply(value: ErgoBox): Constant[SBox.type]  = Constant[SBox.type](value, SBox)
    def unapply(v: SValue): Option[ErgoBox] = v match {
      case Constant(value: ErgoBox, SBox) => Some(value)
      case _ => None
    }
  }

  object GroupElementConstant {
    def apply(value: EcPointType): Constant[SGroupElement.type]  = Constant[SGroupElement.type](value, SGroupElement)
    def unapply(v: SValue): Option[EcPointType] = v match {
      case Constant(value: EcPointType, SGroupElement) => Some(value)
      case _ => None
    }
  }

  object SigmaPropConstant {
    def apply(value: SigmaBoolean): Constant[SSigmaProp.type]  = Constant[SSigmaProp.type](value, SSigmaProp)
    def unapply(v: SValue): Option[SigmaBoolean] = v match {
      case Constant(value: SigmaBoolean, SSigmaProp) => Some(value)
      case _ => None
    }
  }

  object AvlTreeConstant {
    def apply(value: AvlTreeData): Constant[SAvlTree.type]  = Constant[SAvlTree.type](value, SAvlTree)
    def unapply(v: SValue): Option[AvlTreeData] = v match {
      case Constant(value: AvlTreeData, SAvlTree) => Some(value)
      case _ => None
    }
  }

  implicit class AvlTreeConstantOps(val c: AvlTreeConstant) extends AnyVal {
    def createVerifier(proof: SerializedAdProof) =
      new BatchAVLVerifier[Digest32, Blake2b256.type](
        c.value.startingDigest,
        proof,
        c.value.keyLength,
        c.value.valueLengthOpt,
        c.value.maxNumOperations,
        c.value.maxDeletes)
  }

  object ContextConstant {
    def apply(value: ErgoLikeContext): Constant[SContext.type]  = Constant[SContext.type](value, SContext)
    def unapply(v: SValue): Option[ErgoLikeContext] = v match {
      case Constant(value: ErgoLikeContext, SContext) => Some(value)
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
    def apply[T <: SType](value: Array[T#WrappedType], elementType: T): Constant[SCollection[T]] =
      Constant[SCollection[T]](value, SCollection(elementType))
    def unapply[T <: SType](node: Value[SCollection[T]]): Option[(Array[T#WrappedType], T)] = node match {
      case arr: Constant[SCollection[a]] @unchecked if arr.tpe.isCollection =>
        val v = arr.value.asInstanceOf[Array[T#WrappedType]]
        val t = arr.tpe.elemType.asInstanceOf[T]
        Some((v, t))
      case _ => None
    }
  }

  implicit class CollectionConstantOps[T <: SType](val c: CollectionConstant[T]) extends AnyVal {
    def toConcreteCollection: ConcreteCollection[T] = {
      val tElem = c.tpe.elemType
      val items = c.value.map(v => tElem.mkConstant(v.asInstanceOf[tElem.WrappedType]))
      ConcreteCollection(items, tElem)
    }
  }

  val ByteArrayTypeCode = (SCollectionType.CollectionTypeCode + SByte.typeCode).toByte

  object ByteArrayConstant {
    def apply(value: Array[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value, SByte)
    def unapply(node: SValue): Option[Array[Byte]] = node match {
      case coll: CollectionConstant[SByte.type] @unchecked => coll match {
        case CollectionConstant(arr, SByte) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object ShortArrayConstant {
    def apply(value: Array[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value, SShort)
    def unapply(node: SValue): Option[Array[Short]] = node match {
      case coll: CollectionConstant[SShort.type] @unchecked => coll match {
        case CollectionConstant(arr, SShort) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object IntArrayConstant {
    def apply(value: Array[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value, SInt)
    def unapply(node: SValue): Option[Array[Int]] = node match {
      case coll: CollectionConstant[SInt.type] @unchecked => coll match {
        case CollectionConstant(arr, SInt) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object LongArrayConstant {
    def apply(value: Array[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value, SLong)
    def unapply(node: SValue): Option[Array[Long]] = node match {
      case coll: CollectionConstant[SLong.type] @unchecked => coll match {
        case CollectionConstant(arr, SLong) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object BigIntArrayConstant {
    def apply(value: Array[BigInteger]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value, SBigInt)
    def unapply(node: SValue): Option[Array[BigInteger]] = node match {
      case coll: CollectionConstant[SBigInt.type] @unchecked => coll match {
        case CollectionConstant(arr, SBigInt) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  val BoolArrayTypeCode = (SCollectionType.CollectionTypeCode + SBoolean.typeCode).toByte

  object BoolArrayConstant {
    def apply(value: Array[Boolean]): CollectionConstant[SBoolean.type] = CollectionConstant[SBoolean.type](value, SBoolean)
    def unapply(node: SValue): Option[Array[Boolean]] = node match {
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

  case object GroupGenerator extends EvaluatedValue[SGroupElement.type] {

    override val opCode: OpCode = OpCodes.GroupGeneratorCode

    import CryptoConstants.dlogGroup

    override def tpe = SGroupElement

    override val value: CryptoConstants.EcPointType = dlogGroup.generator
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

  val TrueLeaf: Constant[SBoolean.type] = Constant[SBoolean.type](true, SBoolean)
  val FalseLeaf: Constant[SBoolean.type] = Constant[SBoolean.type](false, SBoolean)

  trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
    override def tpe = SBoolean
  }

  /**
    * For sigma statements
    */
  trait SigmaBoolean extends NotReadyValue[SBoolean.type] {
    override def tpe = SBoolean

    def fields: Seq[(String, SType)] = SigmaBoolean.fields
    /** This is not used as operation, but rather as data value of SigmaProp type. */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }

  object SigmaBoolean {
    val PropBytes = "propBytes"
    val IsProven = "isProven"
    val fields = Seq(
      PropBytes -> SByteArray,
      IsProven -> SBoolean
    )
  }

  trait NotReadyValueBox extends NotReadyValue[SBox.type] {
    def tpe = SBox
  }

  case class Tuple(items: IndexedSeq[Value[SType]]) extends EvaluatedValue[STuple] with EvaluatedCollection[SAny.type, STuple] {
    override val opCode: OpCode = TupleCode
    override def elementType = SAny
    lazy val tpe = STuple(items.map(_.tpe))
    lazy val value = {
      val xs = items.cast[EvaluatedValue[SAny.type]].map(_.value)
      xs.toArray(SAny.classTag.asInstanceOf[ClassTag[SAny.WrappedType]])
    }
  }

  object Tuple {
    def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)
  }

  trait OptionValue[T <: SType] extends Value[SOption[T]] {
  }

  case class SomeValue[T <: SType](x: Value[T]) extends OptionValue[T] {
    override val opCode = SomeValueCode
    val tpe = SOption(x.tpe)
    def opType = SFunc(x.tpe, tpe)
  }

  case class NoneValue[T <: SType](elemType: T) extends OptionValue[T] {
    override val opCode = NoneValueCode
    val tpe = SOption(elemType)
    def opType = SFunc(elemType, tpe)
  }

  case class ConcreteCollection[V <: SType](items: IndexedSeq[Value[V]], elementType: V)
    extends EvaluatedCollection[V, SCollection[V]] {
    override val opCode: OpCode =
      if (elementType == SBoolean && items.forall(_.isInstanceOf[Constant[_]]))
        ConcreteCollectionBooleanConstantCode
      else
        ConcreteCollectionCode

    val tpe = SCollection[V](elementType)

    lazy val value = {
      val xs = items.cast[EvaluatedValue[V]].map(_.value)
      xs.toArray(elementType.classTag.asInstanceOf[ClassTag[V#WrappedType]])
    }
  }
  object ConcreteCollection {
    def apply[V <: SType](items: Value[V]*)(implicit tV: V): ConcreteCollection[V] =
      ConcreteCollection(items.toIndexedSeq, tV)

    def apply[V <: SType](items: => Seq[Value[V]])(implicit tV: V): ConcreteCollection[V] =
      ConcreteCollection(items.toIndexedSeq, tV)
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
  }

  implicit class SigmaBooleanOps(val sb: SigmaBoolean) extends AnyVal {
    def isProven: Value[SBoolean.type] = SigmaPropIsProven(SigmaPropConstant(sb))
    def propBytes: Value[SByteArray] = SigmaPropBytes(SigmaPropConstant(sb))
    def toAnyValue: AnyValue = Extensions.toAnyValue(sb)(SType.SigmaBooleanRType)
  }

  implicit class BoolValueOps(val b: BoolValue) extends AnyVal {
    def toSigmaProp: SigmaPropValue = BoolToSigmaProp(b)
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
                    tpeArgs: Seq[STypeIdent],
                    override val rhs: SValue) extends BlockItem {
    require(id >= 0, "id must be >= 0")
    val opCode: OpCode = if (tpeArgs.isEmpty) ValDefCode else FunDefCode
    def tpe: SType = rhs.tpe
    def isValDef: Boolean = tpeArgs.isEmpty
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object ValDef {
    def apply(id: Int, rhs: SValue): ValDef = ValDef(id, Nil, rhs)
  }
  object FunDef {
    def unapply(d: BlockItem): Option[(Int, Seq[STypeIdent], SValue)] = d match {
      case ValDef(id, targs, rhs) if !d.isValDef => Some((id, targs, rhs))
      case _ => None
    }
  }

  /** Special node which represents a reference to ValDef in was introduced as result of CSE. */
  case class ValUse[T <: SType](valId: Int, tpe: T) extends NotReadyValue[T] {
    override val opCode: OpCode = ValUseCode
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }

  /** The order of ValDefs in the block is used to assign ids to ValUse(id) nodes
    * For all i: items(i).id == {number of ValDefs preceded in a graph} with respect to topological order.
    * Specific topological order doesn't really matter, what is important is to preserve semantic linkage
    * between ValUse(id) and ValDef with the corresponding id.
    * This convention allow to valid serializing ids because we always serializing and deserializing
    * in a fixed well defined order.
    */
  case class BlockValue(items: IndexedSeq[BlockItem], result: SValue) extends NotReadyValue[SType] {
    val opCode: OpCode = BlockValueCode
    def tpe: SType = result.tpe
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }

  /**
    * @param args parameters list, where each parameter has an id and a type.
    * @param body expression, which refers function parameters with ValUse.
    */
  case class FuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]) extends NotReadyValue[SFunc] {
    lazy val tpe: SFunc = SFunc(args.map(_._2), body.tpe)
    val opCode: OpCode = FuncValueCode
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = SFunc(Vector(), tpe)
  }
  object FuncValue {
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
    *  Bit 3 - reserved (should be 0)
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
    *  @param constants   If isConstantSegregation == true contains the constants for which there may be
    *                     ConstantPlaceholders in the tree.
    *                     If isConstantSegregation == false this array should be empty and any placeholder in
    *                     the tree will lead to exception.
    *  @param root        if isConstantSegregation == true contains ConstantPlaceholder instead of some Constant nodes.
    *                     Otherwise may not contain placeholders.
    *                     It is possible to have both constants and placeholders in the tree, but for every placeholder
    *                     there should be a constant in `constants` array.
    *  @param proposition When isConstantSegregation == false this is the same as root.
    *                     Otherwise, it is equivalent to `root` where all placeholders are replaced by Constants
    * */
  case class ErgoTree private(
    header: Byte,
    constants: IndexedSeq[Constant[SType]],
    root: SValue,
    proposition: SValue
  ) {
    assert(isConstantSegregation || constants.isEmpty)

    @inline def isConstantSegregation: Boolean = ErgoTree.isConstantSegregation(header)
    @inline def bytes: Array[Byte] = DefaultSerializer.serializeErgoTree(this)
  }

  object ErgoTree {
    /** Current version of ErgoTree serialization format (aka bite-code language version)*/
    val VersionFlag: Byte = 0

    /** Default value of ErgoTree.header byte */
    val DefaultHeader: Byte = (0 | VersionFlag).toByte

    /** Header flag to indicate that constant segregation should be applied. */
    val ConstantSegregationFlag: Byte = 0x10

    /** Default header with constant segregation enabled. */
    val ConstantSegregationHeader = (DefaultHeader | ConstantSegregationFlag).toByte

    @inline def isConstantSegregation(header: Byte): Boolean = (header & ErgoTree.ConstantSegregationFlag) != 0

    def substConstants(root: SValue, constants: IndexedSeq[Constant[SType]]): SValue = {
      val store = new ConstantStore(constants)
      val substRule = strategy[Value[_ <: SType]] {
        case ph: ConstantPlaceholder[_] =>
          Some(store.get(ph.id))
        case _ => None
      }
      everywherebu(substRule)(root).fold(root)(_.asInstanceOf[SValue])
    }

    def apply(header: Byte, constants: IndexedSeq[Constant[SType]], root: SValue) = {
      if ((header & ConstantSegregationFlag) != 0) {
        val prop = substConstants(root, constants)
        new ErgoTree(header, constants, root, prop)
      } else
        new ErgoTree(header, constants, root, root)
    }

    implicit def fromProposition(prop: SValue): ErgoTree = {
      // get ErgoTree with segregated constants
      // todo rewrite with everywherebu?
      val nonSigmaBooleanProp = prop match { case sb: SigmaBoolean => SigmaPropConstant(sb) case _ => prop }
      DefaultSerializer.deserializeErgoTree(DefaultSerializer.serializeWithSegregation(nonSigmaBooleanProp))
    }
  }

}
