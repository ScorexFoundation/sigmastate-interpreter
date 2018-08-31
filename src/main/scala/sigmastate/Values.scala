package sigmastate

import java.math.BigInteger
import java.util.{Arrays, Objects}

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.ergoplatform.ErgoBox
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{Context, CryptoConstants}
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.serialization.OpCodes._
import sigmastate.utxo.CostTable.Cost
import sigmastate.utils.Extensions._
import sigmastate.lang.Terms._
import sigmastate.utxo.{SigmaPropIsValid, SigmaPropBytes}


import scala.language.implicitConversions
import scala.reflect.ClassTag
import sigmastate.lang.DefaultSigmaBuilder._


object Values {

  type SigmaTree = Tree[SigmaNode, SValue]

  type SValue = Value[SType]
  type Idn = String

  trait Value[+S <: SType] extends SigmaNode {
    def companion: ValueCompanion =
      sys.error(s"Companion object is not defined for AST node ${this.getClass}")

    val opCode: OpCode

    def tpe: S

    def typeCode: SType.TypeCode = tpe.typeCode

    def cost[C <: Context[C]](context: C): Long

    /** Returns true if this value represent some constant or sigma statement, false otherwise */
    def evaluated: Boolean

    lazy val bytes = ValueSerializer.serialize(this)
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

  }

  trait EvaluatedValue[S <: SType] extends Value[S] {
    val value: S#WrappedType
    override lazy val evaluated = true
  }

  trait Constant[S <: SType] extends EvaluatedValue[S] {}

  case class ConstantNode[S <: SType](value: S#WrappedType, tpe: S) extends Constant[S] {
    override def companion: ValueCompanion = Constant

    override val opCode: OpCode = ConstantCode
    override def cost[C <: Context[C]](context: C) = tpe.dataSize(value)

    override def equals(obj: scala.Any): Boolean = obj match {
      case c: Constant[_] => Objects.deepEquals(value, c.value) && tpe == c.tpe
      case _ => false
    }

    override def hashCode(): Int = Arrays.deepHashCode(Array(value.asInstanceOf[AnyRef], tpe))
  }

  object Constant extends ValueCompanion {
    def apply[S <: SType](value: S#WrappedType, tpe: S): Constant[S] = ConstantNode(value, tpe)
    def unapply[S <: SType](v: EvaluatedValue[S]): Option[(S#WrappedType, S)] = v match {
      case ConstantNode(value, tpe) => Some((value, tpe))
      case _ => None
    }
  }

  trait NotReadyValue[S <: SType] extends Value[S] {
    override lazy val evaluated = false
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
    override def cost[C <: Context[C]](context: C): Long = context.extension.cost(varId) + 1
  }

  object TaggedVariable {
    def apply[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
      TaggedVariableNode(varId, tpe)
  }

  case object UnitConstant extends EvaluatedValue[SUnit.type] {
    override val opCode = UnitConstantCode
    override def cost[C <: Context[C]](context: C) = 1

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

  implicit class AvlTreeConstantOps(c: AvlTreeConstant) {
    def createVerifier(proof: SerializedAdProof) =
      new BatchAVLVerifier[Digest32, Blake2b256.type](
        c.value.startingDigest,
        proof,
        c.value.keyLength,
        c.value.valueLengthOpt,
        c.value.maxNumOperations,
        c.value.maxDeletes)
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

  implicit class CollectionConstantOps[T <: SType](c: CollectionConstant[T]) {
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

    override def cost[C <: Context[C]](context: C) = 10

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
    override lazy val evaluated = true

    override def tpe = SBoolean

    def fields: Seq[(String, SType)] = SigmaBoolean.fields
  }

  object SigmaBoolean {
    val PropBytes = "propBytes"
    val IsValid = "isValid"
    val fields = Seq(
      PropBytes -> SByteArray,
      IsValid -> SBoolean
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
    override def cost[C <: Context[C]](context: C) = Cost.TupleDeclaration + items.map(_.cost(context)).sum
  }

  object Tuple {
    def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)
  }

  trait OptionValue[T <: SType] extends Value[SOption[T]] {
    def evaluated: Boolean = false
  }

  case class SomeValue[T <: SType](x: Value[T]) extends OptionValue[T] {
    override val opCode = SomeValueCode
    def cost[C <: Context[C]](context: C): Long = x.cost(context) + 1
    val tpe = SOption(x.tpe)
  }

  case class NoneValue[T <: SType](elemType: T) extends OptionValue[T] {
    override val opCode = NoneValueCode
    def cost[C <: Context[C]](context: C): Long = 1
    val tpe = SOption(elemType)
  }

  case class ConcreteCollection[V <: SType](items: IndexedSeq[Value[V]], elementType: V)
    extends EvaluatedCollection[V, SCollection[V]] {
    override val opCode: OpCode =
      if (elementType == SBoolean && items.forall(_.isInstanceOf[Constant[_]]))
        ConcreteCollectionBooleanConstantCode
      else
        ConcreteCollectionCode

    def cost[C <: Context[C]](context: C): Long = Cost.ConcreteCollectionDeclaration + items.map(_.cost(context)).sum

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

  implicit class CollectionOps[T <: SType](coll: Value[SCollection[T]]) {
    def length: Int = matchCase(_.items.length, _.value.length, _.items.length)
    def items = matchCase(_.items, _ => sys.error(s"Cannot get 'items' property of node $coll"), _.items)
    def isEvaluatedCollection =
      coll.evaluated && matchCase(_.items.forall(_.evaluated), _ => true, _.items.forall(_.evaluated))
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

  implicit class SigmaPropValueOps(p: Value[SSigmaProp.type]) {
    def isValid: Value[SBoolean.type] = SigmaPropIsValid(p)
    def propBytes: Value[SByteArray] = SigmaPropBytes(p)
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
  case class ValDef(id: Int, tpeArgs: Seq[STypeIdent], rhs: SValue) extends BlockItem {
    val opCode: OpCode = if (tpeArgs.isEmpty) ValDefCode else FunDefCode
    def tpe: SType = rhs.tpe
    def cost[C <: Context[C]](ctx: C): Long = rhs.cost(ctx)
    def isValDef: Boolean = tpeArgs.isEmpty
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
    override def cost[C <: Context[C]](context: C): Long = 1
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
    def cost[C <: Context[C]](ctx: C): Long = items.map(_.cost(ctx)).sum + result.cost(ctx)
  }

  case class FuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]) extends NotReadyValue[SFunc] {
    lazy val tpe: SFunc = SFunc(args.map(_._2), body.tpe)
    val opCode: OpCode = FuncValueCode
    def cost[C <: Context[C]](context: C): Long = 1
  }
  object FuncValue {
    def apply(argId: Int, tArg: SType, body: SValue): FuncValue =
      FuncValue(IndexedSeq((argId,tArg)), body)
  }
}
