package sigmastate

import java.math.BigInteger
import java.util.{Objects, Arrays}

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
import org.ergoplatform.ErgoBox
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{Context, CryptoConstants}
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Overloading.Overload1
import sigmastate.utxo.CostTable.Cost
import sigmastate.utils.Extensions._

import scala.collection.immutable
import scala.language.implicitConversions
import scala.reflect.ClassTag


object Values {

  type SigmaTree = Tree[SigmaNode, SValue]

  type SValue = Value[SType]
  type Idn = String

  trait Value[+S <: SType] extends SigmaNode {
    val opCode: OpCode

    def tpe: S

    def typeCode: SType.TypeCode = tpe.typeCode

    def cost[C <: Context[C]](context: C): Long

    /** Returns true if this value represent some constant or sigma statement, false otherwise */
    def evaluated: Boolean

    lazy val bytes = ValueSerializer.serialize(this)
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

    def apply[S <: SType](tS: S)(const: tS.WrappedType): Value[S] = tS.mkConstant(const)

    object Typed {
      def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
    }

  }

  trait EvaluatedValue[S <: SType] extends Value[S] {
    val value: S#WrappedType
    override lazy val evaluated = true
  }

  case class Constant[S <: SType](value: S#WrappedType, tpe: S) extends EvaluatedValue[S] {
    override val opCode: OpCode = (ConstantCode + tpe.typeCode).toByte
    override def cost[C <: Context[C]](context: C) = tpe.dataCost(value)

    override def equals(obj: scala.Any): Boolean = obj match {
      case c: Constant[_] => Objects.deepEquals(value, c.value) && tpe == c.tpe
      case _ => false
    }

    override def hashCode(): Int = Arrays.deepHashCode(Array(value.asInstanceOf[AnyRef], tpe))
  }

  trait NotReadyValue[S <: SType] extends Value[S] {
    override lazy val evaluated = false
  }

  /** Base class for references to context variables. */
  trait ContextVariable[S <: SType] extends NotReadyValue[S] {
  }

  /** Reference a context variable by id. */
  case class TaggedVariable[T <: SType](varId: Byte, override val tpe: T) extends ContextVariable[T] {
    override val opCode: OpCode = TaggedVariableCode
    override def cost[C <: Context[C]](context: C) = context.extension.cost(varId) + 1
  }

  case object UnitConstant extends EvaluatedValue[SUnit.type] {
    override val opCode = UnitConstantCode
    override def cost[C <: Context[C]](context: C) = 1

    override def tpe = SUnit

    val value = ()
  }

  type ByteConstant = Constant[SByte.type]
  type ShortConstant = Constant[SShort.type]
  type IntConstant = Constant[SInt.type]
  type LongConstant = Constant[SLong.type]
  type BigIntConstant = Constant[SBigInt.type]
  type BoxConstant = Constant[SBox.type]
  type GroupElementConstant = Constant[SGroupElement.type]
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
  type TaggedAvlTree = TaggedVariable[SAvlTree.type]
  type TaggedByteArray = TaggedVariable[SCollection[SByte.type]]

  def TaggedBoolean(id: Byte): TaggedBoolean = TaggedVariable(id, SBoolean)
  def TaggedByte   (id: Byte): TaggedByte = TaggedVariable(id, SByte)
  def TaggedShort  (id: Byte): TaggedShort = TaggedVariable(id, SShort)
  def TaggedInt    (id: Byte): TaggedInt = TaggedVariable(id, SInt)
  def TaggedLong   (id: Byte): TaggedLong = TaggedVariable(id, SLong)
  def TaggedBigInt (id: Byte): TaggedBigInt = TaggedVariable(id, SBigInt)
  def TaggedBox    (id: Byte): TaggedBox = TaggedVariable(id, SBox)
  def TaggedGroupElement(id: Byte): TaggedGroupElement = TaggedVariable(id, SGroupElement)
  def TaggedAvlTree     (id: Byte): TaggedAvlTree = TaggedVariable(id, SAvlTree)
  def TaggedByteArray   (id: Byte): TaggedByteArray = TaggedVariable(id, SByteArray)

  trait EvaluatedCollection[T <: SType] extends EvaluatedValue[SCollection[T]] {
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
      implicit val tElem = c.tpe.elemType
      val items = c.value.map(v => tElem.mkConstant(v.asInstanceOf[tElem.WrappedType]))
      ConcreteCollection(items)
    }
  }

  val ByteArrayTypeCode = (SCollection.CollectionTypeCode + SByte.typeCode).toByte

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

  val BoolArrayTypeCode = (SCollection.CollectionTypeCode + SBoolean.typeCode).toByte

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

  sealed abstract class BooleanConstant(val value: Boolean) extends EvaluatedValue[SBoolean.type] {
    override def tpe = SBoolean
  }

  object BooleanConstant {
    def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf
  }

  case object TrueLeaf extends BooleanConstant(true) {
    override val opCode: OpCode = TrueCode

    override def cost[C <: Context[C]](context: C): Long = Cost.ConstantNode
  }

  case object FalseLeaf extends BooleanConstant(false) {
    override val opCode: OpCode = FalseCode

    override def cost[C <: Context[C]](context: C): Long = Cost.ConstantNode
  }

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

  case class Tuple(items: IndexedSeq[Value[SType]]) extends EvaluatedValue[STuple] {
    override val opCode: OpCode = TupleCode
    val cost: Int = value.size
    val tpe = STuple(items.map(_.tpe))
    lazy val value = items

    override def cost[C <: Context[C]](context: C) = Cost.ConcreteCollection + items.map(_.cost(context)).sum
  }

  object Tuple {
    def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)

    def apply(items: Seq[Value[SType]])(implicit o: Overload1): Tuple = Tuple(items.toIndexedSeq)
  }

  trait OptionValue[T <: SType] extends EvaluatedValue[SOption[T]] {
  }

  case class SomeValue[T <: SType](x: Value[T]) extends OptionValue[T] {
    override val opCode = SomeValueCode

    def cost[C <: Context[C]](context: C): Long = x.cost(context) + 1

    val tpe = SOption(x.tpe)
    lazy val value = Some(x)
  }

  case class NoneValue[T <: SType](elemType: T) extends OptionValue[T] {
    override val opCode = NoneValueCode

    def cost[C <: Context[C]](context: C): Long = 1

    val tpe = SOption(elemType)
    lazy val value = None
  }

  case class ConcreteCollection[V <: SType](items: IndexedSeq[Value[V]], elementType: V)
    extends EvaluatedCollection[V] {
    override val opCode: OpCode =
      if (elementType == SBoolean
        && items.forall(i => i.isInstanceOf[Constant[_]] || i.isInstanceOf[BooleanConstant]))
        ConcreteCollectionBooleanConstantCode
      else
        ConcreteCollectionCode

    def cost[C <: Context[C]](context: C): Long = Cost.ConcreteCollection + items.map(_.cost(context)).sum

    val tpe = SCollection[V](elementType)

    lazy val value = {
      val xs = items.cast[EvaluatedValue[V]].map(_.value)
      xs.toArray(elementType.classTag.asInstanceOf[ClassTag[V#WrappedType]])
    }
  }
  object ConcreteCollection {
    def apply[V <: SType](items: Value[V]*)(implicit tV: V) =
      new ConcreteCollection(items.toIndexedSeq, tV)

    def apply[V <: SType](items: => Seq[Value[V]])(implicit tV: V) =
      new ConcreteCollection(items.toIndexedSeq, tV)
  }

  trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]

  implicit class CollectionOps[T <: SType](coll: Value[SCollection[T]]) {
    def length: Int = matchCase(_.items.length, _.value.length)
    def items = matchCase(_.items, _ => sys.error(s"Cannot get 'items' property of node $coll"))
    def isEvaluated =
      coll.evaluated && matchCase(_.items.forall(_.evaluated), _ => true)
    def matchCase[R](whenConcrete: ConcreteCollection[T] => R, whenConstant: CollectionConstant[T] => R): R = coll match {
      case cc: ConcreteCollection[T]@unchecked => whenConcrete(cc)
      case const: CollectionConstant[T]@unchecked => whenConstant(const)
      case _ => sys.error(s"Unexpected node $coll")
    }
    def toConcreteCollection: ConcreteCollection[T] =
      matchCase(cc => cc, _.toConcreteCollection)
  }
}
