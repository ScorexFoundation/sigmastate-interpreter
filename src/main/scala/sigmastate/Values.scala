package sigmastate

import java.math.BigInteger
import java.util

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.{Context, CryptoConstants}
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Helpers
import sigmastate.utils.Overloading.Overload1
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.ErgoBox
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

    implicit def liftInt(n: Int): Value[SInt.type] = IntConstant(n)

    implicit def liftLong(n: Long): Value[SInt.type] = IntConstant(n)

    implicit def liftByteArray(arr: Array[Byte]): Value[SByteArray] = ByteArrayConstant(arr)

    implicit def liftBigInt(arr: BigInteger): Value[SBigInt.type] = BigIntConstant(arr)

    implicit def liftGroupElement(g: CryptoConstants.EcPointType): Value[SGroupElement.type] = GroupElementConstant(g)

    def apply[S <: SType](tS: S)(const: tS.WrappedType): Value[S] = tS.lift(const)

    object Typed {
      def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
    }

  }

  trait EvaluatedValue[S <: SType] extends Value[S] {
    val value: S#WrappedType
    override lazy val evaluated = true
  }

  case class Constant[S <: SType](val value: S#WrappedType, val tpe: S, val constCost: Long) extends EvaluatedValue[S] {
    override val opCode: OpCode = ConstantCode
    override def cost[C <: Context[C]](context: C) = constCost
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
  type IntConstant = Constant[SInt.type]

  object ByteConstant {
    def apply(value: Byte): Constant[SByte.type] = Constant[SByte.type](value, SByte, Cost.ByteConstantDeclaration)
    def unapply(v: SValue): Option[Byte] = v match {
      case Constant(value: Byte, SByte, _) => Some(value)
      case _ => None
    }
  }
  object IntConstant {
    def apply(value: Long): Constant[SInt.type]  = Constant[SInt.type](value, SInt, Cost.IntConstantDeclaration)
    def unapply(v: SValue): Option[Long] = v match {
      case Constant(value: Long, SInt, _) => Some(value)
      case _ => None
    }
  }

  trait NotReadyValueInt extends NotReadyValue[SInt.type] {
    override def tpe = SInt
  }

  case class BigIntConstant(value: BigInteger) extends EvaluatedValue[SBigInt.type] {

    override val opCode: OpCode = BigIntConstantCode

    override def cost[C <: Context[C]](context: C) = 1

    override def tpe = SBigInt
  }

  trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
    override def tpe = SBigInt
  }

  type TaggedByte = TaggedVariable[SByte.type]
  type TaggedBoolean = TaggedVariable[SBoolean.type]
  type TaggedInt = TaggedVariable[SInt.type]
  type TaggedBigInt = TaggedVariable[SBigInt.type]
  type TaggedBox = TaggedVariable[SBox.type]
  type TaggedGroupElement = TaggedVariable[SGroupElement.type]
  type TaggedAvlTree = TaggedVariable[SAvlTree.type]
  type TaggedByteArray = TaggedVariable[SCollection[SByte.type]]

  def TaggedByte   (id: Byte): TaggedByte = TaggedVariable(id, SByte)
  def TaggedBoolean(id: Byte): TaggedBoolean = TaggedVariable(id, SBoolean)
  def TaggedInt    (id: Byte): TaggedInt = TaggedVariable(id, SInt)
  def TaggedBigInt (id: Byte): TaggedBigInt = TaggedVariable(id, SBigInt)
  def TaggedBox    (id: Byte): TaggedBox = TaggedVariable(id, SBox)
  def TaggedGroupElement(id: Byte): TaggedGroupElement = TaggedVariable(id, SGroupElement)
  def TaggedAvlTree     (id: Byte): TaggedAvlTree = TaggedVariable(id, SAvlTree)
  def TaggedByteArray   (id: Byte): TaggedByteArray = TaggedVariable(id, SByteArray)

  trait EvaluatedCollection[T <: SType] extends EvaluatedValue[SCollection[T]] {
    def elementType: T
  }

  case class CollectionConstant[T <: SType](value: Array[T#WrappedType], elementType: T) extends EvaluatedCollection[T] {

    override def cost[C <: Context[C]](context: C): Long = ((value.length / 1024) + 1) * Cost.ByteArrayPerKilobyte

    override val opCode: OpCode = CollectionConstantCode

    override val tpe = SCollection(elementType)

    override def equals(obj: scala.Any): Boolean = obj match {
      case c: CollectionConstant[_] => util.Objects.deepEquals(value, c.value) && elementType == c.elementType
      case _ => false
    }

    override def hashCode(): Int = 31 * Helpers.deepHashCode(value) + elementType.hashCode()
  }

  object ByteArrayConstant {
    def apply(value: Array[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value, SByte)
    def unapply(node: SValue): Option[Array[Byte]] = node match {
      case arr: CollectionConstant[SByte.type] @unchecked if arr.elementType == SByte => Some(arr.value)
      case _ => None
    }
  }

  object IntArrayConstant {
    def apply(value: Array[Long]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value, SInt)
    def unapply(node: SValue): Option[Array[Long]] = node match {
      case arr: CollectionConstant[SInt.type] @unchecked if arr.elementType == SInt => Some(arr.value)
      case _ => None
    }
  }

  object BoolArrayConstant {
    def apply(value: Array[Boolean]): CollectionConstant[SBoolean.type] = CollectionConstant[SBoolean.type](value, SBoolean)
    def unapply(node: SValue): Option[Array[Boolean]] = node match {
      case arr: CollectionConstant[SBoolean.type] @unchecked if arr.elementType == SBoolean => Some(arr.value)
      case _ => None
    }
  }

  trait NotReadyValueByteArray extends NotReadyValue[SByteArray] {
    override def tpe = SByteArray
  }

  case class AvlTreeConstant(value: AvlTreeData) extends EvaluatedValue[SAvlTree.type] {
    override val opCode: OpCode = OpCodes.AvlTreeConstantCode

    override def cost[C <: Context[C]](context: C) = Cost.AvlTreeConstant

    override def tpe = SAvlTree

    def createVerifier(proof: SerializedAdProof) =
      new BatchAVLVerifier[Digest32, Blake2b256.type](
        value.startingDigest,
        proof,
        value.keyLength,
        value.valueLengthOpt,
        value.maxNumOperations,
        value.maxDeletes)
  }

  trait NotReadyValueAvlTree extends NotReadyValue[SAvlTree.type] {
    override def tpe = SAvlTree
  }

  case class GroupElementConstant(value: CryptoConstants.EcPointType) extends EvaluatedValue[SGroupElement.type] {
    override def cost[C <: Context[C]](context: C) = 10

    override val opCode: OpCode = GroupElementConstantCode

    override def tpe = SGroupElement
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

  case class BoxConstant(value: ErgoBox) extends EvaluatedValue[SBox.type] {
    override val opCode: OpCode = OpCodes.BoxConstantCode

    override def cost[C <: Context[C]](context: C): Long = value.cost

    override def tpe = SBox
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

  case class ConcreteCollection[V <: SType](items: IndexedSeq[Value[V]])(implicit val elementType: V)
    extends EvaluatedCollection[V] with Rewritable {
    override val opCode: OpCode = ConcreteCollectionCode

    def cost[C <: Context[C]](context: C): Long = Cost.ConcreteCollection + items.map(_.cost(context)).sum

    val tpe = SCollection[V](elementType)

    lazy val value = {
      val xs = items.cast[EvaluatedValue[V]].map(_.value)
      xs.toArray(elementType.classTag.asInstanceOf[ClassTag[V#WrappedType]])
    }

    def arity = 1 + items.size

    def deconstruct = immutable.Seq[Any](elementType) ++ items

    def reconstruct(cs: immutable.Seq[Any]) = cs match {
      case Seq(t: SType, vs@_*) => ConcreteCollection[SType](vs.asInstanceOf[Seq[Value[V]]].toIndexedSeq)(t)
      case _ =>
        illegalArgs("ConcreteCollection", "(IndexedSeq, SType)", cs)
    }
  }
  object ConcreteCollection {
    def apply[V <: SType](items: Value[V]*)(implicit tV: V) = new ConcreteCollection(items.toIndexedSeq)
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
  }
}