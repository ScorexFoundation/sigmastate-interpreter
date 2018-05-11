package sigmastate

import java.math.BigInteger
import java.util.Arrays

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.interpreter.{Context, GroupSettings}
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Overloading.Overload1
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.{ErgoBox, ErgoContext}

import scala.collection.immutable
import scala.language.implicitConversions

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

    implicit def liftByteArray(arr: Array[Byte]): Value[SByteArray.type] = ByteArrayConstant(arr)

    implicit def liftBigInt(arr: BigInteger): Value[SBigInt.type] = BigIntConstant(arr)

    implicit def liftGroupElement(g: GroupSettings.EcPointType): Value[SGroupElement.type] = GroupElementConstant(g)

    object Typed {
      def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
    }

  }

  trait EvaluatedValue[S <: SType] extends Value[S] {
    val value: S#WrappedType
    override lazy val evaluated = true
  }

  trait NotReadyValue[S <: SType] extends Value[S] {
    override lazy val evaluated = false
  }

  trait TaggedVariable[S <: SType] extends NotReadyValue[S] {
    override val opCode: OpCode = TaggedVariableCode
    val id: Byte

    override def cost[C <: Context[C]](context: C) = context.extension.cost(id) + 1
  }

  case object UnitConstant extends EvaluatedValue[SUnit.type] {
    override val opCode = UnitConstantCode
    override def cost[C <: Context[C]](context: C) = 1

    override def tpe = SUnit

    val value = ()
  }

  case class IntConstant(value: Long) extends EvaluatedValue[SInt.type] {
    override val opCode: OpCode = IntConstantCode
    override def cost[C <: Context[C]](context: C) = Cost.IntConstantDeclaration

    override def tpe = SInt
  }

  case class ByteConstant(value: Byte) extends EvaluatedValue[SByte.type] {
    override val opCode: OpCode = ByteConstantCode
    override def cost[C <: Context[C]](context: C) = Cost.ByteConstantDeclaration
    override def tpe = SByte
  }

  trait NotReadyValueInt extends NotReadyValue[SInt.type] {
    override def tpe = SInt
  }

  case class TaggedInt(override val id: Byte) extends TaggedVariable[SInt.type] with NotReadyValueInt {
    override def cost[C <: Context[C]](context: C) = 1
  }

  case class BigIntConstant(value: BigInteger) extends EvaluatedValue[SBigInt.type] {

    override val opCode: OpCode = BigIntConstantCode

    override def cost[C <: Context[C]](context: C) = 1

    override def tpe = SBigInt
  }

  trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
    override def tpe = SBigInt
  }

  case class TaggedBigInt(override val id: Byte) extends TaggedVariable[SBigInt.type] with NotReadyValueBigInt {
  }

  case class ByteArrayConstant(value: Array[Byte]) extends EvaluatedValue[SByteArray.type] {

    override def cost[C <: Context[C]](context: C): Long = ((value.length / 1024) + 1) * Cost.ByteArrayPerKilobyte

    override val opCode: OpCode = ByteArrayConstantCode

    override def tpe = SByteArray

    override def equals(obj: scala.Any): Boolean = obj match {
      case c: ByteArrayConstant => Arrays.equals(value, c.value)
      case _ => false
    }

    override def hashCode(): Int = Arrays.hashCode(value)
  }

  trait NotReadyValueByteArray extends NotReadyValue[SByteArray.type] {
    override def tpe = SByteArray
  }

  case class TaggedByteArray(override val id: Byte) extends TaggedVariable[SByteArray.type] with NotReadyValueByteArray

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

  case class TaggedAvlTree(override val id: Byte) extends TaggedVariable[SAvlTree.type] with NotReadyValueAvlTree {
  }


  case class GroupElementConstant(value: GroupSettings.EcPointType) extends EvaluatedValue[SGroupElement.type] {
    override def cost[C <: Context[C]](context: C) = 10

    override val opCode: OpCode = GroupElementConstantCode

    override def tpe = SGroupElement
  }


  case object GroupGenerator extends EvaluatedValue[SGroupElement.type] {

    override val opCode: OpCode = OpCodes.GroupGeneratorCode

    import GroupSettings.dlogGroup

    override def cost[C <: Context[C]](context: C) = 10

    override def tpe = SGroupElement

    override val value: GroupSettings.EcPointType = dlogGroup.generator
  }

  trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
    override def tpe = SGroupElement
  }

  case class TaggedGroupElement(override val id: Byte)
    extends TaggedVariable[SGroupElement.type] with NotReadyValueGroupElement {
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

  case class TaggedBoolean(override val id: Byte) extends TaggedVariable[SBoolean.type] with NotReadyValueBoolean {
    override def cost[C <: Context[C]](context: C) = 1
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

  case class TaggedBox(override val id: Byte) extends TaggedVariable[SBox.type] with NotReadyValueBox {
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

  case class ConcreteCollection[V <: SType](value: IndexedSeq[Value[V]])(implicit val tItem: V)
    extends EvaluatedValue[SCollection[V]] with Rewritable {
    override val opCode: OpCode = ConcreteCollectionCode

    def cost[C <: Context[C]](context: C): Long = Cost.ConcreteCollection + items.map(_.cost(context)).sum

    val tpe = SCollection[V](tItem)

    def items = value // convenience accessor for code readability
    def arity = 1 + value.size

    def deconstruct = immutable.Seq[Any](tItem) ++ items

    def reconstruct(cs: immutable.Seq[Any]) = cs match {
      case Seq(t: SType, vs@_*) => ConcreteCollection[SType](vs.asInstanceOf[Seq[Value[V]]].toIndexedSeq)(t)
      case _ =>
        illegalArgs("ConcreteCollection", "(IndexedSeq, SType)", cs)
    }
  }
  object ConcreteCollection {
    def apply[V <: SType](items: Value[V]*)(implicit tV: V) = new ConcreteCollection(items.toIndexedSeq)
    def isEvaluated[V <: SType](c: Value[SCollection[V]]) =
      c.evaluated && c.asInstanceOf[ConcreteCollection[V]].value.forall(_.evaluated)
  }

  trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]

}