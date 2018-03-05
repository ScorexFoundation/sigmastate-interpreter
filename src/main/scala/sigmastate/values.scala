package sigmastate

import java.math.BigInteger
import edu.biu.scapi.primitives.dlog.GroupElement
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bitbucket.inkytonik.kiama.rewriting.Rewritable
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Digest32, Blake2b256Unsafe}
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.ValueSerializer.OpCode
import sigmastate.utxo.SigmaStateBox
import sigmastate.utxo.CostTable.Cost
import scala.collection.immutable

trait Value[+S <: SType] extends Product {
  val opCode: ValueSerializer.OpCode = 0: Byte
  def tpe: S
  def typeCode: SType.TypeCode = tpe.typeCode
  def cost: Int

  /** Returns true if this value represent some constant or sigma statement, false otherwise */
  def evaluated: Boolean

  //todo: remove after serialization, replace with just .bytes
  lazy val propBytes = this.toString.getBytes
}

object Value {
  type PropositionCode = Byte
}

trait EvaluatedValue[S <: SType] extends Value[S] {
  val value: S#WrappedType
  override lazy val evaluated = true
}

trait NotReadyValue[S <: SType] extends Value[S] {
  override lazy val evaluated = false
}

trait TaggedVariable[S <: SType] extends NotReadyValue[S] {
  override val opCode: OpCode = ValueSerializer.TaggedVariableCode
  val id: Byte
}

//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?

case object UnitConstant extends EvaluatedValue[SUnit.type] {
  override val opCode = ValueSerializer.UnitConstantCode
  override val cost = 1
  override def tpe = SUnit
  val value = ()
}

case class IntConstant(value: Long) extends EvaluatedValue[SInt.type] {
  override val opCode = ValueSerializer.IntConstantCode
  override val cost = 1
  override def tpe = SInt
}

trait NotReadyValueInt extends NotReadyValue[SInt.type] {
  override def tpe = SInt
}

case object UnknownInt extends NotReadyValueInt {
  override val cost = 1
}

case class TaggedInt(override val id: Byte) extends TaggedVariable[SInt.type] with NotReadyValueInt {
  override val cost = 1
}

case class BigIntConstant(value: BigInteger) extends EvaluatedValue[SBigInt.type] {
  override val cost = 1
  override def tpe = SBigInt
}

trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
  override lazy val cost: Int = 1
  override def tpe = SBigInt
}

case class TaggedBigInt(override val id: Byte) extends TaggedVariable[SBigInt.type] with NotReadyValueBigInt {
}

case class ByteArrayConstant(value: Array[Byte]) extends EvaluatedValue[SByteArray.type] {
  override def cost: Int = ((value.length / 1024) + 1) * Cost.ByteArrayPerKilobyte
  override def tpe = SByteArray

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayConstant => value sameElements ob.value
    case _ => false
  }
}

object EmptyByteArray extends ByteArrayConstant(Array.emptyByteArray)

trait NotReadyValueByteArray extends NotReadyValue[SByteArray.type] {
  override lazy val cost: Int = Cost.ByteArrayDeclaration
  override def tpe = SByteArray
}

case object UnknownByteArray extends NotReadyValueByteArray


case class TaggedByteArray(override val id: Byte) extends TaggedVariable[SByteArray.type] with NotReadyValueByteArray {
}


case class AvlTreeConstant(value: AvlTreeData) extends EvaluatedValue[SAvlTree.type] {
  override val cost = 50
  override def tpe = SAvlTree
  def createVerifier(proof: SerializedAdProof) =
    new BatchAVLVerifier[Digest32, Blake2b256Unsafe](
      value.startingDigest,
      proof,
      value.keyLength,
      value.valueLengthOpt,
      value.maxNumOperations,
      value.maxDeletes)
}


trait NotReadyValueAvlTree extends NotReadyValue[SAvlTree.type] {
  override val cost = 50
  override def tpe = SAvlTree
}

case class TaggedAvlTree(override val id: Byte) extends TaggedVariable[SAvlTree.type] with NotReadyValueAvlTree {
}


case class GroupElementConstant(value: GroupElement) extends EvaluatedValue[SGroupElement.type] {
  override val cost = 10
  override def tpe = SGroupElement
}


case object GroupGenerator extends EvaluatedValue[SGroupElement.type] {
  override val cost = 10
  override def tpe = SGroupElement
  override val value: GroupElement = new BcDlogECFp().getGenerator
}


trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
  override val cost = 10
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
  override val opCode: OpCode = ValueSerializer.TrueCode

  override def cost: Int = Cost.ConstantNode
}


case object FalseLeaf extends BooleanConstant(false) {
  override val opCode: OpCode = ValueSerializer.FalseCode

  override def cost: Int = Cost.ConstantNode
}


trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
  override def tpe = SBoolean
}

case class TaggedBoolean(override val id: Byte) extends TaggedVariable[SBoolean.type] with NotReadyValueBoolean {
  override def cost = 1
}

/**
  * For sigma statements
  */
trait SigmaBoolean extends NotReadyValue[SBoolean.type] {
  override lazy val evaluated = true
  override def tpe = SBoolean
}

case class BoxConstant(value: SigmaStateBox) extends EvaluatedValue[SBox.type] {
  override def cost: Int = 10
  override def tpe = SBox
}

trait NotReadyValueBox extends NotReadyValue[SBox.type] {
  override def cost: Int = 10
  def tpe = SBox
}

case class TaggedBox(override val id: Byte) extends TaggedVariable[SBox.type] with NotReadyValueBox {
}

case class Tuple(items: IndexedSeq[Value[SType]]) extends EvaluatedValue[STuple] {
  override val opCode = ValueSerializer.TupleCode
  val cost: Int = value.size
  val tpe = STuple(items.map(_.tpe))
  lazy val value = items
}
object Tuple {
  def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)
}

case class ConcreteCollection[V <: SType](value: IndexedSeq[Value[V]])(implicit val tItem: V)
    extends EvaluatedValue[SCollection[V]] with Rewritable {
  override val opCode = ValueSerializer.ConcreteCollectionCode
  val cost: Int = value.size
  val tpe = SCollection[V]()(tItem)
  def items = value // convenience accessor for code readability
  def arity = 2
  def deconstruct = immutable.Seq[Any](value, tItem)
  def reconstruct(cs: immutable.Seq[Any]) = cs match {
    case Seq(v: IndexedSeq[Value[V]] @unchecked, t: SType) => ConcreteCollection[SType](v)(t)
    case _ =>
      illegalArgs("ConcreteCollection", "(IndexedSeq, SType)", cs)
  }
}


trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]