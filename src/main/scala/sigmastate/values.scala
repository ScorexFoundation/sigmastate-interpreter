package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import sigmastate.SType.TypeCode
import sigmastate.serialization.SigmaSerializer
import sigmastate.serialization.SigmaSerializer.OpCode
import sigmastate.utxo.SigmaStateBox
import sigmastate.utxo.CostTable.Cost


trait Value[+S <: SType] extends Product {
  val opCode: SigmaSerializer.OpCode = 0: Byte
  def tpe: S
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
  override val opCode: OpCode = SigmaSerializer.TaggedVariableCode

  val id: Byte
  val typeCode: SType.TypeCode
}


//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?
//sealed trait IntLeaf extends Value[SInt.type]

case class IntConstant(value: Long) extends EvaluatedValue[SInt.type] {
  override val opCode = SigmaSerializer.IntConstantCode
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
  override val typeCode: TypeCode = SInt.typeCode
}


//sealed trait BigIntLeaf extends Value[SBigInt.type]

case class BigIntConstant(value: BigInteger) extends EvaluatedValue[SBigInt.type] {
  override val cost = 1
  override def tpe = SBigInt
}

trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
  override lazy val cost: Int = 1
  override def tpe = SBigInt
}

case class TaggedBigInt(override val id: Byte) extends TaggedVariable[SBigInt.type] with NotReadyValueBigInt {
  override val typeCode: TypeCode = SBigInt.typeCode
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
  override val typeCode: TypeCode = SByteArray.typeCode
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
  override val typeCode: TypeCode = SAvlTree.typeCode
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
  override val typeCode: TypeCode = SGroupElement.typeCode
}


sealed abstract class BooleanConstant(val value: Boolean) extends EvaluatedValue[SBoolean.type] {
  override def tpe = SBoolean
}

object BooleanConstant {
  def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf
}

case object TrueLeaf extends BooleanConstant(true) {
  override val opCode: OpCode = SigmaSerializer.TrueCode

  override def cost: Int = Cost.ConstantNode
}


case object FalseLeaf extends BooleanConstant(false) {
  override val opCode: OpCode = SigmaSerializer.FalseCode

  override def cost: Int = Cost.ConstantNode
}


trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
  override def tpe = SBoolean
}

case class TaggedBoolean(override val id: Byte) extends TaggedVariable[SBoolean.type] with NotReadyValueBoolean {
  override def cost = 1
  override val typeCode: TypeCode = SBoolean.typeCode
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
}

case class TaggedBox(override val id: Byte) extends TaggedVariable[SBox.type] with NotReadyValueBox {
  override val typeCode: TypeCode = SBox.typeCode
  override def tpe = SBox
}

case class ConcreteCollection[V <: SType](value: IndexedSeq[Value[V]])(implicit val tV: V)
    extends EvaluatedValue[SCollection[V]] {
  override val opCode = SigmaSerializer.ConcreteCollectionCode
  val cost: Int = value.size
  val tpe = SCollection[V]()(tV)
}


trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]