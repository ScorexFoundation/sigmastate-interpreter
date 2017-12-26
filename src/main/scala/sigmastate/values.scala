package sigmastate


import edu.biu.scapi.primitives.dlog.GroupElement
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import sigmastate.utxo.BoxWithMetadata
import sigmastate.utxo.CostTable.Cost



trait Value[S <: SType] extends StateTree {
  def evaluated: Boolean
}

trait EvaluatedValue[S <: SType] extends Value[S] {
  val value: S#WrappedType
  override lazy val evaluated = true
}

trait NotReadyValue[S <: SType] extends Value[S] {
  override lazy val evaluated = false
}

trait TaggedVariable[S <: SType] extends NotReadyValue[S] {
  val id: Byte
}


//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?
sealed trait IntLeaf extends Value[SInt.type]

case class IntLeafConstant(value: Long) extends EvaluatedValue[SInt.type] {
  override val cost = 1
}

trait NotReadyValueIntLeaf extends NotReadyValue[SInt.type]{
  override lazy val cost: Int = 1
}

case object Height extends NotReadyValueIntLeaf {
  override lazy val cost: Int = Cost.HeightAccess
}

case object UnknownIntLeaf extends NotReadyValueIntLeaf

case class TaggedInt(override val id: Byte) extends TaggedVariable[SInt.type] with NotReadyValueIntLeaf


case class ByteArrayLeafConstant(value: Array[Byte]) extends EvaluatedValue[SByteArray.type] {

  override def cost: Int = (value.length / 1024.0).ceil.round.toInt * Cost.ByteArrayPerKilobyte

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayLeafConstant => value sameElements ob.value
    case _ => false
  }
}

object EmptyByteArray extends ByteArrayLeafConstant(Array.emptyByteArray)

trait NotReadyValueByteArray extends NotReadyValue[SByteArray.type]{
  override lazy val cost: Int = Cost.ByteArrayDeclaration
}

case object UnknownByteArrayLeaf extends NotReadyValueByteArray


case class TaggedByteArray(override val id: Byte) extends TaggedVariable[SByteArray.type] with NotReadyValueByteArray


//todo: merge with SByteArray?

case class PropLeafConstant(value: Array[Byte]) extends EvaluatedValue[SProp.type] {
  override def cost: Int = value.length + Cost.PropLeafDeclaration

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: PropLeafConstant => value sameElements ob.value
    case _ => false
  }
}

object PropLeafConstant {
  def apply(value: BoxWithMetadata): PropLeafConstant = new PropLeafConstant(value.box.propositionBytes)

  def apply(value: SigmaStateTree): PropLeafConstant = new PropLeafConstant(value.toString.getBytes)
}

trait NotReadyValueProp extends NotReadyValue[SProp.type] {
  override def cost: Int = Cost.PropLeafDeclaration
}

case class TaggedPropLeaf(override val id: Byte) extends TaggedVariable[SProp.type] with NotReadyValueProp


case class AvlTreeConstant(value: AvlTreeData) extends EvaluatedValue[SAvlTree.type] {
  override val cost = 50

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
}

case class TaggedAvlTree(override val id: Byte) extends TaggedVariable[SAvlTree.type] with NotReadyValueAvlTree



case class GroupElementConstant(value: GroupElement) extends EvaluatedValue[SGroupElement.type] {
  override val cost = 10
}

trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
  override val cost = 10
}

case class TaggedGroupElement(override val id: Byte)
  extends TaggedVariable[SGroupElement.type] with NotReadyValueGroupElement



sealed abstract class BooleanConstant(val value: Boolean) extends EvaluatedValue[SBoolean.type]

object BooleanConstant {
  def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf
}

case object TrueLeaf extends BooleanConstant(true) {
  override def cost: Int = Cost.ConstantNode
}

case object FalseLeaf extends BooleanConstant(false) {
  override def cost: Int = Cost.ConstantNode
}

trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
  override def cost: Int = 1
}

case class TaggedBoolean(override val id: Byte) extends TaggedVariable[SBoolean.type] with NotReadyValueBoolean

/**
  * For sigma statements
  */
trait FakeBoolean extends NotReadyValue[SBoolean.type]{
  override lazy val evaluated = true
}


case class BoxLeafConstant(value: BoxWithMetadata) extends EvaluatedValue[SBox.type] {
  override def cost: Int = 10
}

trait NotReadyValueBox extends NotReadyValue[SBox.type] {
  override def cost: Int = 10
}

case class TaggedBox(override val id: Byte) extends TaggedVariable[SBox.type] with NotReadyValueBox


case object Self extends NotReadyValueBox {
  override def cost: Int = 10

  override type M = this.type
}


case class ConcreteCollection[V <: SType](value: IndexedSeq[Value[V]]) extends EvaluatedValue[SCollection[V]] {
  val cost = value.size
}

trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]


case object Inputs extends LazyCollection[SBox.type] {
  val cost = 1
}

case object Outputs extends LazyCollection[SBox.type] {
  val cost = 1
}
