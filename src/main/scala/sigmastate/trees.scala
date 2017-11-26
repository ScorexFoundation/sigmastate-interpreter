package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import scapi.sigma.DLogProtocol._
import scapi.sigma.{ProveDiffieHellmanTuple, FirstDiffieHellmanTupleProverMessage, SecondDiffieHellmanTupleProverMessage}
import scapi.sigma.rework.{FirstProverMessage, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256, Blake2b256Unsafe, Digest32}
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utxo.{BoxWithMetadata, Transformer, TransformerInstantiation}
import sigmastate.utxo.CostTable.Cost


sealed trait SigmaStateTree extends Product with SigmaStateProposition {
  def cost: Int
}

trait StateTree extends SigmaStateTree with StateProposition

trait SigmaTree extends SigmaStateTree with SigmaProposition with NotReadyValueBoolean

case class CAND(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override def cost: Int = sigmaTrees.map(_.cost).sum + sigmaTrees.length * Cost.AndPerChild + Cost.AndDeclaration

  override val code: PropositionCode = CAND.Code
  override type M = this.type
}

object CAND {
  val Code: PropositionCode = 101: Byte
}

case class COR(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override def cost: Int = sigmaTrees.map(_.cost).sum + sigmaTrees.length * Cost.OrPerChild + Cost.OrDeclaration

  override val code: PropositionCode = COR.Code
  override type M = this.type
}

object COR {
  val Code: PropositionCode = 101: Byte
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaTree with ProofOfKnowledgeProposition[S] with SigmaProtocolCommonInput[SP]


case class OR(children: Seq[BooleanLeaf]) extends NotReadyValueBoolean {
  override def cost: Int = children.map(_.cost).sum + children.length * Cost.OrPerChild + Cost.OrDeclaration
}


object OR {
  def apply(left: BooleanLeaf, right: BooleanLeaf): OR = apply(Seq(left, right))

  def apply(arg1: BooleanLeaf, arg2: BooleanLeaf, arg3: BooleanLeaf): OR = apply(Seq(arg1, arg2, arg3))
}

case class AND(children: Seq[BooleanLeaf]) extends NotReadyValueBoolean {
  override def cost: Int = children.map(_.cost).sum + children.length * Cost.AndPerChild + Cost.AndDeclaration
}

object AND {
  def apply(left: BooleanLeaf, right: BooleanLeaf): AND = apply(Seq(left, right))
}


trait Value extends StateTree {
  type WrappedValue

  def evaluated: Boolean = ???
}

trait EvaluatedValue[V <: Value] extends Value {
  self: V =>

  val value: V#WrappedValue

  override lazy val evaluated = true
}

trait NotReadyValue[V <: Value] extends Value {
  self: V =>
  override lazy val evaluated = false
}

//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?
sealed trait IntLeaf extends Value {
  override type WrappedValue = Long
}

case class IntLeafConstant(value: Long) extends IntLeaf with EvaluatedValue[IntLeaf] {
  override val cost = 1
}

trait NotReadyValueIntLeaf extends IntLeaf with NotReadyValue[IntLeaf]

case object Height extends NotReadyValueIntLeaf {
  override def cost: Int = Cost.HeightAccess
}

case object UnknownIntLeaf extends NotReadyValueIntLeaf {
  override val cost = 1
}


sealed trait ByteArrayLeaf extends Value {
  override type WrappedValue = Array[Byte]
}

case class ByteArrayLeafConstant(value: Array[Byte]) extends EvaluatedValue[ByteArrayLeaf] with ByteArrayLeaf {

  override def cost: Int = (value.length / 1024.0).ceil.round.toInt * Cost.ByteArrayPerKilobyte

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayLeafConstant => value sameElements ob.value
    case _ => false
  }
}


trait NotReadyValueByteArray extends ByteArrayLeaf with NotReadyValue[ByteArrayLeaf]

case object UnknownByteArrayLeaf extends NotReadyValueByteArray {
  override def cost: Int = 1
}

case object EmptyByteArray extends ByteArrayLeaf with NotReadyValueByteArray {
  override def cost: Int = 1

  override type M = this.type
}

//todo: merge with ByteArrayLeaf?

sealed trait PropLeaf extends Value {
  override type WrappedValue = Array[Byte]
}

case class PropLeafConstant(value: Array[Byte]) extends EvaluatedValue[PropLeaf] with PropLeaf {
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

trait NotReadyValueProp extends PropLeaf with NotReadyValue[PropLeaf]


class AvlTreeData(val startingDigest: ADDigest,
                  val keyLength: Int,
                  val valueLengthOpt: Option[Int],
                  val maxNumOperations: Option[Int] = None,
                  val maxDeletes: Option[Int] = None)

sealed trait AvlTreeLeaf extends Value {
  override type WrappedValue = AvlTreeData
}

case class AvlTreeLeafConstant(value: AvlTreeData) extends AvlTreeLeaf with EvaluatedValue[AvlTreeLeaf] {
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

trait NotReadyValueAvlTree extends AvlTreeLeaf with NotReadyValue[AvlTreeLeaf]



sealed trait GroupElementLeaf extends Value with Proposition {
  override type WrappedValue = GroupElement
}

case class GroupElementConstant(value: GroupElement) extends GroupElementLeaf with EvaluatedValue[GroupElementLeaf] {
  override val cost = 10
}

trait NotReadyValueGroupElement extends GroupElementLeaf with NotReadyValue[GroupElementLeaf]



sealed trait BooleanLeaf extends Value {
  override type WrappedValue = Boolean
}

sealed abstract class BooleanLeafConstant(val value: Boolean) extends BooleanLeaf with EvaluatedValue[BooleanLeaf]

object BooleanLeafConstant {
  def fromBoolean(v: Boolean): BooleanLeafConstant = if (v) TrueLeaf else FalseLeaf
}

case object TrueLeaf extends BooleanLeafConstant(true) {
  override def cost: Int = Cost.ConstantNode
}

case object FalseLeaf extends BooleanLeafConstant(false) {
  override def cost: Int = Cost.ConstantNode
}

trait NotReadyValueBoolean extends BooleanLeaf with NotReadyValue[BooleanLeaf]


sealed trait BoxLeaf extends Value {
  override type WrappedValue = BoxWithMetadata
}

case class BoxLeafConstant(value: BoxWithMetadata) extends BoxLeaf with EvaluatedValue[BoxLeaf] {
  override def cost: Int = 10
}

trait NotReadyValueBoxLeaf extends BoxLeaf with NotReadyValue[BoxLeaf]

case object Self extends NotReadyValueBoxLeaf {
  override def cost: Int = 10

  override type M = this.type
}


trait CollectionLeaf[V <: Value] extends Value {
  override type WrappedValue = IndexedSeq[V]
}

case class ConcreteCollection[V <: Value](value: IndexedSeq[V]) extends CollectionLeaf[V] with EvaluatedValue[CollectionLeaf[V]] {
  val cost = value.size
}

trait LazyCollection[V <: Value] extends CollectionLeaf[V] with NotReadyValue[CollectionLeaf[V]]


case object Inputs extends LazyCollection[BoxLeaf] {
  val cost = 1
}

case object Outputs extends LazyCollection[BoxLeaf] {
  val cost = 1
}


trait CustomVariable[V <: Value] extends NotReadyValue[V] {
  self: V =>
  val id: Int
}

case class CustomByteArray(override val id: Int) extends CustomVariable[ByteArrayLeaf] with NotReadyValueByteArray {
  override def cost: Int = Cost.ByteArrayDeclaration
}


sealed trait CalcBlake2b256 extends Transformer[ByteArrayLeaf, ByteArrayLeaf] with NotReadyValueByteArray {
  override def function(bal: EvaluatedValue[ByteArrayLeaf]): ByteArrayLeaf =
    ByteArrayLeafConstant(Blake2b256(bal.value))

}

case class CalcBlake2b256Inst(input: ByteArrayLeaf)
  extends CalcBlake2b256 with TransformerInstantiation[ByteArrayLeaf, ByteArrayLeaf] {

  override def cost: Int = input.cost + Cost.Blake256bDeclaration
}

case object CalcBlake2b256Fn extends CalcBlake2b256 {
  override def cost: Int = Cost.Blake256bDeclaration

  override def instantiate(input: ByteArrayLeaf) = CalcBlake2b256Inst(input)

  override type M = this.type
}



/**
  * A tree node with left and right descendants
  */
sealed trait Triple[LIV <: Value, RIV <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>
  val left: LIV
  val right: RIV

  override def cost: Int = left.cost + right.cost + Cost.TripleDeclaration

  def withLeft(newLeft: LIV): Triple[LIV, RIV, OV]

  def withRight(newRight: RIV): Triple[LIV, RIV, OV]
}


sealed trait TwoArgumentsOperation[LIV <: Value, RIV <: Value, OV <: Value] extends Triple[LIV, RIV, OV] {
  self: OV =>
}

case class Plus(override val left: IntLeaf,
                override val right: IntLeaf)
  extends TwoArgumentsOperation[IntLeaf, IntLeaf, IntLeaf] with NotReadyValueIntLeaf {

  def withLeft(newLeft: IntLeaf): Plus = copy(left = newLeft)

  def withRight(newRight: IntLeaf): Plus = copy(right = newRight)
}

case class Minus(override val left: IntLeaf,
                 override val right: IntLeaf)
  extends TwoArgumentsOperation[IntLeaf, IntLeaf, IntLeaf] with NotReadyValueIntLeaf {

  def withLeft(newLeft: IntLeaf): Minus = copy(left = newLeft)

  def withRight(newRight: IntLeaf): Minus = copy(right = newRight)
}

case class Xor(override val left: ByteArrayLeaf,
               override val right: ByteArrayLeaf)
  extends TwoArgumentsOperation[ByteArrayLeaf, ByteArrayLeaf, ByteArrayLeaf] with NotReadyValueByteArray {

  def withLeft(newLeft: ByteArrayLeaf): Xor = copy(left = newLeft)

  def withRight(newRight: ByteArrayLeaf): Xor = copy(right = newRight)
}

case class Append(override val left: ByteArrayLeaf,
                  override val right: ByteArrayLeaf)
  extends TwoArgumentsOperation[ByteArrayLeaf, ByteArrayLeaf, ByteArrayLeaf] with NotReadyValueByteArray {

  def withLeft(newLeft: ByteArrayLeaf): Append = copy(left = newLeft)

  def withRight(newRight: ByteArrayLeaf): Append = copy(right = newRight)
}

sealed trait Relation[LIV <: Value, RIV <: Value] extends Triple[LIV, RIV, BooleanLeaf] with NotReadyValueBoolean

case class LT(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf] {

  def withLeft(newLeft: IntLeaf): LT = copy(left = newLeft)

  def withRight(newRight: IntLeaf): LT = copy(right = newRight)
}

case class LE(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf] {
  def withLeft(newLeft: IntLeaf): LE = copy(left = newLeft)

  def withRight(newRight: IntLeaf): LE = copy(right = newRight)
}

case class GT(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf] {
  def withLeft(newLeft: IntLeaf): GT = copy(left = newLeft)

  def withRight(newRight: IntLeaf): GT = copy(right = newRight)
}

case class GE(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf] {
  def withLeft(newLeft: IntLeaf): GE = copy(left = newLeft)

  def withRight(newRight: IntLeaf): GE = copy(right = newRight)
}

case class EQ[V <: Value](override val left: V,
                          override val right: V) extends Relation[V, V] {
  def withLeft(newLeft: V): EQ[V] = copy(left = newLeft)

  def withRight(newRight: V): EQ[V] = copy(right = newRight)
}

case class NEQ[V <: Value](override val left: V, override val right: V) extends Relation[V, V] {
  def withLeft(newLeft: V): NEQ[V] = copy(left = newLeft)

  def withRight(newRight: V): NEQ[V] = copy(right = newRight)
}

/**
  * A tree node with three descendants
  */
sealed trait Quadruple[IV1 <: Value, IV2 <: Value, IV3 <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>

  val first: IV1
  val second: IV2
  val third: IV3

  override def cost: Int = first.cost + second.cost + third.cost + Cost.QuadrupleDeclaration

  def withFirst(newFirst: IV1): Quadruple[IV1, IV2, IV3, OV]

  def withSecond(newSecond: IV2): Quadruple[IV1, IV2, IV3, OV]

  def withThird(newThird: IV2): Quadruple[IV1, IV2, IV3, OV]
}

sealed trait Relation3[IV1 <: Value, IV2 <: Value, IV3 <: Value]
  extends Quadruple[IV1, IV2, IV3, BooleanLeaf] with NotReadyValueBoolean

/**
  *
  * @param first - tree
  * @param second - key
  * @param third - proof
  */
case class IsMember(override val first: AvlTreeLeaf,
                    override val second: ByteArrayLeaf,
                    override val third: ByteArrayLeaf) extends Relation3[AvlTreeLeaf, ByteArrayLeaf, ByteArrayLeaf] {

  def withFirst(newFirst: AvlTreeLeaf): IsMember = this.copy(first = newFirst)

  def withSecond(newSecond: ByteArrayLeaf): IsMember = this.copy(second = newSecond)

  def withThird(newThird: ByteArrayLeaf): IsMember = this.copy(third = newThird)
}


//Proof tree

trait ProofTree extends Product

sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaTree

  val simulated: Boolean

  lazy val real: Boolean = !simulated

  val challengeOpt: Option[Array[Byte]]

  def withChallenge(challenge: Array[Byte]): UnprovenTree

  def withSimulated(newSimulated: Boolean): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree {
  val commitmentOpt: Option[FirstProverMessage[_]]
}

sealed trait UnprovenConjecture extends UnprovenTree {
  val childrenCommitments: Seq[FirstProverMessage[_]]
}

case class CAndUnproven(override val proposition: CAND,
                        override val childrenCommitments: Seq[FirstProverMessage[_]] = Seq(),
                        override val challengeOpt: Option[Array[Byte]] = None,
                        override val simulated: Boolean,
                        children: Seq[ProofTree]) extends UnprovenConjecture {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class COrUnproven(override val proposition: COR,
                       override val childrenCommitments: Seq[FirstProverMessage[_]] = Seq(),
                       override val challengeOpt: Option[Array[Byte]] = None,
                       override val simulated: Boolean,
                       children: Seq[ProofTree]) extends UnprovenConjecture {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class SchnorrUnproven(override val proposition: ProveDlog,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Array[Byte]] = None,
                           override val simulated: Boolean) extends UnprovenLeaf {

  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class DiffieHellmanTupleUnproven(override val proposition: ProveDiffieHellmanTuple,
                                      override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                      randomnessOpt: Option[BigInteger],
                                      override val challengeOpt: Option[Array[Byte]] = None,
                                      override val simulated: Boolean
                                     ) extends UnprovenLeaf {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree[ST <: SigmaTree] extends UncheckedTree with BytesSerializable {
  val proposition: ST
  val propCode: SigmaProposition.PropositionCode
}

trait UncheckedConjecture[ST <: SigmaTree] extends UncheckedSigmaTree[ST] {
  val challengeOpt: Option[Array[Byte]]
  val commitments: Seq[FirstProverMessage[_]]
}


case class SchnorrNode(override val proposition: ProveDlog,
                       firstMessageOpt: Option[FirstDLogProverMessage],
                       challenge: Array[Byte],
                       secondMessage: SecondDLogProverMessage)
  extends UncheckedSigmaTree[ProveDlog] {

  override val propCode: SigmaProposition.PropositionCode = ProveDlog.Code
  override type M = this.type

  override def serializer: Serializer[M] = ???
}

case class DiffieHellmanTupleUncheckedNode(override val proposition: ProveDiffieHellmanTuple,
                                           firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                           challenge: Array[Byte],
                                           secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedSigmaTree[ProveDiffieHellmanTuple] {

  override val propCode: SigmaProposition.PropositionCode = ProveDiffieHellmanTuple.Code
  override type M = DiffieHellmanTupleUncheckedNode

  override def serializer: Serializer[M] = ???
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             leafs: Seq[ProofTree])
  extends UncheckedConjecture[CAND] {

  override val propCode: PropositionCode = CAND.Code
  override type M = CAndUncheckedNode

  override def serializer: Serializer[M] = ???
}


case class COr2UncheckedNode(override val proposition: COR,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             children: Seq[ProofTree]) extends UncheckedConjecture[COR] {

  override val propCode: PropositionCode = COR.Code

  override type M = COr2UncheckedNode

  override def serializer: Serializer[M] = ???
}