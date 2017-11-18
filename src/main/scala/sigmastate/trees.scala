package sigmastate

import java.math.BigInteger

import scapi.sigma.DLogProtocol._
import scapi.sigma.{DiffieHellmanTupleNode, FirstDiffieHellmanTupleProverMessage, SecondDiffieHellmanTupleProverMessage}
import scapi.sigma.rework.{FirstProverMessage, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utxo.BoxLeaf
import sigmastate.utxo.CostTable.Cost


sealed trait SigmaStateTree extends Product with SigmaStateProposition {
  def cost: Int
}

trait StateTree extends SigmaStateTree with StateProposition

trait SigmaTree extends SigmaStateTree with SigmaProposition

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


case class OR(children: Seq[SigmaStateTree]) extends SigmaStateTree {
  override def cost: Int = children.map(_.cost).sum + children.length * Cost.OrPerChild + Cost.OrDeclaration
}


object OR {
  def apply(left: SigmaStateTree, right: SigmaStateTree): OR = apply(Seq(left, right))

  def apply(arg1: SigmaStateTree, arg2: SigmaStateTree, arg3: SigmaStateTree): OR = apply(Seq(arg1, arg2, arg3))
}

case class AND(children: Seq[SigmaStateTree]) extends SigmaStateTree {
  override def cost: Int = children.map(_.cost).sum + children.length * Cost.AndPerChild + Cost.AndDeclaration
}

object AND {
  def apply(left: SigmaStateTree, right: SigmaStateTree): AND = apply(Seq(left, right))
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
  override def cost: Int = 1
}

trait NotReadyValueIntLeaf extends IntLeaf with NotReadyValue[IntLeaf]

case object Height extends NotReadyValueIntLeaf {
  override def cost: Int = Cost.HeightAccess
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


sealed trait PropLeaf extends Value {
  override type WrappedValue = SigmaStateTree
}

case class PropLeafConstant(value: SigmaStateTree) extends EvaluatedValue[PropLeaf] with PropLeaf {
  override def cost: Int = value.cost + Cost.PropLeafDeclaration
}

trait NotReadyValueProp extends PropLeaf with NotReadyValue[PropLeaf]


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


trait CollectionLeaf[V <: Value] extends Value {
  override type WrappedValue = Seq[V]
}

case class ConcreteCollection[V <: Value](value: Seq[V]) extends CollectionLeaf[V] with EvaluatedValue[CollectionLeaf[V]] {
  val cost = value.size
  lazy val isLazy = value.exists(_.isInstanceOf[NotReadyValue[_]] == true)
  override lazy val evaluated = !isLazy
}

trait LazyCollection[V <: Value] extends CollectionLeaf[V] with NotReadyValue[LazyCollection[V]]


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


trait OneArgumentOperation extends StateTree {
  val operand: SigmaStateTree
}

case class CalcBlake2b256(operand: SigmaStateTree) extends OneArgumentOperation {
  override def cost: Int = operand.cost + Cost.Blake256bDeclaration
}

/**
  * A tree node with left and right descendants
  */
sealed trait Triple extends StateTree {
  val left: SigmaStateTree
  val right: SigmaStateTree

  override def cost: Int = left.cost + right.cost + Cost.TripleDeclaration

  //todo: define via F-Bounded polymorphism?
  def withLeft(newLeft: SigmaStateTree): Triple

  def withRight(newRight: SigmaStateTree): Triple
}


sealed trait TwoArgumentsOperation extends Triple

case class Plus(override val left: SigmaStateTree,
                override val right: SigmaStateTree) extends TwoArgumentsOperation {

  def withLeft(newLeft: SigmaStateTree): Plus = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): Plus = copy(right = newRight)
}

case class Minus(override val left: SigmaStateTree,
                 override val right: SigmaStateTree) extends TwoArgumentsOperation {
  def withLeft(newLeft: SigmaStateTree): Minus = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): Minus = copy(right = newRight)
}

case class Xor(override val left: SigmaStateTree,
               override val right: SigmaStateTree) extends TwoArgumentsOperation {
  def withLeft(newLeft: SigmaStateTree): Xor = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): Xor = copy(right = newRight)
}

case class Append(override val left: SigmaStateTree,
                  override val right: SigmaStateTree) extends TwoArgumentsOperation {
  def withLeft(newLeft: SigmaStateTree): Append = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): Append = copy(right = newRight)
}

sealed trait Relation extends Triple

case class LT(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def withLeft(newLeft: SigmaStateTree): LT = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): LT = copy(right = newRight)
}

case class LE(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def withLeft(newLeft: SigmaStateTree): LE = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): LE = copy(right = newRight)
}

case class GT(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def withLeft(newLeft: SigmaStateTree): GT = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): GT = copy(right = newRight)
}

case class GE(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def withLeft(newLeft: SigmaStateTree): GE = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): GE = copy(right = newRight)
}

case class EQ(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def withLeft(newLeft: SigmaStateTree): EQ = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): EQ = copy(right = newRight)
}

case class NEQ(override val left: SigmaStateTree,
               override val right: SigmaStateTree) extends Relation {
  def withLeft(newLeft: SigmaStateTree): NEQ = copy(left = newLeft)

  def withRight(newRight: SigmaStateTree): NEQ = copy(right = newRight)
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

case class SchnorrUnproven(override val proposition: DLogNode,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Array[Byte]] = None,
                           override val simulated: Boolean) extends UnprovenLeaf {

  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class DiffieHellmanTupleUnproven(override val proposition: DiffieHellmanTupleNode,
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


case class SchnorrNode(override val proposition: DLogNode,
                       firstMessageOpt: Option[FirstDLogProverMessage],
                       challenge: Array[Byte],
                       secondMessage: SecondDLogProverMessage)
  extends UncheckedSigmaTree[DLogNode] {

  override val propCode: SigmaProposition.PropositionCode = DLogNode.Code
  override type M = this.type

  override def serializer: Serializer[M] = ???
}

case class DiffieHellmanTupleUncheckedNode(override val proposition: DiffieHellmanTupleNode,
                                           firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                           challenge: Array[Byte],
                                           secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedSigmaTree[DiffieHellmanTupleNode] {

  override val propCode: SigmaProposition.PropositionCode = DiffieHellmanTupleNode.Code
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