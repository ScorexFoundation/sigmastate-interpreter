package sigmastate

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData, GroupElement}
import scapi.sigma.rework.DLogProtocol._
import scapi.sigma.rework.{Challenge, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import sigmastate.SigmaProposition.PropositionCode


sealed trait SigmaStateTree extends Product with SigmaStateProposition

trait StateTree extends SigmaStateTree with StateProposition

trait SigmaTree extends SigmaStateTree with SigmaProposition

case class CAND(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override val code: PropositionCode = CAND.Code
  override type M = this.type
}

object CAND {
  val Code = 101: Byte
}

case class COR(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override val code: PropositionCode = COR.Code
  override type M = this.type
}

object COR {
  val Code = 101: Byte
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaTree with ProofOfKnowledgeProposition[S] with SigmaProtocolCommonInput[SP]

case class DLogNode(h: GroupElement)
  extends SigmaProtocolCommonInput[DLogSigmaProtocol]
    with SigmaProofOfKnowledgeTree[DLogSigmaProtocol, DLogProverInput] {

  override type M = this.type
  override val code: PropositionCode = DLogCommonInput.Code

  override def serializer: Serializer[DLogNode.this.type] = ???

  override val dlogGroup: DlogGroup = new BcDlogECFp()
  override val soundness: Int = 256

  lazy val toCommonInput: DLogCommonInput = DLogCommonInput(dlogGroup, h, soundness)
}


case class OR(children: Seq[SigmaStateTree]) extends SigmaStateTree

object OR {
  def apply(left: SigmaStateTree, right: SigmaStateTree): OR = apply(Seq(left, right))
}


case class AND(children: Seq[SigmaStateTree]) extends SigmaStateTree

object AND {
  def apply(left: SigmaStateTree, right: SigmaStateTree): AND = apply(Seq(left, right))
}


trait Value extends StateTree

case class IntLeaf(value: Long) extends Value

sealed abstract class BooleanConstantTree(val value: Boolean) extends Value

object BooleanConstantTree {
  def fromBoolean(v: Boolean) = v match {
    case true => TrueConstantTree
    case false => FalseConstantTree
  }
}

case object TrueConstantTree extends BooleanConstantTree(true)

case object FalseConstantTree extends BooleanConstantTree(false)

trait Variable[V <: Value] extends Value

case object Height extends Variable[IntLeaf]


sealed trait Relation extends StateTree {
  val left: SigmaStateTree
  val right: SigmaStateTree

  def swapLeft(newLeft: SigmaStateTree): Relation

  def swapRight(newRight: SigmaStateTree): Relation
}

case class LT(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def swapLeft(newLeft: SigmaStateTree): LT = copy(left = newLeft)

  def swapRight(newRight: SigmaStateTree): LT = copy(right = newRight)
}

case class LE(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def swapLeft(newLeft: SigmaStateTree): LE = copy(left = newLeft)

  def swapRight(newRight: SigmaStateTree): LE = copy(right = newRight)
}

case class GT(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def swapLeft(newLeft: SigmaStateTree): GT = copy(left = newLeft)

  def swapRight(newRight: SigmaStateTree): GT = copy(right = newRight)
}

case class GE(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def swapLeft(newLeft: SigmaStateTree): GE = copy(left = newLeft)

  def swapRight(newRight: SigmaStateTree): GE = copy(right = newRight)
}

case class EQ(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def swapLeft(newLeft: SigmaStateTree): EQ = copy(left = newLeft)

  def swapRight(newRight: SigmaStateTree): EQ = copy(right = newRight)
}


//Proof tree

trait ProofTree extends Product

sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaTree

  val challengeOpt: Option[Array[Byte]]

  def setChallenge(challenge: Array[Byte]): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree {
  val simulated: Boolean
}

case class CAndUnproven(override val proposition: CAND,
                        override val challengeOpt: Option[Array[Byte]] = None, children: Seq[UnprovenTree]) extends UnprovenTree {
  override def setChallenge(challenge: Array[Byte]) = CAndUnproven(proposition, Some(challenge), children)
}

case class COrUnproven(override val proposition: COR,
                       override val challengeOpt: Option[Array[Byte]] = None, children: Seq[UnprovenTree]) extends UnprovenTree {
  override def setChallenge(challenge: Array[Byte]) = COrUnproven(proposition, Some(challenge), children)
}


case class SchnorrUnproven(override val challengeOpt: Option[Array[Byte]] = None,
                           override val simulated: Boolean,
                           override val proposition: DLogCommonInput) extends UnprovenLeaf {
  override def setChallenge(challenge: Array[Byte]) = SchnorrUnproven(Some(challenge), simulated, proposition)
}

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

abstract class UncheckedSigmaTree[ST <: SigmaTree](val proposition: ST, val challenge: Array[Byte])
  extends Proof[ST] with UncheckedTree

case class SchnorrNode(override val proposition: DLogCommonInput,
                       override val challenge: Array[Byte], signature: Array[Byte]) extends
  UncheckedSigmaTree[DLogCommonInput](proposition, challenge)
  with ProofOfKnowledge[DLogSigmaProtocol, DLogCommonInput] {

  override def verify(): Boolean = {
    //signature is g^r as a pair of points, and z
    val (grx, gry, zb) = EcPointFunctions.decodeBigIntTriple(signature).get
    val gr = new ECElementSendableData(grx, gry)

    //h = g^w is a pubkey
    val x: DLogCommonInput = proposition

    val a: FirstDLogProverMessage = FirstDLogProverMessage(gr)

    val z: SecondDLogProverMessage = SecondDLogProverMessage(zb)

    val sigmaTranscript = DLogTranscript(x, a, Challenge(challenge), z)
    sigmaTranscript.accepted
  }

  override val propCode: SigmaProposition.PropositionCode = DLogCommonInput.Code
  override type M = this.type

  override def serializer: Serializer[M] = ???
}

case class CAndUncheckedNode(override val proposition: CAND, override val challenge: Array[Byte], leafs: Seq[UncheckedTree])
  extends UncheckedSigmaTree[CAND](proposition, challenge) {

  override def verify(): Boolean =
    leafs.zip(proposition.sigmaTrees).forall { case (proof, prop) =>
      proof match {
        case NoProof => true
        case ut: UncheckedSigmaTree[_] => ut.challenge.sameElements(this.challenge) && ut.verify()
      }
    }

  override val propCode: PropositionCode = CAND.Code
  override type M = CAndUncheckedNode

  override def serializer: Serializer[M] = ???
}

//todo: implement
case class COrUncheckedNode(override val proposition: COR, override val challenge: Array[Byte], leafs: Seq[UncheckedTree])
  extends UncheckedSigmaTree(proposition, challenge) {

  override def verify(): Boolean = ???

  override val propCode: PropositionCode = COR.Code

  override type M = COrUncheckedNode

  override def serializer: Serializer[M] = ???
}