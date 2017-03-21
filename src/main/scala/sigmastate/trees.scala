package sigmastate

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData, GroupElement}
import scapi.sigma.rework.DLogProtocol._
import scapi.sigma.rework.{Challenge, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import org.bitbucket.inkytonik.kiama.relation._
import sigmastate.SigmaProposition.PropositionCode

import scala.util.Try


sealed trait SigmaStateTree extends Product

trait StateTree extends SigmaStateTree

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


sealed abstract class BooleanConstantTree(val value: Boolean) extends SigmaStateTree

object BooleanConstantTree {
  def fromBoolean(v: Boolean) = v match {
    case true => TrueConstantTree
    case false => FalseConstantTree
  }
}

case object TrueConstantTree extends BooleanConstantTree(true)

case object FalseConstantTree extends BooleanConstantTree(false)


case class OR(left: SigmaStateTree, right: SigmaStateTree) extends SigmaStateTree

case class AND(left: SigmaStateTree, right: SigmaStateTree) extends SigmaStateTree


trait Relation extends StateTree

case class LT(left: SigmaStateTree, right: SigmaStateTree) extends Relation

case class LE(left: SigmaStateTree, right: SigmaStateTree) extends Relation

case class GT(left: SigmaStateTree, right: SigmaStateTree) extends Relation

case class GE(left: SigmaStateTree, right: SigmaStateTree) extends Relation

case class EQ(left: SigmaStateTree, right: SigmaStateTree) extends Relation


trait Value extends StateTree

case class IntLeaf(value: Int) extends Value

trait Variable extends Value

case object Height extends Variable


//Proof tree

trait ProofTree extends Product

sealed trait CheckedProof extends ProofTree

case object SuccessfulProof extends CheckedProof

case class FailedProof[SP <: SigmaProposition](proposition: SP) extends CheckedProof


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

case class CAndUncheckedNode(override val proposition: CAND, override val challenge: Array[Byte], leafs: Seq[ProofTree])
  extends UncheckedSigmaTree[CAND](proposition, challenge) {

  override def verify(): Boolean =
    leafs.zip(proposition.sigmaTrees).forall { case (proof, prop) =>
      proof match {
        case SuccessfulProof => true
        case FailedProof(_) => false
        case ut: UncheckedSigmaTree[_] => ut.challenge.sameElements(this.challenge) && ut.verify()
      }
    }

  override val propCode: PropositionCode = CAND.Code
  override type M = CAndUncheckedNode

  override def serializer: Serializer[M] = ???
}

//todo: implement
case class COrUncheckedNode(override val proposition: COR, override val challenge: Array[Byte], leafs: Seq[ProofTree])
  extends UncheckedSigmaTree(proposition, challenge) {

  override def verify(): Boolean = ???

  override val propCode: PropositionCode = COR.Code

  override type M = COrUncheckedNode

  override def serializer: Serializer[M] = ???
}