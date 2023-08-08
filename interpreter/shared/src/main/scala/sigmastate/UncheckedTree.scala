package sigmastate

import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.{FirstDHTupleProverMessage, ProveDHTuple, SecondDHTupleProverMessage}
import sigmastate.crypto.GF2_192_Poly

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree extends UncheckedTree {
  val challenge: Challenge
}

trait UncheckedConjecture extends UncheckedSigmaTree with ProofTreeConjecture

trait UncheckedLeaf extends UncheckedSigmaTree with ProofTreeLeaf

case class UncheckedSchnorr(
    override val proposition: ProveDlog,
    override val commitmentOpt: Option[FirstDLogProverMessage],
    override val challenge: Challenge,
    secondMessage: SecondDLogProverMessage
) extends UncheckedLeaf

case class UncheckedDiffieHellmanTuple(
    override val proposition: ProveDHTuple,
    override val commitmentOpt: Option[FirstDHTupleProverMessage],
    override val challenge: Challenge,
    secondMessage: SecondDHTupleProverMessage
) extends UncheckedLeaf

case class CAndUncheckedNode(
    override val challenge: Challenge,
    override val children: Seq[UncheckedSigmaTree]) extends UncheckedConjecture {
  override val conjectureType = ConjectureType.AndConjecture
}

case class COrUncheckedNode(
    override val challenge: Challenge,
    override val children: Seq[UncheckedSigmaTree]) extends UncheckedConjecture {
  override val conjectureType = ConjectureType.OrConjecture
}

case class CThresholdUncheckedNode(
    override val challenge: Challenge,
    override val children: Seq[UncheckedSigmaTree],
    k: Integer,
    polynomialOpt: Option[GF2_192_Poly]) extends UncheckedConjecture {
  require(children.length <= 255) // Our polynomial arithmetic can take only byte inputs
  require(k >= 0 && k <= children.length)

  override val conjectureType = ConjectureType.ThresholdConjecture
}
