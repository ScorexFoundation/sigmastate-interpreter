package sigmastate

import java.util

import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import scapi.sigma.VerifierMessage.Challenge
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.SigmaBoolean
import gf2t.GF2_192_Poly;

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree extends UncheckedTree {
  val challenge: Array[Byte]
}

trait UncheckedConjecture extends UncheckedSigmaTree with ProofTreeConjecture {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedConjecture =>
        util.Arrays.equals(challenge, x.challenge) && // todo: why does this code mix .equals and == ?
        children == x.children
  }
}

trait UncheckedLeaf[SP <: SigmaBoolean] extends UncheckedSigmaTree with ProofTreeLeaf {
  val proposition: SigmaBoolean
}

case class UncheckedSchnorr(override val proposition: ProveDlog,
                            override val commitmentOpt: Option[FirstDLogProverMessage],
                            override val challenge: Challenge,
                            secondMessage: SecondDLogProverMessage)
  extends UncheckedLeaf[ProveDlog] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedSchnorr =>
        util.Arrays.equals(challenge, x.challenge) && // todo: why does this code mix .equals and == ?
        commitmentOpt == x.commitmentOpt &&
        secondMessage == x.secondMessage
    case _ => false
  }
}


case class UncheckedDiffieHellmanTuple(override val proposition: ProveDiffieHellmanTuple,
                                       override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                       override val challenge: Challenge,
                                       secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedLeaf[ProveDiffieHellmanTuple] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedDiffieHellmanTuple =>
      proposition == x.proposition &&
      commitmentOpt == x.commitmentOpt &&
      util.Arrays.equals(challenge, x.challenge) && // todo: why does this code mix .equals and == ?
      secondMessage == x.secondMessage
  }
}

case class CAndUncheckedNode(override val challenge: Challenge,
                             override val children: Seq[UncheckedSigmaTree])
  extends UncheckedConjecture {

  override val conjectureType = ConjectureType.AndConjecture
}


case class COrUncheckedNode(override val challenge: Challenge,
                            override val children: Seq[UncheckedSigmaTree]) extends UncheckedConjecture {

  override val conjectureType = ConjectureType.OrConjecture

}

case class CThresholdUncheckedNode(override val challenge: Challenge,
                            override val children: Seq[UncheckedSigmaTree],
                            k: Integer, polynomialOpt: Option[GF2_192_Poly]) extends UncheckedConjecture {

  // TODO: how to enforce limits on k (0 to number of children) and on number of children (at most 255)?
  override val conjectureType = ConjectureType.ThresholdConjecture
}