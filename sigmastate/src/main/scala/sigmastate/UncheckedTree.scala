package sigmastate

import java.util

import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.Values.SigmaBoolean
import gf2t.GF2_192_Poly
import sigmastate.basics.{FirstDiffieHellmanTupleProverMessage, ProveDHTuple, SecondDiffieHellmanTupleProverMessage}

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree extends UncheckedTree {
  val challenge: Array[Byte]
}

trait UncheckedConjecture extends UncheckedSigmaTree with ProofTreeConjecture {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedConjecture =>
      util.Arrays.equals(challenge, x.challenge) && children == x.children
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
      util.Arrays.equals(challenge, x.challenge) &&
        commitmentOpt == x.commitmentOpt &&
        secondMessage == x.secondMessage
    case _ => false
  }
}


case class UncheckedDiffieHellmanTuple(override val proposition: ProveDHTuple,
                                       override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                       override val challenge: Challenge,
                                       secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedLeaf[ProveDHTuple] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedDiffieHellmanTuple =>
      proposition == x.proposition &&
        commitmentOpt == x.commitmentOpt &&
        util.Arrays.equals(challenge, x.challenge) &&
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
                                   k: Integer,
                                   polynomialOpt: Option[GF2_192_Poly]) extends UncheckedConjecture {
  require(children.length <= 255) // Our polynomial arithmetic can take only byte inputs
  require(k >= 0 && k <= children.length)

  override val conjectureType = ConjectureType.ThresholdConjecture
}
