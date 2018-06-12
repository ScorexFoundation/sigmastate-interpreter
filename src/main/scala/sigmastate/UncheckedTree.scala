package sigmastate

import java.util

import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.SigmaBoolean
import sigmastate.utils.Helpers

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree extends UncheckedTree {
  val proposition: SigmaBoolean
}

// todo: why do ANDs and ORs have challenges that are optional, while leaves have challenges that are required? This causes messy code elsewhere
trait UncheckedConjecture extends UncheckedSigmaTree with ProofTreeConjecture {
  val challengeOpt: Option[Array[Byte]]
  val commitments: Seq[FirstProverMessage[_]]

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedConjecture =>
      // todo: why does the code below mix .equals and == ?
      proposition == x.proposition &&
        Helpers.optionArrayEquals(challengeOpt, x.challengeOpt) &&
        commitments == x.commitments &&
        children == x.children
  }
}

trait UncheckedLeaf[SP <: SigmaBoolean] extends UncheckedSigmaTree with ProofTreeLeaf {
  val challenge: Array[Byte]
}

case class UncheckedSchnorr(override val proposition: ProveDlog,
                            override val commitmentOpt: Option[FirstDLogProverMessage],
                            override val challenge: Array[Byte],
                            secondMessage: SecondDLogProverMessage)
  extends UncheckedLeaf[ProveDlog] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedSchnorr =>
      // todo: why does the code below mix .equals and == ?
      util.Arrays.equals(challenge, x.challenge) &&
        commitmentOpt == x.commitmentOpt &&
        secondMessage == x.secondMessage
    case _ => false
  }
}


case class UncheckedDiffieHellmanTuple(override val proposition: ProveDiffieHellmanTuple,
                                       override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                       override val challenge: Array[Byte],
                                       secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedLeaf[ProveDiffieHellmanTuple] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedDiffieHellmanTuple =>
      // todo: why does the code below mix .equals and == ?
      proposition == x.proposition &&
        commitmentOpt == x.commitmentOpt &&
        util.Arrays.equals(challenge, x.challenge) &&
        secondMessage == x.secondMessage
  }
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             override val children: Seq[ProofTree])
  extends UncheckedConjecture {

  override val conjectureType = ConjectureType.AndConjecture
}


case class COrUncheckedNode(override val proposition: COR,
                            override val challengeOpt: Option[Array[Byte]],
                            override val commitments: Seq[FirstProverMessage[_]],
                            override val children: Seq[ProofTree]) extends UncheckedConjecture {

  override val conjectureType = ConjectureType.OrConjecture
}
