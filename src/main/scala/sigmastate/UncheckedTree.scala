package sigmastate

import java.util

import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.SigmaBoolean
import sigmastate.utils.Helpers

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree extends UncheckedTree {
  val challenge: Array[Byte]
}

trait UncheckedConjecture extends UncheckedSigmaTree with ProofTreeConjecture {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedConjecture =>
        util.Arrays.equals(challenge, x.challenge) &&
        children == x.children
  }
}

trait UncheckedLeaf[SP <: SigmaBoolean] extends UncheckedSigmaTree with ProofTreeLeaf {
  val proposition: SigmaBoolean
}

case class UncheckedSchnorr(override val proposition: ProveDlog,
                            override val commitmentOpt: Option[FirstDLogProverMessage],
                            override val challenge: Array[Byte],
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


case class UncheckedDiffieHellmanTuple(override val proposition: ProveDiffieHellmanTuple,
                                       override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                       override val challenge: Array[Byte],
                                       secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedLeaf[ProveDiffieHellmanTuple] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedDiffieHellmanTuple =>
      proposition == x.proposition &&
      commitmentOpt == x.commitmentOpt &&
      util.Arrays.equals(challenge, x.challenge) &&
      secondMessage == x.secondMessage
  }
}

case class CAndUncheckedNode(override val challenge: Array[Byte],
                             override val children: Seq[UncheckedSigmaTree])
  extends UncheckedConjecture {

  override val conjectureType = ConjectureType.AndConjecture
}


case class COrUncheckedNode(override val challenge: Array[Byte],
                            override val children: Seq[UncheckedSigmaTree]) extends UncheckedConjecture {

  override val conjectureType = ConjectureType.OrConjecture
}