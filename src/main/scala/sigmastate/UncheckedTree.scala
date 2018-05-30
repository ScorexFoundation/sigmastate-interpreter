package sigmastate

import java.util

import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.SigmaBoolean
import sigmastate.utils.Helpers

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree[ST <: SigmaBoolean] extends UncheckedTree {
  val proposition: ST
}

trait UncheckedConjecture[ST <: SigmaBoolean] extends UncheckedSigmaTree[ST] {
  val challengeOpt: Option[Array[Byte]]
  val commitments: Seq[FirstProverMessage[_]]
  val leafs: Seq[ProofTree]

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedConjecture[_] =>
      proposition == x.proposition &&
        Helpers.optionArrayEquals(challengeOpt, x.challengeOpt) &&
        commitments == x.commitments &&
        leafs == x.leafs
  }
}

trait UncheckedLeaf[ST <: SigmaBoolean] extends UncheckedSigmaTree[ST]{
  val challenge: Array[Byte]
}

case class UncheckedSchnorr(override val proposition: ProveDlog,
                            firstMessageOpt: Option[FirstDLogProverMessage],
                            override val challenge: Array[Byte],
                            secondMessage: SecondDLogProverMessage)
  extends UncheckedLeaf[ProveDlog] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedSchnorr =>
        util.Arrays.equals(challenge, x.challenge) &&
        firstMessageOpt == x.firstMessageOpt &&
        secondMessage == x.secondMessage
    case _ => false
  }
}


case class UncheckedDiffieHellmanTuple(override val proposition: ProveDiffieHellmanTuple,
                                       firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                       override val challenge: Array[Byte],
                                       secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedLeaf[ProveDiffieHellmanTuple] {

  override def equals(obj: Any): Boolean = obj match {
    case x: UncheckedDiffieHellmanTuple =>
      proposition == x.proposition &&
      firstMessageOpt == x.firstMessageOpt &&
      util.Arrays.equals(challenge, x.challenge) &&
      secondMessage == x.secondMessage
  }
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             override val leafs: Seq[ProofTree])
  extends UncheckedConjecture[CAND]


case class COrUncheckedNode(override val proposition: COR,
                            override val challengeOpt: Option[Array[Byte]],
                            override val commitments: Seq[FirstProverMessage[_]],
                            override val leafs: Seq[ProofTree]) extends UncheckedConjecture[COR]
