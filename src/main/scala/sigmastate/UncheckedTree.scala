package sigmastate

import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.SigmaBoolean

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree[ST <: SigmaBoolean] extends UncheckedTree {
  val proposition: ST
}

trait UncheckedConjecture[ST <: SigmaBoolean] extends UncheckedSigmaTree[ST] {
  val challengeOpt: Option[Array[Byte]]
  val commitments: Seq[FirstProverMessage[_]]
}


case class SchnorrNode(override val proposition: ProveDlog,
                       firstMessageOpt: Option[FirstDLogProverMessage],
                       challenge: Array[Byte],
                       secondMessage: SecondDLogProverMessage)
  extends UncheckedSigmaTree[ProveDlog] {
}

case class DiffieHellmanTupleUncheckedNode(override val proposition: ProveDiffieHellmanTuple,
                                           firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                           challenge: Array[Byte],
                                           secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedSigmaTree[ProveDiffieHellmanTuple] {
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             leafs: Seq[ProofTree])
  extends UncheckedConjecture[CAND] {
}


case class COr2UncheckedNode(override val proposition: COR,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             children: Seq[ProofTree]) extends UncheckedConjecture[COR] {
}
