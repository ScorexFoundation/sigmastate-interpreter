package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.Extensions.IndexedSeqOps
import org.ergoplatform.sdk.ReducedTransaction
import sigmastate.{PositionedLeaf, SigmaLeaf}
import sigmastate.interpreter.{Hint, HintsBag}

case class SessionId(value: String) extends AnyVal

trait SigningAction {
  def signerPk: SigmaLeaf
  def inputIndex: Int
  def leaf: PositionedLeaf
}

case class CreateCommitment(signerPk: SigmaLeaf, inputIndex: Int, leaf: PositionedLeaf) extends SigningAction

case class CreateSignature(signerPk: SigmaLeaf, inputIndex: Int, leaf: PositionedLeaf) extends SigningAction


case class SigningSession(
    reduced: ReducedTransaction,
    collectedHints: Vector[HintsBag]
) {
  require(reduced.ergoTx.reducedInputs.length == collectedHints.length,
    s"Collected hints should be provided for each input, but got ${collectedHints.length} hints for ${reduced.ergoTx.reducedInputs.length} inputs")

  def id: SessionId = SessionId(reduced.ergoTx.unsignedTx.id)

  /** Returns a seq of public keys (leaf sigma propositions) for each input.
    * For each such leaf a proof should be generated to complete this session.
    */
  lazy val positionsToProve: Seq[Seq[PositionedLeaf]] = {
    val inputs = reduced.ergoTx.reducedInputs
    inputs.map { reducedInput =>
      val sb = reducedInput.reductionResult.value
      sb.leaves()
    }
  }

  def addHintsAt(inputIndex: Int, hints: Seq[Hint]): SigningSession = {
    copy(collectedHints = collectedHints.modify(inputIndex, _.addHints(hints: _*)))
  }


}

object SigningSession {
  def apply(reduced: ReducedTransaction): SigningSession = {
    val collectedHints = Vector.fill(reduced.ergoTx.reducedInputs.length)(HintsBag.empty)
    SigningSession(reduced, collectedHints)
  }
}