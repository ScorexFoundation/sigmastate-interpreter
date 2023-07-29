package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.ReducedTransaction
import sigmastate.PositionedLeaf
import sigmastate.interpreter.{Hint, HintsBag}

case class SessionId(value: String) extends AnyVal

trait SigningAction {
  def signer: Signer
  def inputIndex: Int
  def leaf: PositionedLeaf
}

case class CreateCommitment(signer: Signer, inputIndex: Int, leaf: PositionedLeaf) extends SigningAction

case class CreateSignature(signer: Signer, inputIndex: Int, leaf: PositionedLeaf) extends SigningAction


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

  def getActionsFor(signer: Signer): Seq[SigningAction] = {
    val canProve = positionsToProve.map { positions =>
      positions.filter { pl => signer.canProve(pl.leaf) }
    }
    canProve.zipWithIndex.flatMap { case (positions, inputIndex) =>
      positions.map { pl =>
        val action = if (collectedHints(inputIndex).hints.isEmpty)
          CreateCommitment(signer, inputIndex, pl)
        else
          CreateSignature(signer, inputIndex, pl)
        action
      }
    }
  }

  private def addHintsAt(inputIndex: Int, hints: Seq[Hint]): SigningSession = {
    val existingHints = collectedHints(inputIndex)
    val newBag = existingHints.addHints(hints: _*)
    copy(collectedHints = collectedHints.updated(inputIndex, newBag))
  }

  def execute(action: SigningAction): SigningSession = {
    val newHints: Seq[Hint] = action match {
      case CreateCommitment(signer, inputIndex, pl) =>
        val proposition = reduced.sigmaPropositions(inputIndex)
        val commitments = signer.prover.generateCommitments(proposition).realCommitments
        commitments.filter { c => c.position == pl.position && c.image == pl.leaf }

//      case CreateSignature(signer) =>
//        val positions = positionsToProve
//        val signatures = positions.map { positions =>
//          positions.map { pl =>
//            val leaf = pl.leaf
//            val position = pl.position
//            val signature = signer.prover.createSignature(leaf, position, collectedHints)
//            (leaf, position, signature)
//          }
//        }
//        collectedHints.addHints(signatures)
      case _ => Seq.empty
    }
    addHintsAt(action.inputIndex, newHints)
  }

}

object SigningSession {
  def apply(reduced: ReducedTransaction): SigningSession = {
    val collectedHints = Vector.fill(reduced.ergoTx.reducedInputs.length)(HintsBag.empty)
    SigningSession(reduced, collectedHints)
  }
}