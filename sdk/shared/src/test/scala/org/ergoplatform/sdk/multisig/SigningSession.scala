package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.ReducedTransaction
import sigmastate.PositionedLeaf
import sigmastate.interpreter.HintsBag

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
    collectedHints: HintsBag
) {
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
        val action = if (collectedHints.hints.isEmpty)
          CreateCommitment(signer, inputIndex, pl)
        else
          CreateSignature(signer, inputIndex, pl)
        action
      }
    }
  }

  def execute(action: SigningAction): SigningSession = {
//    val newHints: HintsBag = action match {
//      case CreateCommitment(signer) =>
//        val positions = positionsToProve
//        val commitments = positions.map { positions =>
//          positions.map { pl =>
//            val leaf = pl.leaf
//            val position = pl.position
//            val commitment = signer.prover.createCommitment(leaf, position)
//            (leaf, position, commitment)
//          }
//        }
//        collectedHints.add(commitments)
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
//    }
    this
  }
}

object SigningSession {
}