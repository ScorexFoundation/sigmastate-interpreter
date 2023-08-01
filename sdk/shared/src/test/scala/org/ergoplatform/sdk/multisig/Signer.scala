package org.ergoplatform.sdk.multisig

import org.ergoplatform.P2PKAddress
import org.ergoplatform.sdk.{ReducedTransaction, SigmaProver, SignedTransaction}
import scalan.reflection.memoize
import sigmastate.SigmaLeaf
import sigmastate.Values.SigmaBoolean
import sigmastate.interpreter.{HintsBag, RealCommitment}

import scala.collection.mutable

/** Stateful object, represents one participant in multisig session.
  * Keeps secret random values in secure persistent storage.
  * Can participate in many [[SigningSession]].
  */
class Signer(val prover: SigmaProver) {

  /** Mapping from (session id, input proposition) stored hints. */
  private val sessions = mutable.HashMap.empty[(SessionId, SigmaBoolean), HintsBag]

  /** Mapping from (session id, input proposition) stored proof hints. */
  private val proofs = mutable.HashMap.empty[(SessionId, SigmaBoolean), HintsBag]

  def masterAddress: P2PKAddress = prover.getP2PKAddress

  def pubkey: SigmaLeaf = masterAddress.pubkey

  def eip3Addresses: Seq[P2PKAddress] = prover.getEip3Addresses

  def allAddresses: Seq[P2PKAddress] = masterAddress +: eip3Addresses

  def allKeys: Seq[SigmaLeaf] = allAddresses.map(_.pubkey)

  def canProve(leaf: SigmaLeaf): Boolean = allKeys.contains(leaf)

  def getHintsBag(sessionId: SessionId, leaf: SigmaBoolean): Option[HintsBag] = {
    sessions.get((sessionId, leaf))
  }

  def startCosigning(reduced: ReducedTransaction): SigningSession = {
    SigningSession(reduced)
  }

  private def generateCommitments(sigmaTree: SigmaBoolean, sessionId: SessionId): Seq[RealCommitment] = {
    val bag = memoize(sessions)((sessionId, sigmaTree), prover.generateCommitments(sigmaTree))
    bag.realCommitments
  }

  private def generateProof(
      sb: SigmaBoolean, sessionId: SessionId,
      messageToSign: Array[Byte],
      hintsBag: HintsBag): Array[Byte] = {
    prover.generateProof(sb, messageToSign, hintsBag)
  }

  def getActionsFrom(session: SigningSession): Seq[SigningAction] = {
    val canProveInputs = session.positionsToProve.map { positions =>
      positions.filter { pl => canProve(pl.leaf) }
    }
    canProveInputs.zipWithIndex.flatMap { case (positions, inputIndex) =>
      positions.map { pl =>
        val action = if (session.collectedHints(inputIndex).hints.isEmpty)
          CreateCommitment(this.pubkey, inputIndex, pl)
        else
          CreateSignature(this.pubkey, inputIndex, pl)
        action
      }
    }
  }

  def execute(action: SigningAction, session: SigningSession): SigningSession = {
    val proposition = session.reduced.inputPropositions(action.inputIndex)
    val newHints = action match {
      case CreateCommitment(_, inputIndex, pl) =>
        val commitments = generateCommitments(proposition, session.id)
        commitments.filter(c => c.position == pl.position && c.image == pl.leaf)

      case CreateSignature(_, inputIndex, pl) =>
        val ownCommitments = getHintsBag(session.id, proposition).get.ownCommitments
        val otherCommitments = session.collectedHints(inputIndex)
            .filter(_.image != pl.leaf)
        val proof = generateProof(proposition,
          session.id,
          session.reduced.bytesToSign,
          otherCommitments.addHints(ownCommitments: _*))
        val proofHints = prover.extractHints(proposition, proof, Seq(pl.leaf), Seq.empty)
        proofHints.realProofs.filter(rp => rp.position == pl.position && rp.image == pl.leaf)
    }
    session.addHintsAt(action.inputIndex, newHints)
  }

  def createSignedTransaction(session: SigningSession): SignedTransaction = {
    prover.signReduced(session.reduced, session.collectedHints)
  }

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case s: Signer => s.prover == prover
    case _ => false
  })
  override def hashCode(): Int = prover.hashCode()
  override def toString: String = s"Signer($masterAddress)"
}

object Signer {
  def apply(prover: SigmaProver): Signer = new Signer(prover)
}