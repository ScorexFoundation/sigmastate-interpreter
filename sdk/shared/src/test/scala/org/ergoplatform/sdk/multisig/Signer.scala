package org.ergoplatform.sdk.multisig

import org.ergoplatform.P2PKAddress
import org.ergoplatform.sdk.{ReducedTransaction, SigmaProver}
import sigmastate.SigmaLeaf
import sigmastate.Values.SigmaBoolean
import sigmastate.interpreter.HintsBag

class Signer(val prover: SigmaProver) {
  def masterAddress: P2PKAddress = prover.getP2PKAddress

  def eip3Addresses: Seq[P2PKAddress] = prover.getEip3Addresses

  def allAddresses: Seq[P2PKAddress] = masterAddress +: eip3Addresses

  def allKeys: Seq[SigmaLeaf] = allAddresses.map(_.pubkey)

  def canProve(leaf: SigmaLeaf): Boolean = allKeys.contains(leaf)

  def startCosigning(reduced: ReducedTransaction): SigningSession = {
    SigningSession(reduced)
  }

  def generateCommitments(sigmaTree: SigmaBoolean): HintsBag = {
    prover.generateCommitments(sigmaTree)
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