package org.ergoplatform.sdk.multisig

import org.ergoplatform.P2PKAddress
import org.ergoplatform.sdk.{ReducedTransaction, SigmaProver}
import sigmastate.SigmaLeaf
import sigmastate.interpreter.HintsBag

case class Signer(prover: SigmaProver) {
  def masterAddress: P2PKAddress = prover.getP2PKAddress

  def eip3Addresses: Seq[P2PKAddress] = prover.getEip3Addresses

  def allAddresses: Seq[P2PKAddress] = masterAddress +: eip3Addresses

  def allKeys: Seq[SigmaLeaf] = allAddresses.map(_.pubkey)

  def canProve(leaf: SigmaLeaf): Boolean = allKeys.contains(leaf)

  def startCosigning(reduced: ReducedTransaction): SigningSession = {
    SigningSession(reduced)
  }
}

object Signer {
}