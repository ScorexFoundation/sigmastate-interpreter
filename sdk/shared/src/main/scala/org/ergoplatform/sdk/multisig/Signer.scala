package org.ergoplatform.sdk.multisig

import org.ergoplatform.P2PKAddress
import org.ergoplatform.sdk.SigmaProver

case class Signer(prover: SigmaProver) {
  def masterAddress: P2PKAddress = prover.getP2PKAddress
  def eip3Addresses: Seq[P2PKAddress] = prover.getEip3Addresses
}

object Signer {


  def main(args: Array[String]): Unit = {

//    val pkAlice =
  }
}