package org.ergoplatform.sdk

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform._
import org.ergoplatform.sdk.wallet.protocol.context.ErgoLikeStateContext
import sigmastate.eval.{CostingSigmaDslBuilder, SigmaDsl}
import sigmastate.interpreter.HintsBag
import sigmastate.utils.Helpers.TryOps
import special.sigma.{BigInt, SigmaProp}

class Prover(_prover: AppkitProvingInterpreter, networkPrefix: NetworkPrefix) {
  implicit val ergoAddressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(networkPrefix)

  def getP2PKAddress: P2PKAddress = {
    val pk = _prover.pubKeys(0)
    P2PKAddress(pk)
  }

  def getSecretKey: BigInt =
    CostingSigmaDslBuilder.BigInt(_prover.secretKeys(0).privateInput.w)

  def getEip3Addresses: Seq[P2PKAddress] = {
    val addresses = _prover.secretKeys
        .drop(1)
        .map { k =>
          val p2pkAddress = P2PKAddress(k.publicImage)
          p2pkAddress
        }
    addresses
  }

  def sign(stateCtx: ErgoLikeStateContext, tx: UnsignedTransaction): SignedTransaction =
    sign(stateCtx, tx, baseCost = 0)

  def sign(stateCtx: ErgoLikeStateContext, tx: UnsignedTransaction, baseCost: Int): SignedTransaction = {
    val (signed, cost) = _prover.sign(
      unsignedTx = tx.ergoTx,
      boxesToSpend = tx.boxesToSpend,
      dataBoxes = tx.dataInputs,
      stateContext = stateCtx,
      baseCost = baseCost,
      tokensToBurn = tx.tokensToBurn
    ).getOrThrow
    SignedTransaction(signed, cost)
  }

  def signMessage(sigmaProp: SigmaProp, message:  Array[Byte], hintsBag: HintsBag): Array[Byte] = {
    _prover.signMessage(SigmaDsl.toSigmaBoolean(sigmaProp), message, hintsBag).getOrThrow
  }

  def reduce(stateCtx: ErgoLikeStateContext, tx: UnsignedTransaction, baseCost: Int): ReducedTransaction = {
    val (reduced, cost) = _prover.reduceTransaction(
      unsignedTx = tx.ergoTx,
      boxesToSpend = tx.boxesToSpend,
      dataBoxes = tx.dataInputs,
      stateContext = stateCtx,
      baseCost = baseCost,
      tokensToBurn = tx.tokensToBurn)
    ReducedTransaction(reduced, cost)
  }

  def signReduced(tx: ReducedTransaction, baseCost: Int): SignedTransaction = {
    val (signed, cost) = _prover.signReduced(tx.ergoTx, baseCost)
    SignedTransaction(signed, cost)
  }

}
