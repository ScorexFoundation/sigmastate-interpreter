package org.ergoplatform.sdk.js

import org.ergoplatform.sdk
import scorex.util.encode.Base16
import sigma.interpreter.js.ProverHints
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.fleetSdkCommon.{distEsmTypesBoxesMod => boxesMod, distEsmTypesCommonMod => commonMod, distEsmTypesInputsMod => inputsMod, distEsmTypesProverResultMod => proverResultMod, distEsmTypesTokenMod => tokenMod, distEsmTypesTransactionsMod => transactionsMod}

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sdk.SigmaProver]] available from JS. */
@JSExportTopLevel("SigmaProver")
class SigmaProver(_prover: sdk.SigmaProver) extends js.Object {
  import Isos._

  /** Returns the Pay-to-Public-Key (P2PK) address associated with the prover's public key.
    * The returned address corresponds to the master secret derived from the mnemonic
    * phrase configured in the [[ProverBuilder]].
    */
  def getP2PKAddress(): String = {
    val addr = _prover.getP2PKAddress
    addr.toString
  }

  /** Returns the prover's secret key. */
  def getSecretKey(): js.BigInt =
    sigma.js.Isos.isoBigInt.from(_prover.getSecretKey)

  /** Returns an array of EIP-3 addresses associated with the prover's secret keys. */
  def getEip3Addresses(): js.Array[String] = {
    val addresses = _prover.getEip3Addresses
    js.Array(addresses.map(_.toString): _*)
  }

  /** Reduces the transaction to the reduced form, which is ready to be signed.
    *
    * @param stateCtx     blockchain state context
    * @param unsignedTx   unsigned transaction to be reduced (created by Fleet builders)
    * @param boxesToSpend boxes to be spent by the transaction
    * @param dataInputs   data inputs to be used by the transaction
    * @param tokensToBurn tokens to be burned by the transaction
    * @param baseCost     base cost of the transaction
    * @return reduced transaction
    */
  def reduce(
    stateCtx: BlockchainStateContext,
    unsignedTx: transactionsMod.UnsignedTransaction,
    boxesToSpend: js.Array[inputsMod.EIP12UnsignedInput],
    dataInputs: js.Array[boxesMod.Box[commonMod.Amount, NonMandatoryRegisters]],
    tokensToBurn: js.Array[tokenMod.TokenAmount[commonMod.Amount]],
    baseCost: Int
  ): ReducedTransaction = {
    val unreducedTx = sdk.UnreducedTransaction(
      unsignedTx = isoUnsignedTransaction.to(unsignedTx),
      boxesToSpend = sigma.js.Isos.isoArrayToIndexed(isoEIP12UnsignedInput).to(boxesToSpend),
      dataInputs = sigma.js.Isos.isoArrayToIndexed(sigma.js.Box.isoBox).to(dataInputs),
      tokensToBurn = sigma.js.Isos.isoArrayToIndexed(sigma.data.js.Isos.isoToken.andThen(sdk.SdkIsos.isoErgoTokenToPair.inverse)).to(tokensToBurn)
    )
    val ctx         = isoBlockchainStateContext.to(stateCtx)
    val reducedTx   = _prover.reduce(ctx, unreducedTx, baseCost)
    new ReducedTransaction(reducedTx)
  }

  /** Signs the reduced transaction.
    * @param reducedTx reduced transaction to be signed
    * @return signed transaction containting all the required proofs (signatures)
    */
  def signReduced(reducedTx: ReducedTransaction, hints: UndefOr[TransactionHintsBag]): transactionsMod.SignedTransaction = {
    val hintsSdk = sigma.js.Isos.isoUndefOr(TransactionHintsBag.isoToSdk).to(hints)
    val signed = _prover.signReduced(reducedTx._tx, hintsSdk)
    isoSignedTransaction.from(signed.ergoTx)
  }

  /** Generates proof (aka signature) for the given message using secrets of this prover.
    * All the necessary secrets should be configured in this prover to satisfy the given
    * sigma proposition in the reducedInput.
    */
  def signReduced(
    reducedInput: ReducedInputData,
    messageHex: String,
    hintsBag: UndefOr[ProverHints]
  ): proverResultMod.ProverResult = {
    val input = ReducedInputData.isoToSdk.to(reducedInput)
    val res   = _prover.signReduced(input,
      message = Base16.decode(messageHex).get,
      sigma.js.Isos.isoUndefOr(ProverHints.isoProverHints).to(hintsBag)
    )
    Isos.isoProverResult.from(res)
  }

  /** Generates commitments for a given `ReducedTransaction` using the wallets's secret keys.
    *
    * @param reducedTx reduced transaction to generate commitments
    * @return a secrete and public hints for each input of the transaction
    */
  def generateCommitments(reducedTx: ReducedTransaction): TransactionHintsBag = {
    val bag = _prover.generateCommitments(reducedTx._tx)
    TransactionHintsBag.isoToSdk.from(bag)
  }

}
