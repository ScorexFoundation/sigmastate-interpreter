package org.ergoplatform.sdk

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform._
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import sigma.Extensions.TryOps
import sigma.data.{CSigmaDslBuilder, SigmaBoolean, SigmaLeaf}
import sigma.eval.SigmaDsl
import sigma.interpreter.ProverResult
import sigmastate.interpreter.HintsBag
import sigma.{BigInt, SigmaProp}
import sigmastate.crypto.SigmaProtocolPrivateInput

/** Represents a prover for signing Ergo transactions and messages.
  *
  * Note, the class is mutable and allow appending secret keys to the prover.
  * This is to allow easier migration to/from alternative SDKs like JS, Rust, etc.
  *
  * @param _prover        an instance of interpreter and a prover combined
  * @param networkPrefix  the network prefix for Ergo addresses
  */
class SigmaProver(var _prover: AppkitProvingInterpreter, networkPrefix: NetworkPrefix) {
  implicit val ergoAddressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(networkPrefix)

  /** All secrets available to this interpreter including [[ExtendedSecretKey]], dlog and
    * dht secrets.
    */
  def secrets: Seq[SigmaProtocolPrivateInput[SigmaLeaf]] =
    _prover.secrets.asInstanceOf[Seq[SigmaProtocolPrivateInput[SigmaLeaf]]]

  /** Returns the Pay-to-Public-Key (P2PK) address associated with the prover's public key.
    * The returned address corresponds to the master secret derived from the mnemonic
    * phrase configured in the [[ProverBuilder]].
    */
  def getP2PKAddress: P2PKAddress = {
    val pk = _prover.pubKeys(0)
    P2PKAddress(pk)
  }

  /** Returns the prover's secret key. */
  def getSecretKey: BigInt =
    CSigmaDslBuilder.BigInt(_prover.secretKeys(0).privateInput.w)

  /** Returns a sequence of EIP-3 addresses associated with the prover's secret keys. */
  def getEip3Addresses: Seq[P2PKAddress] = {
    val addresses = _prover.secretKeys
        .drop(1) // the master address
        .map { k =>
          val p2pkAddress = P2PKAddress(k.publicImage)
          p2pkAddress
        }
    addresses
  }

  /** Signs a given `UnreducedTransaction` using the prover's secret keys and the provided [[BlockchainStateContext]].
    * Uses baseCost == 0.
    */
  def sign(
    stateCtx: BlockchainStateContext,
    tx: UnreducedTransaction,
    hints: Option[TransactionHintsBag] = None
  ): SignedTransaction =
    sign(stateCtx, tx, baseCost = 0, hints)

  /** Signs a given `UnreducedTransaction` using the prover's secret keys and the provided [[BlockchainStateContext]].
    * Uses the given baseCost.
    */
  def sign(
    stateCtx: BlockchainStateContext,
    tx: UnreducedTransaction,
    baseCost: Int,
    hints: Option[TransactionHintsBag]
  ): SignedTransaction = {
    val signed = _prover
        .sign(tx, stateContext = stateCtx, baseCost = baseCost, hints = hints)
        .getOrThrow
    signed
  }

  /** Sign arbitrary message under a key representing a statement provable via a sigma-protocol.
    *
    * @param sigmaProp - public key
    * @param message   - message to sign
    * @param hintsBag  - additional hints for a signer (useful for distributed signing)
    * @return - signature bytes
    */
  def signMessage(sigmaProp: SigmaProp, message:  Array[Byte], hintsBag: HintsBag): Array[Byte] = {
    _prover.signMessage(SigmaDsl.toSigmaBoolean(sigmaProp), message, hintsBag).getOrThrow
  }

  /** Reduces a given `UnreducedTransaction` using the prover's secret keys and the
    * provided [[BlockchainStateContext]] with a base cost.
    */
  def reduce(stateCtx: BlockchainStateContext, tx: UnreducedTransaction, baseCost: Int): ReducedTransaction = {
    val reduced = _prover.reduceTransaction(
      unreducedTx = tx, stateContext = stateCtx, baseCost = baseCost)
    reduced
  }

  /** Reduces a given input of the given `UnreducedTransaction` using the prover's secret
    * keys and the provided [[BlockchainStateContext]] with a base cost.
    */
  def reduceTransactionInput(stateCtx: BlockchainStateContext, tx: UnreducedTransaction, inputIdx: Int): ReducedInputData = {
    val inputData = _prover.reduceTransactionInput(
      unreducedTx = tx, inputIdx, stateContext = stateCtx)
    inputData
  }

  /** Signs a given ReducedTransaction using the prover's secret keys. */
  def signReduced(tx: ReducedTransaction, hints: Option[TransactionHintsBag] = None): SignedTransaction = {
    _prover.signReduced(tx, tx.ergoTx.cost, hints)
  }

  /** Generates proof (aka signature) for the given message using secrets of this prover.
    * All the necessary secrets should be configured in this prover to satisfy the given
    * sigma proposition in the reducedInput.
    */
  def signReduced(
    reducedInput: ReducedInputData,
    message: Array[Byte],
    hintsBag: Option[HintsBag]): ProverResult = {
    _prover.proveReduced(reducedInput, message, hintsBag.getOrElse(HintsBag.empty))
  }

  /** Generates commitments for a given sigma proposition. */
  def generateCommitments(sb: SigmaBoolean): HintsBag =
    _prover.generateCommitments(sb)

  /** Generates commitments for a given `ReducedTransaction` using the prover's secret keys.
    *
    * @param reducedTx reduced transaction to generate commitments
    * @return a secrete and public hints for each input of the transaction
    */
  def generateCommitments(reducedTx: ReducedTransaction): TransactionHintsBag = {
    val publicKeys: Seq[SigmaBoolean] = secrets.map(_.publicImage)
    reducedTx.ergoTx.reducedInputs
      .zipWithIndex
      .foldLeft(TransactionHintsBag.empty) { case (bag, (input, idx)) =>
        val hints = _prover.generateCommitmentsFor(input.reductionResult.value, publicKeys)
        bag.addHintsForInput(idx, hints)
      }
  }

}
