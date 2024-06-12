package org.ergoplatform.sdk

import org.ergoplatform.sdk.wallet.protocol.context.{BlockchainStateContext, TransactionContext}
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey, SecretKey}
import sigma.data.SigmaBoolean
import sigma.util.Extensions.EcpOps

class Wallet(prover: SigmaProver) {

  /** Generates commitments for a given `UnreducedTransaction` using the wallets's secret
    * keys and the provided [[BlockchainStateContext]].
    *
    * It first reduces the transaction and then generates commitments hints for each input.
    *
    * @param tx       transaction to reduce and generate commitments
    * @param stateCtx blockchain state context
    * @return a secrete and public hints for each input of the transaction
    */
  def generateCommitments(
    tx: UnreducedTransaction,
    stateCtx: BlockchainStateContext
  ): TransactionHintsBag = {
    val reducedTx = prover.reduce(stateCtx, tx, baseCost = 0 /* doesn't matter in this method */)
    generateCommitments(reducedTx)
  }

  /** Generates commitments for a given `ReducedTransaction` using the wallets's secret keys.
    *
    * @param reducedTx reduced transaction to generate commitments
    * @return a secrete and public hints for each input of the transaction
    */
  def generateCommitments(reducedTx: ReducedTransaction): TransactionHintsBag = {
    prover.generateCommitments(reducedTx)
  }

  def signTransaction(
    tx: UnreducedTransaction,
    stateCtx: BlockchainStateContext,
    hints: Option[TransactionHintsBag]
  ): SignedTransaction = {
    val reducedTx = prover.reduce(stateCtx, tx, baseCost = 0 /* doesn't matter in this method */)
    signReducedTransaction(reducedTx, hints)
  }

  /** Reduces a given `UnreducedTransaction` using the wallet's secret keys and the
    * provided [[BlockchainStateContext]] with a base cost.
    */
  def reduce(
    stateCtx: BlockchainStateContext,
    tx: UnreducedTransaction,
    baseCost: Int
  ): ReducedTransaction = {
    prover.reduce(stateCtx, tx, baseCost)
  }

  def signReducedTransaction(
    reducedTx: ReducedTransaction,
    hints: Option[TransactionHintsBag]
  ): SignedTransaction = {
    prover.signReduced(reducedTx, hints)
  }
}

object Wallet {
  /** Creates a new wallet with a prover initialized with the specified mnemonic phrase and password.
    *
    * @param parameters   blockchain parameters from network `/info` API endpoint
    * @param mnemonic     secret mnemonic phrase
    * @param mnemonicPass secret mnemonic password
    * @return a new wallet instance
    */
  def fromMnemonic(
    network: NetworkType,
    parameters: BlockchainParameters,
    mnemonic: SecretString,
    mnemonicPass: SecretString
  ): Wallet = {
    val builder = new ProverBuilder(parameters, network.networkPrefix)
      .withMnemonic(mnemonic, mnemonicPass)
    val prover  = builder.build()
    new Wallet(prover)
  }

  def fromSecrets(
    network: NetworkType,
    parameters: BlockchainParameters,
    secrets: Seq[SecretKey]
  ): Wallet = {
    val builder = new ProverBuilder(parameters, network.networkPrefix)
    secrets.foreach {
      case DhtSecretKey(dhtInput) =>
        builder.withDHTData(
          dhtInput.commonInput.g.toGroupElement,
          dhtInput.commonInput.h.toGroupElement,
          dhtInput.commonInput.u.toGroupElement,
          dhtInput.commonInput.v.toGroupElement,
          dhtInput.w
        )
      case DlogSecretKey(dlogInput) =>
        builder.withDLogSecret(dlogInput.w)
    }
    val prover = builder.build()
    new Wallet(prover)
  }
}
