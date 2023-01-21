package org.ergoplatform.sdk

import debox.cfor
import org.ergoplatform._
import org.ergoplatform.sdk.JavaHelpers.TokenColl
import org.ergoplatform.sdk.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext}
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import scalan.util.Extensions.LongOps
import sigmastate.Values.SigmaBoolean
import sigmastate.VersionContext
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import sigmastate.eval.Evaluation.addCostChecked
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.interpreter.{ContextExtension, CostedProverResult, HintsBag, ProverInterpreter}
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable
import scala.util.Try

/**
 * A class which holds secrets and can sign transactions (aka generate proofs).
 *
 * @param secretKeys secrets in extended form to be used by prover
 * @param dhtInputs  prover inputs containing secrets for generating proofs for ProveDHTuple nodes.
 * @param params     ergo blockchain parameters
 */
class AppkitProvingInterpreter(
      val secretKeys: IndexedSeq[ExtendedSecretKey],
      val dLogInputs: IndexedSeq[DLogProverInput],
      val dhtInputs: IndexedSeq[DiffieHellmanTupleProverInput],
      params: ErgoLikeParameters)
  extends ReducingInterpreter(params) with ProverInterpreter {

  val secrets: Seq[SigmaProtocolPrivateInput[_, _]] = {
    val dlogs: IndexedSeq[DLogProverInput] = secretKeys.map(_.privateInput)
    dlogs ++ dLogInputs ++ dhtInputs
  }

  val pubKeys: Seq[ProveDlog] = secrets
    .filter { case _: DLogProverInput => true case _ => false}
    .map(_.asInstanceOf[DLogProverInput].publicImage)

  /** Reduces and signs the given transaction.
   *
   * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
   * @param boxesToSpend input boxes of the transaction
   * @param dataBoxes    data inputs of the transaction
   * @param stateContext state context of the blockchain in which the transaction should be signed
   * @param baseCost     the cost accumulated before this transaction
   * @param tokensToBurn requested tokens to be burnt in the transaction, if empty no burning allowed
   * @return a new signed transaction with all inputs signed and the cost of this transaction
   *         The returned cost doesn't include `baseCost`.
   */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ExtendedInputBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: ErgoLikeStateContext,
           baseCost: Int,
           tokensToBurn: IndexedSeq[ErgoToken]): Try[(ErgoLikeTransaction, Int)] = Try {
    val maxCost = params.maxBlockCost
    var currentCost: Long = baseCost

    val (reducedTx, txCost) = reduceTransaction(unsignedTx, boxesToSpend, dataBoxes, stateContext, baseCost, tokensToBurn)
    currentCost = addCostChecked(currentCost, txCost, maxCost, msgSuffix = reducedTx.toString())

    val (signedTx, cost) = signReduced(reducedTx, currentCost.toInt)
    (signedTx, txCost + cost)
  }

  /** Signs the given transaction (i.e. providing spending proofs) for each input so that
   * the resulting transaction can be submitted to the blockchain.
   * Note, this method doesn't require context to generate proofs (aka signatures).
   *
   * @param reducedTx unsigend transaction augmented with reduced
   * @param baseCost the cost accumulated so far and before this operation
   * @return a new signed transaction with all inputs signed and the cost of this transaction
   *         The returned cost includes all the costs of the reduced inputs, but not baseCost
   */
  def signReduced(
          reducedTx: ReducedErgoLikeTransaction,
          baseCost: Int): (ErgoLikeTransaction, Int) = {
    val provedInputs = mutable.ArrayBuilder.make[Input]
    val unsignedTx = reducedTx.unsignedTx

    val maxCost = params.maxBlockCost
    var currentCost: Long = baseCost

    for ((reducedInput, boxIdx) <- reducedTx.reducedInputs.zipWithIndex ) {
      val unsignedInput = unsignedTx.inputs(boxIdx)

      val proverResult = proveReduced(reducedInput, unsignedTx.messageToSign)
      val signedInput = Input(unsignedInput.boxId, proverResult)

      currentCost = addCostChecked(currentCost, proverResult.cost, maxCost, msgSuffix = signedInput.toString())

      provedInputs += signedInput
    }

    val signedTx = new ErgoLikeTransaction(
      provedInputs.result(), unsignedTx.dataInputs, unsignedTx.outputCandidates)
    val txCost = currentCost.toInt - baseCost
    (signedTx, txCost)
  }

  // TODO pull this method up to the base class and reuse in `prove`
  /** Generates proof (aka signature) for the given message using secrets of this prover.
    * All the necessary secrets should be configured in this prover to satisfy the given
    * sigma proposition in the reducedInput.
    */
  def proveReduced(
        reducedInput: ReducedInputData,
        message: Array[Byte],
        hintsBag: HintsBag = HintsBag.empty): CostedProverResult = {
    val proof = generateProof(reducedInput.reductionResult.value, message, hintsBag)
    CostedProverResult(proof, reducedInput.extension, reducedInput.reductionResult.cost)
  }

}

/** Thrown during transaction signing when inputs token are not balanced with output tokens.
  * @param tokensDiff balance difference which caused the error
  */
case class TokenBalanceException(
  message: String,
  tokensDiff: TokenColl
) extends Exception(s"Input and output tokens are not balanced: $message")

/** Represents data necessary to sign an input of an unsigned transaction.
  * @param reductionResult result of reducing input script to a sigma proposition
  * @param extension context extensions (aka context variables) used by script and which
  *                  are also necessary to verify the transaction on-chain. Extensions are
  *                  included in tx bytes, which are signed.
  */
case class ReducedInputData(reductionResult: ReductionResult, extension: ContextExtension)

/** Represent `reduced` transaction, i.e. unsigned transaction where each unsigned input
  * is augmented with [[ReducedInputData]] which contains a script reduction result.
  * After an unsigned transaction is reduced it can be signed without context.
  * Thus, it can be serialized and transferred for example to Cold Wallet and signed
  * in an environment where secrets are known.
  */
case class ReducedErgoLikeTransaction(
  unsignedTx: UnsignedErgoLikeTransaction,
  reducedInputs: Seq[ReducedInputData]
) {
  require(unsignedTx.inputs.length == reducedInputs.length)
}

/** HOTSPOT: don't beautify the code */
object ReducedErgoLikeTransactionSerializer extends SigmaSerializer[ReducedErgoLikeTransaction, ReducedErgoLikeTransaction] {

  override def serialize(tx: ReducedErgoLikeTransaction, w: SigmaByteWriter): Unit = {
    val msg = tx.unsignedTx.messageToSign
    w.putUInt(msg.length)  // size of the tx bytes to restore tx reliably
    w.putBytes(msg)

    // serialize sigma propositions for each input
    val nInputs = tx.reducedInputs.length
    // no need to save nInputs because it is known from unsignedTx.inputs
    cfor(0)(_ < nInputs, _ + 1) { i =>
      val input = tx.reducedInputs(i)
      SigmaBoolean.serializer.serialize(input.reductionResult.value, w)
      w.putULong(input.reductionResult.cost)
      // Note, we don't need to save `extension` field because it has already
      // been saved in msg
    }
  }

  override def parse(r: SigmaByteReader): ReducedErgoLikeTransaction = {
    val nBytes = r.getUInt()
    val msg = r.getBytes(nBytes.toIntExact)

    // here we read ErgoLikeTransaction which is used below as raw data for
    // the new UnsignedErgoLikeTransaction
    val tx = ErgoLikeTransactionSerializer.parse(SigmaSerializer.startReader(msg))

    // serialize sigma propositions for each input
    val nInputs = tx.inputs.length
    val reducedInputs = new Array[ReducedInputData](nInputs)
    val unsignedInputs = new Array[UnsignedInput](nInputs)
    cfor(0)(_ < nInputs, _ + 1) { i =>
      val sb = SigmaBoolean.serializer.parse(r)
      val cost = r.getULong()
      val input = tx.inputs(i)
      val extension = input.extension
      val reductionResult = ReductionResult(sb, cost)
      reducedInputs(i) = ReducedInputData(reductionResult, extension)
      unsignedInputs(i) = new UnsignedInput(input.boxId, extension)
    }

    val unsignedTx = UnsignedErgoLikeTransaction(unsignedInputs, tx.dataInputs, tx.outputCandidates)
    ReducedErgoLikeTransaction(unsignedTx, reducedInputs)
  }

  /** Parses the [[ReducedErgoLikeTransaction]] using the given blockVersion.
    * @param blockVersion version of Ergo protocol to use during parsing.
    */
  def parse(r: SigmaByteReader, blockVersion: Byte): ReducedErgoLikeTransaction = {
    val scriptVersion = (blockVersion - 1).toByte
    VersionContext.withVersions(scriptVersion, scriptVersion) {
      parse(r)
    }
  }
}

