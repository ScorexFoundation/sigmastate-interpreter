package org.ergoplatform.sdk

import org.ergoplatform.validation.ValidationRules
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}

import java.util
import java.util.{Objects, List => JList}
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import org.ergoplatform._
import org.ergoplatform.sdk.JavaHelpers.{TokenColl, UniversalConverter}
import org.ergoplatform.sdk.ReducedInputData.createReductionResult
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import sigmastate.Values.{ErgoTree, SigmaBoolean}

import scala.util.Try
import sigmastate.interpreter.Interpreter.{ReductionResult, ScriptEnv}
import sigmastate.interpreter.{ContextExtension, CostedProverResult, HintsBag, Interpreter, ProverInterpreter}
import sigmastate.exceptions.CostLimitException
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import debox.cfor
import org.ergoplatform.sdk.Extensions.{CollOps, PairCollOps}
import org.ergoplatform.sdk.utils.ArithUtils
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import scalan.util.Extensions.LongOps
import sigmastate.{AvlTreeData, VersionContext}

import scala.collection.{IndexedSeq, mutable}

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
  extends ErgoLikeInterpreter with ProverInterpreter {

  override type CTX = ErgoLikeContext
  import org.ergoplatform.sdk.Iso._

  val secrets: Seq[SigmaProtocolPrivateInput[_ <: SigmaProtocol[_], _ <: SigmaProtocolCommonInput[_]]] = {
    val dlogs: IndexedSeq[DLogProverInput] = secretKeys.map(_.privateInput)
    dlogs ++ dLogInputs ++ dhtInputs
  }

  val pubKeys: Seq[ProveDlog] = secrets
    .filter { case _: DLogProverInput => true case _ => false}
    .map(_.asInstanceOf[DLogProverInput].publicImage)

  def addCostLimited(currentCost: Long, delta: Long, limit: Long, msg: => String): Long = {
    val newCost = java7.compat.Math.addExact(currentCost, delta)
    if (newCost > limit)
      throw new Exception(s"Cost of transaction $newCost exceeds limit $limit: $msg")
    newCost
  }

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
    currentCost = addCostLimited(currentCost, txCost, maxCost, msg = reducedTx.toString())

    val (signedTx, cost) = signReduced(reducedTx, currentCost.toInt)
    (signedTx, txCost + cost)
  }

  /** Reduce inputs of the given unsigned transaction to provable sigma propositions using
   * the given context. See [[ReducedErgoLikeTransaction]] for details.
   *
   * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
   * @param boxesToSpend input boxes of the transaction
   * @param dataBoxes    data inputs of the transaction
   * @param stateContext state context of the blockchain in which the transaction should be signed
   * @param baseCost the cost accumulated so far and before this operation
   * @param tokensToBurn requested tokens to be burnt in the transaction, if empty no burning allowed
   * @return a new reduced transaction with all inputs reduced and the cost of this transaction
   *         The returned cost doesn't include (so they need to be added back to get the total cost):
   *         1) `baseCost`
   *         2) reduction cost for each input.
   */
  def reduceTransaction(
        unsignedTx: UnsignedErgoLikeTransaction,
        boxesToSpend: IndexedSeq[ExtendedInputBox],
        dataBoxes: IndexedSeq[ErgoBox],
        stateContext: ErgoLikeStateContext,
        baseCost: Int,
        tokensToBurn: IndexedSeq[ErgoToken]): (ReducedErgoLikeTransaction, Int) = {
    if (unsignedTx.inputs.length != boxesToSpend.length) throw new Exception("Not enough boxes to spend")
    if (unsignedTx.dataInputs.length != dataBoxes.length) throw new Exception("Not enough data boxes")

    val inputTokens = boxesToSpend.flatMap(_.box.additionalTokens.toArray)
    val outputTokens = unsignedTx.outputCandidates.flatMap(_.additionalTokens.toArray)
    val tokenDiff = JavaHelpers.subtractTokens(outputTokens, inputTokens)
    if (tokenDiff.nonEmpty) {
      val (toBurn, toMint) = tokenDiff.partition(_._2 < 0)  // those with negative diff are to be burnt
      if (toBurn.nonEmpty) {
        if (!tokensToBurn.isEmpty) {
          val requestedToBurn = isoTokensListToTokenColl.to(tokensToBurn.convertTo[JList[ErgoToken]])
          val diff = JavaHelpers.subtractTokenColls(
            reducedTokens = toBurn.mapSecond(v => -v), // make positive amounts
            subtractedTokens = requestedToBurn
          )
          if (diff.nonEmpty) {  // empty diff would mean equality
            throw TokenBalanceException(
              "Transaction tries to burn tokens, but not how it was requested", diff)
          }
        } else {
            throw TokenBalanceException(
              "Transaction tries to burn tokens when no burning was requested", tokenDiff)
        }
      }
      if (toMint.nonEmpty) {
        if (toMint.length > 1) {
          throw TokenBalanceException("Only one token can be minted in a transaction", toMint)
        }
        val isCorrectMintedTokenId = Objects.deepEquals(toMint(0)._1.toArray, boxesToSpend.head.box.id)
        if (!isCorrectMintedTokenId) {
          throw TokenBalanceException("Cannot mint a token with invalid id", toMint)
        }
      }
    }

    // Cost of transaction initialization: we should read and parse all inputs and data inputs,
    // and also iterate through all outputs to check rules
    val initialCost = ArithUtils.addExact(
      1000,
      java7.compat.Math.multiplyExact(boxesToSpend.size, params.inputCost),
      java7.compat.Math.multiplyExact(dataBoxes.size, params.dataInputCost),
      java7.compat.Math.multiplyExact(unsignedTx.outputCandidates.size, params.outputCost)
    )
    val maxCost = params.maxBlockCost
    val startCost = addCostLimited(baseCost, initialCost, maxCost, msg = unsignedTx.toString())

    val transactionContext = TransactionContext(boxesToSpend.map(_.box), dataBoxes, unsignedTx)

    val (outAssets, outAssetsNum) = JavaHelpers.extractAssets(unsignedTx.outputCandidates)
    val (inAssets, inAssetsNum) = JavaHelpers.extractAssets(boxesToSpend.map(_.box))

    val tokenAccessCost = params.tokenAccessCost
    val totalAssetsAccessCost =
      java7.compat.Math.addExact(
        java7.compat.Math.multiplyExact(java7.compat.Math.addExact(outAssetsNum, inAssetsNum), tokenAccessCost),
        java7.compat.Math.multiplyExact(java7.compat.Math.addExact(inAssets.size, outAssets.size), tokenAccessCost))

    val txCost = addCostLimited(startCost,
      delta = totalAssetsAccessCost,
      limit = maxCost, msg = s"when adding assets cost of $totalAssetsAccessCost")

    var currentCost = txCost
    val reducedInputs = mutable.ArrayBuilder.make[ReducedInputData]()

    for ((inputBox, boxIdx) <- boxesToSpend.zipWithIndex) {
      val unsignedInput = unsignedTx.inputs(boxIdx)
      require(util.Arrays.equals(unsignedInput.boxId, inputBox.box.id))

      val context = new ErgoLikeContext(
        AvlTreeData.avlTreeFromDigest(stateContext.previousStateDigest),
        stateContext.sigmaLastHeaders,
        stateContext.sigmaPreHeader,
        transactionContext.dataBoxes,
        transactionContext.boxesToSpend,
        transactionContext.spendingTransaction,
        boxIdx.toShort,
        inputBox.extension,
        ValidationRules.currentSettings,
        costLimit = maxCost - currentCost,
        initCost = 0,
        activatedScriptVersion = (params.blockVersion - 1).toByte
      )

      val reducedInput = reduce(Interpreter.emptyEnv, inputBox.box.ergoTree, context)

      currentCost = addCostLimited(currentCost,
        reducedInput.reductionResult.cost, limit = maxCost, msg = inputBox.toString())

      reducedInputs += reducedInput
    }

    val reducedTx = ReducedErgoLikeTransaction(unsignedTx, reducedInputs.result())
    val txReductionCost = txCost.toInt - baseCost
    (reducedTx, txReductionCost)
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
    val provedInputs = mutable.ArrayBuilder.make[Input]()
    val unsignedTx = reducedTx.unsignedTx

    val maxCost = params.maxBlockCost
    var currentCost: Long = baseCost

    for ((reducedInput, boxIdx) <- reducedTx.reducedInputs.zipWithIndex ) {
      val unsignedInput = unsignedTx.inputs(boxIdx)

      val proverResult = proveReduced(reducedInput, unsignedTx.messageToSign)
      val signedInput = Input(unsignedInput.boxId, proverResult)

      currentCost = addCostLimited(currentCost, proverResult.cost, maxCost, msg = signedInput.toString())

      provedInputs += signedInput
    }

    val signedTx = new ErgoLikeTransaction(
      provedInputs.result(), unsignedTx.dataInputs, unsignedTx.outputCandidates)
    val txCost = currentCost.toInt - baseCost
    (signedTx, txCost)
  }

  // TODO pull this method up to the base class and reuse in `prove`
  /** Reduces the given ErgoTree in the given context to the sigma proposition.
   *
   * @param env      script environment (use Interpreter.emptyEnv as default)
   * @param ergoTree input ErgoTree expression to reduce
   * @param context  context used in reduction
   * @return data object containing enough data to sign a transaction without Context.
   */
  def reduce(env: ScriptEnv,
            ergoTree: ErgoTree,
            context: CTX): ReducedInputData = {
    val initCost = ergoTree.complexity + context.initCost
    val remainingLimit = context.costLimit - initCost
    if (remainingLimit <= 0)
      throw new CostLimitException(initCost,
        s"Estimated execution cost $initCost exceeds the limit ${context.costLimit}", None)

    val ctxUpdInitCost = context.withInitCost(initCost).asInstanceOf[CTX]

    val res = fullReduction(ergoTree, ctxUpdInitCost, env)
    ReducedInputData(res, ctxUpdInitCost.extension)
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

/** Represents data necessary to sign an input of an unsigend transaction.
  * @param reductionResult result of reducing input script to a sigma proposition
  * @param extension context extensions (aka context variables) used by script and which
  *                  are also necessary to verify the transaction on-chain. Extensions are
  *                  included in tx bytes, which are signed.
  */
case class ReducedInputData(reductionResult: ReductionResult, extension: ContextExtension)

object ReducedInputData {
  /** Creates [[ReductionResult]] for the given blockVersion.
    *
    * @param blockVersion version of Ergo protocol (stored in block header)
    * @param sb           sigma proposition (typically result of script reduction)
    * @param cost         cost accumulated during reduction
    */
  def createReductionResult(blockVersion: Byte, sb: SigmaBoolean, cost: Long): ReductionResult = {
    val scriptVersion = blockVersion - 1 // convert to script version
    ReductionResult(sb, cost)
  }
}

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

