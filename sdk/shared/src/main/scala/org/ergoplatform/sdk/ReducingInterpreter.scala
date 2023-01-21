package org.ergoplatform.sdk

import org.ergoplatform.sdk.Extensions.{CollOps, PairCollOps}
import org.ergoplatform.sdk.JavaHelpers.UniversalConverter
import org.ergoplatform.sdk.utils.ArithUtils
import org.ergoplatform.sdk.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, UnsignedErgoLikeTransaction}
import scorex.crypto.authds.ADDigest
import sigmastate.AvlTreeData
import sigmastate.Values.ErgoTree
import sigmastate.eval.Evaluation.addCostChecked
import sigmastate.exceptions.CostLimitException
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter.ScriptEnv

import java.util
import java.util.{Objects, List => JList}
import scala.collection.mutable

/** Interpreter that can reduce transactions with given chain parameters. */
class ReducingInterpreter(params: ErgoLikeParameters) extends ErgoLikeInterpreter {
  override type CTX = ErgoLikeContext
  import org.ergoplatform.sdk.Iso._

  /** Reduces the given ErgoTree in the given context to the sigma proposition.
    *
    * @param env      script environment (use Interpreter.emptyEnv as default)
    * @param ergoTree input ErgoTree expression to reduce
    * @param context  context used in reduction
    * @return data object containing enough data to sign a transaction without Context.
    */
  def reduce(env: ScriptEnv, ergoTree: ErgoTree, context: CTX): ReducedInputData = {
    val initCost = ergoTree.complexity + context.initCost
    val remainingLimit = context.costLimit - initCost
    if (remainingLimit <= 0)
      throw new CostLimitException(initCost,
        s"Estimated execution cost $initCost exceeds the limit ${context.costLimit}", None)
    val ctxUpdInitCost = context.withInitCost(initCost)
    val res = fullReduction(ergoTree, ctxUpdInitCost, env)
    ReducedInputData(res, ctxUpdInitCost.extension)
  }

  /** Reduce inputs of the given unsigned transaction to provable sigma propositions using
    * the given context. See [[ReducedErgoLikeTransaction]] for details.
    *
    * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
    * @param boxesToSpend input boxes of the transaction
    * @param dataBoxes    data inputs of the transaction
    * @param stateContext state context of the blockchain in which the transaction should be signed
    * @param baseCost     the cost accumulated so far and before this operation
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
      val (toBurn, toMint) = tokenDiff.partition(_._2 < 0) // those with negative diff are to be burnt
      if (toBurn.nonEmpty) {
        if (tokensToBurn.nonEmpty) {
          val requestedToBurn = isoTokensListToTokenColl.to(tokensToBurn.convertTo[JList[ErgoToken]])
          val diff = JavaHelpers.subtractTokenColls(
            reducedTokens = toBurn.mapSecond(v => -v), // make positive amounts
            subtractedTokens = requestedToBurn
          )
          if (diff.nonEmpty) { // empty diff would mean equality
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
    val startCost = addCostChecked(baseCost, initialCost, maxCost, msgSuffix = unsignedTx.toString())
    val transactionContext = TransactionContext(boxesToSpend.map(_.box), dataBoxes, unsignedTx)
    val (outAssets, outAssetsNum) = JavaHelpers.extractAssets(unsignedTx.outputCandidates)
    val (inAssets, inAssetsNum) = JavaHelpers.extractAssets(boxesToSpend.map(_.box))
    val tokenAccessCost = params.tokenAccessCost
    val totalAssetsAccessCost =
      java7.compat.Math.addExact(
        java7.compat.Math.multiplyExact(java7.compat.Math.addExact(outAssetsNum, inAssetsNum), tokenAccessCost),
        java7.compat.Math.multiplyExact(java7.compat.Math.addExact(inAssets.size, outAssets.size), tokenAccessCost))
    val txCost = addCostChecked(startCost,
      delta = totalAssetsAccessCost,
      limit = maxCost, msgSuffix = s"when adding assets cost of $totalAssetsAccessCost")
    var currentCost = txCost
    val reducedInputs = mutable.ArrayBuilder.make[ReducedInputData]
    for ( (inputBox, boxIdx) <- boxesToSpend.zipWithIndex ) {
      val unsignedInput = unsignedTx.inputs(boxIdx)
      require(util.Arrays.equals(unsignedInput.boxId, inputBox.box.id))
      val context = new ErgoLikeContext(
        AvlTreeData.avlTreeFromDigest(ADDigest @@@ stateContext.previousStateDigest.toArray),
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
      currentCost = addCostChecked(currentCost,
        reducedInput.reductionResult.cost, limit = maxCost, msgSuffix = inputBox.toString)
      reducedInputs += reducedInput
    }
    val reducedTx = ReducedErgoLikeTransaction(unsignedTx, reducedInputs.result())
    val txReductionCost = txCost.toInt - baseCost
    (reducedTx, txReductionCost)
  }
}
