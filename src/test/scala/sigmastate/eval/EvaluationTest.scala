package sigmastate.eval

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox}
import sigmastate.{SInt, AvlTreeData}
import sigmastate.Values.{LongConstant, FalseLeaf, TrueLeaf, SigmaPropConstant, IntConstant, BooleanConstant}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.ContextExtension

import scalan.BaseCtxTests
import sigmastate.lang.{LangTests, TransformingSigmaBuilder}
import sigmastate.utxo.CostTable
import special.sigma.{Box, ContractsTestkit, AnyValue, Context, TestBox, TestContext => TContext}

import scala.collection.mutable
import scala.util.Success

class EvaluationTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    val ctx = newContext(height = 1, boxA1)

    reduce(noEnv, "one", "1", ctx,  IntConstant(1))
    reduce(noEnv, "oneL", "1L", ctx, LongConstant(1L))
  }

  test("operations") {
    val ctx = newContext(height = 1, boxA1)
    reduce(noEnv, "one+one", "1 + 1", ctx, IntConstant(2))
    reduce(noEnv, "oneL+oneL", "1L - 1L", ctx, LongConstant(0))
    reduce(noEnv, "one_gt_one", "1 > 1", ctx, FalseLeaf)
    reduce(noEnv, "or", "1 > 1 || 2 < 1", ctx, FalseLeaf)
    reduce(noEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", ctx, TrueLeaf)
    reduce(noEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1", ctx, TrueLeaf)
    reduce(noEnv, "and", "1 > 1 && 2 < 1", ctx, FalseLeaf)
    reduce(noEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", ctx, FalseLeaf)
    reduce(noEnv, "and3", "1 == 1 && (2 < 1 || 2 > 1)", ctx, TrueLeaf)
    reduce(noEnv, "and4", "OUTPUTS.size > 1 && OUTPUTS.size < 1", ctx, FalseLeaf)
  }

  test("lazy logical ops") {
    val prover = new ErgoLikeProvingInterpreter
    val pk = prover.dlogSecrets.head.publicImage
    val self = ErgoBox(1, pk, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(10)))
    val ctx = newContext(height = 1, self.toTestBox)
    // guarded register access: existing reg
    reduce(noEnv, "lazy1", "SELF.R4[Int].isDefined && SELF.R4[Int].value == 10", ctx, TrueLeaf)
    // guarded register access: non-existing reg
    reduce(noEnv, "lazy2", "SELF.R5[Int].isDefined && SELF.R5[Int].value == 10", ctx, FalseLeaf)

    // guarded register access: reading register if it is defined and another one is undefined
    reduce(noEnv, "lazy3", "SELF.R4[Int].isDefined && (SELF.R5[Int].isDefined || SELF.R4[Int].value == 10)", ctx, TrueLeaf)
  }

  test("context data") {
    val ctx = newContext(height = 100, boxA1)
        .withInputs(boxA1)
        .withOutputs(boxA2)
    reduce(noEnv, "height1", "HEIGHT + 1L", ctx, LongConstant(101))
    reduce(noEnv, "height2", "HEIGHT > 1L", ctx, TrueLeaf)
    reduce(noEnv, "size", "INPUTS.size + OUTPUTS.size", ctx, IntConstant(2))
    reduce(noEnv, "value", "SELF.value + 1L", ctx, LongConstant(101))
  }

  test("Crowd Funding") {
    val backerProver = new ErgoLikeProvingInterpreter
    val projectProver = new ErgoLikeProvingInterpreter
    val backerPubKey = backerProver.dlogSecrets.head.publicImage
    val projectPubKey = projectProver.dlogSecrets.head.publicImage
    val ctxVars = contextVars(Map(
      backerPubKeyId -> backerPubKey,
      projectPubKeyId -> projectPubKey
    )).arr
    val ctx = newContext(height = 1, boxA1, ctxVars:_*)
    reduce(envCF, "CrowdFunding", crowdFundingScript, ctx, FalseLeaf)

    val boxToSpend = ErgoBox(10, TrueLeaf)
    val tx1Output1 = ErgoBox(minToRaise, projectPubKey)
    val tx1Output2 = ErgoBox(1, projectPubKey)
    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
    val ergoCtx = ErgoLikeContext(
      currentHeight = timeout - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = boxToSpend,
      extension = ContextExtension(Map(
        backerPubKeyId -> SigmaPropConstant(backerPubKey),
        projectPubKeyId -> SigmaPropConstant(projectPubKey)
      )))
    reduce(envCF, "CrowdFunding", crowdFundingScript, ergoCtx.toTestContext, projectPubKey)
  }
}
