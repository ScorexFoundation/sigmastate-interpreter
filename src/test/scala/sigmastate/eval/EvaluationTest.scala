package sigmastate.eval

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox}
import sigmastate.{SInt, AvlTreeData}
import sigmastate.Values.{LongConstant, FalseLeaf, TrueLeaf, SigmaPropConstant, IntConstant, BooleanConstant}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.ContextExtension

import scalan.BaseCtxTests
import sigmastate.lang.{LangTests, TransformingSigmaBuilder}
import sigmastate.utxo.CostTable
import special.sigma.{Box, ContractsTestkit, AnyValue, Context, TestBox, TestContext => DContext}

import scala.collection.mutable
import scala.util.Success


class EvaluationTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(noEnv, "one", "1", ctx,  1)
    reduce(noEnv, "oneL", "1L", ctx, 1L)
  }

  test("operations") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(noEnv, "one+one", "1 + 1", ctx, 2)
    reduce(noEnv, "oneL+oneL", "1L - 1L", ctx, 0L)
    reduce(noEnv, "one_gt_one", "1 > 1", ctx, false)
    reduce(noEnv, "or", "1 > 1 || 2 < 1", ctx, false)
    reduce(noEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", ctx, true)
    reduce(noEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1", ctx, true)
    reduce(noEnv, "and", "1 > 1 && 2 < 1", ctx, false)
    reduce(noEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", ctx, false)
    reduce(noEnv, "and3", "1 == 1 && (2 < 1 || 2 > 1)", ctx, true)
    reduce(noEnv, "and4", "OUTPUTS.size > 1 && OUTPUTS.size < 1", ctx, false)
  }

  test("lazy logical ops") {
    val prover = new ErgoLikeProvingInterpreter
    val pk = prover.dlogSecrets.head.publicImage
    val self = ErgoBox(1, pk, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(10)))
    val ctx = newErgoContext(height = 1, self)
    // guarded register access: existing reg
    reduce(noEnv, "lazy1", "SELF.R4[Int].isDefined && SELF.R4[Int].value == 10", ctx, true)
    // guarded register access: non-existing reg
    reduce(noEnv, "lazy2", "SELF.R5[Int].isDefined && SELF.R5[Int].value == 10", ctx, false)

    // guarded register access: reading register if it is defined and another one is undefined
    reduce(noEnv, "lazy3", "SELF.R4[Int].isDefined && (SELF.R5[Int].isDefined || SELF.R4[Int].value == 10)", ctx, true)
  }

  test("context data") {
    val ctx = newErgoContext(height = 100, boxToSpend)
        .withTransaction(tx1)
    reduce(noEnv, "height1", "HEIGHT + 1L", ctx, 101)
    reduce(noEnv, "height2", "HEIGHT > 1L", ctx, true)
    reduce(noEnv, "size", "INPUTS.size + OUTPUTS.size", ctx, 2)
    reduce(noEnv, "value", "SELF.value + 1L", ctx, 11L)
  }

  test("lambdas") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(noEnv, "lam3", "{let f = fun (out: Box) = { out.value >= 0L }; f(SELF) }", ctx, true)

    // access R5 and call g only if f returns false
    reduce(noEnv, "lam4",
      """{
       |  let f = fun (out: Box) = { out.value >= 0L };
       |  let g = fun (x: Int) = { x < 0 };
       |  f(SELF) || g(SELF.R5[Int].value)
       | }""".stripMargin, ctx, true)
       
//    reduce(noEnv, "lam5",
//      """{
//       |  let f = fun (out: Box) = { out.value >= 0L };
//       |  let g = fun (xs: Array[Int]) = { xs.size > 0 };
//       |  f(SELF) || g(SELF.R5[Array[Int]].value)
//       | }""".stripMargin, ctx, true)
  }

//  test("Crowd Funding") {
//    val backerProver = new ErgoLikeProvingInterpreter
//    val projectProver = new ErgoLikeProvingInterpreter
//    val backerPubKey = backerProver.dlogSecrets.head.publicImage
//    val projectPubKey = projectProver.dlogSecrets.head.publicImage
//    val ctxVars = contextVars(Map(
//      backerPubKeyId -> backerPubKey,
//      projectPubKeyId -> projectPubKey
//    )).arr
//    val ctx = newContext(height = 1, boxA1, ctxVars:_*)
//    reduce(envCF, "CrowdFunding", crowdFundingScript, ctx, FalseLeaf)
//
//    val boxToSpend = ErgoBox(10, TrueLeaf)
//    val tx1Output1 = ErgoBox(minToRaise, projectPubKey)
//    val tx1Output2 = ErgoBox(1, projectPubKey)
//    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
//    val ergoCtx = ErgoLikeContext(
//      currentHeight = timeout - 1,
//      lastBlockUtxoRoot = AvlTreeData.dummy,
//      boxesToSpend = IndexedSeq(),
//      spendingTransaction = tx1,
//      self = boxToSpend,
//      extension = ContextExtension(Map(
//        backerPubKeyId -> SigmaPropConstant(backerPubKey),
//        projectPubKeyId -> SigmaPropConstant(projectPubKey)
//      )))
//    reduce(envCF, "CrowdFunding", crowdFundingScript, ergoCtx.toTestContext, projectPubKey)
//  }
}
