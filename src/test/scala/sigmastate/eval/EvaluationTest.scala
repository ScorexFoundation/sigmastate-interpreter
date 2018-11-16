package sigmastate.eval

import org.ergoplatform.ErgoBox
import sigmastate.Values.IntConstant
import sigmastate.helpers.ErgoLikeTestProvingInterpreter
import sigmastate.interpreter.Interpreter._
import scalan.BaseCtxTests
import sigmastate.lang.LangTests
import special.sigma.{TestContext => DContext}


class EvaluationTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "one", "1", ctx,  1)
    reduce(emptyEnv, "oneL", "1L", ctx, 1L)
  }

  test("operations") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "one+one", "1 + 1", ctx, 2)
    reduce(emptyEnv, "oneL+oneL", "1L - 1L", ctx, 0L)
    reduce(emptyEnv, "one_gt_one", "1 > 1", ctx, false)
    reduce(emptyEnv, "or", "1 > 1 || 2 < 1", ctx, false)
    reduce(emptyEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", ctx, true)
    reduce(emptyEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1", ctx, true)
    reduce(emptyEnv, "and", "1 > 1 && 2 < 1", ctx, false)
    reduce(emptyEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", ctx, false)
    reduce(emptyEnv, "and3", "1 == 1 && (2 < 1 || 2 > 1)", ctx, true)
    reduce(emptyEnv, "and4", "OUTPUTS.size > 1 && OUTPUTS.size < 1", ctx, false)
  }

  test("lazy logical ops") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk = prover.dlogSecrets.head.publicImage
    val self = ErgoBox(1, pk, 0, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(10)))
    val ctx = newErgoContext(height = 1, self)
    // guarded register access: existing reg
    reduce(emptyEnv, "lazy1", "SELF.R4[Int].isDefined && SELF.R4[Int].get == 10", ctx, true)
    // guarded register access: non-existing reg
    reduce(emptyEnv, "lazy2", "SELF.R5[Int].isDefined && SELF.R5[Int].get == 10", ctx, false)

    // guarded register access: reading register if it is defined and another one is undefined
    reduce(emptyEnv, "lazy3", "SELF.R4[Int].isDefined && (SELF.R5[Int].isDefined || SELF.R4[Int].get == 10)", ctx, true)
  }

  test("context data") {
    val ctx = newErgoContext(height = 100, boxToSpend)
        .withTransaction(tx1)
    reduce(emptyEnv, "height1", "HEIGHT + 1L", ctx, 101)
    reduce(emptyEnv, "height2", "HEIGHT > 1L", ctx, true)
    reduce(emptyEnv, "size", "INPUTS.size + OUTPUTS.size", ctx, 2)
    reduce(emptyEnv, "value", "SELF.value + 1L", ctx, 11L)
  }

  test("lambdas") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "lam3", "{ val f = { (out: Box) => out.value >= 0L }; f(SELF) }", ctx, true)

    // access R5 and call g only if f returns false
    reduce(emptyEnv, "lam4",
      """{
       |  val f = { (out: Box) => out.value >= 0L };
       |  val g = { (x: Int) => x < 0 };
       |  f(SELF) || g(SELF.R5[Int].get)
       | }""".stripMargin, ctx, true)

    reduce(emptyEnv, "lam5",
      """{
       |  val f = { (out: Box) => out.value >= 0L };
       |  val g = { (xs: Col[Int]) => xs.size > 0 };
       |  f(SELF) || g(SELF.R5[Col[Int]].get)
       | }""".stripMargin, ctx, true)
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
