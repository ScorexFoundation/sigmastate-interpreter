package sigmastate.eval

import org.ergoplatform.{ErgoBox, ErgoLikeTransaction, ErgoLikeContext}
import sigmastate.{SInt, AvlTreeData}
import sigmastate.Values.{LongConstant, FalseLeaf, TrueLeaf, SigmaPropConstant, IntConstant, BooleanConstant}
import sigmastate.helpers.ErgoLikeProvingInterpreter

import scalan.BaseCtxTests
import sigmastate.lang.{LangTests, TransformingSigmaBuilder}
import sigmastate.utxo.CostTable
import special.sigma.{Box, ContractsTestkit, AnyValue, Context, TestBox, TestContext => TContext}

import scala.collection.mutable
import scala.util.Success

class EvaluationTest extends BaseCtxTests with LangTests with ContractsTestkit with ExampleContracts {
  lazy val IR = new TestContext with Evaluation {
    import TestSigmaDslBuilder._
    import ConcreteCostedBuilder._
    import MonoidBuilderInst._
    val sigmaDslBuilder = RTestSigmaDslBuilder()
    val builder = TransformingSigmaBuilder
    beginPass(new DefaultPass("mypass",
      Pass.defaultPassConfig.copy(constantPropagation = false)))

    val sigmaDslBuilderValue = new special.sigma.TestSigmaDslBuilder()
    val costedBuilderValue = new special.collection.ConcreteCostedBuilder()
    val monoidBuilderValue = new special.collection.MonoidBuilderInst()

    def getDataEnv: mutable.Map[Sym, AnyRef] = {

      val env = mutable.Map[Sym, AnyRef](
        sigmaDslBuilder -> sigmaDslBuilderValue,
        sigmaDslBuilder.Cols -> sigmaDslBuilderValue.Cols,
        costedBuilder -> costedBuilderValue,
        costedBuilder.monoidBuilder -> monoidBuilderValue,
        costedBuilder.monoidBuilder.intPlusMonoid -> monoidBuilderValue.intPlusMonoid
      )
      env
    }
  }

  val noEnv: Map[String, Any] = Map()
  val AliceId = Array[Byte](1) // 0x0001
  def newAliceBox(id: Byte, value: Long) = new TestBox(Array[Byte](0, id), value, Cols.fromArray(AliceId), noRegisters)

  def newContext(height: Long, self: Box, vars: AnyValue*): TContext = {
    new TContext(noInputs, noOutputs, height, self, vars.toArray)
  }
  implicit class TContextOps(ctx: TContext) {
    def withInputs(inputs: Box*) = new TContext(inputs.toArray, ctx.outputs, ctx.height, ctx.selfBox, ctx.vars)
    def withOutputs(outputs: Box*) = new TContext(ctx.inputs, outputs.toArray, ctx.height, ctx.selfBox, ctx.vars)
  }

  val boxA1 = newAliceBox(1, 100)
  val boxA2 = newAliceBox(2, 200)

  implicit class ErgoBoxOps(ebox: ErgoBox) {
    def toTestBox: Box = {
      val rs = regs(ebox.additionalRegisters.map { case (k,v) => (k.number -> v) })
      new TestBox(ebox.id, ebox.value, Cols.fromArray(ebox.propositionBytes), rs)
    }
  }

  implicit class ErgoLikeContextOps(ergoCtx: ErgoLikeContext) {
    def toTestContext: TContext = {
      val inputs = ergoCtx.boxesToSpend.toArray.map(_.toTestBox)
      val outputs = ergoCtx.spendingTransaction.outputs.toArray.map(_.toTestBox)
      val vars = contextVars(ergoCtx.extension.values)
      new TContext(inputs, outputs, ergoCtx.currentHeight, ergoCtx.self.toTestBox, vars.arr)
    }
  }

  def check(env: Map[String, Any], name: String, script: String, ctx: Context, expected: Any): Unit = {
    import IR._
    val costed = cost(env, script)
    val Pair(valueF, costF) = split(costed)
    emit(name, valueF, costF)
    verifyCostFunc(costF) shouldBe(Success(()))
    verifyIsValid(valueF) shouldBe(Success(()))
    val costFun = IR.compile[SInt.type](getDataEnv, costF)
    val IntConstant(estimatedCost) = costFun(ctx)
    (estimatedCost < CostTable.ScriptLimit) shouldBe true
    val valueFun = IR.compile(getDataEnv, valueF)
    valueFun(ctx) shouldBe expected
  }

  test("costed constants") {
    val ctx = newContext(height = 1, boxA1)

    check(noEnv, "one", "1", ctx,  IntConstant(1))
    check(noEnv, "oneL", "1L", ctx, LongConstant(1L))
  }

  test("costed operations") {
    val ctx = newContext(height = 1, boxA1)
    check(noEnv, "one+one", "1 + 1", ctx, IntConstant(2))
    check(noEnv, "oneL+oneL", "1L - 1L", ctx, LongConstant(0))
    check(noEnv, "one_gt_one", "1 > 1", ctx, FalseLeaf)
    check(noEnv, "or", "1 > 1 || 2 < 1", ctx, FalseLeaf)
    check(noEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", ctx, TrueLeaf)
    check(noEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1", ctx, TrueLeaf)
    check(noEnv, "and", "1 > 1 && 2 < 1", ctx, FalseLeaf)
    check(noEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", ctx, FalseLeaf)
    check(noEnv, "and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1", ctx, FalseLeaf)
  }

  test("costed context data") {
    val ctx = newContext(height = 100, boxA1)
        .withInputs(boxA1)
        .withOutputs(boxA2)
    check(noEnv, "height1", "HEIGHT + 1L", ctx, LongConstant(101))
    check(noEnv, "height2", "HEIGHT > 1L", ctx, TrueLeaf)
    check(noEnv, "size", "INPUTS.size + OUTPUTS.size", ctx, IntConstant(2))
    check(noEnv, "value", "SELF.value + 1L", ctx, LongConstant(101))
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
    check(envCF, "CrowdFunding", crowdFundingScript, ctx, FalseLeaf)

    val boxToSpend = ErgoBox(10, TrueLeaf)
    val tx1Output1 = ErgoBox(minToRaise, projectPubKey)
    val tx1Output2 = ErgoBox(1, projectPubKey)
    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
    val ergoCtx = ErgoLikeContext(
      currentHeight = timeout - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = boxToSpend)
    check(envCF, "CrowdFunding", crowdFundingScript, ergoCtx.toTestContext, TrueLeaf)
  }
}
