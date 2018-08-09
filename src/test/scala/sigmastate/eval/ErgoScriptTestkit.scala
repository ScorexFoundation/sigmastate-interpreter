package sigmastate.eval

import scala.collection.mutable
import scala.util.Success
import sigmastate.{SInt, AvlTreeData}
import sigmastate.Values.{SValue, LongConstant, FalseLeaf, TrueLeaf, SigmaPropConstant, IntConstant, BooleanConstant}
import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import sigmastate.utxo.CostTable
import special.sigma.{TestBox, ContractsTestkit, AnyValue, Box, Context, TestContext => TContext}
import scalan.BaseCtxTests
import sigmastate.lang.{LangTests, TransformingSigmaBuilder}

trait ErgoScriptTestkit extends ContractsTestkit { self: BaseCtxTests =>

  lazy val IR = new TestContext with Evaluation {
    import TestSigmaDslBuilder._

    val sigmaDslBuilder = RTestSigmaDslBuilder()
    val builder = TransformingSigmaBuilder
    beginPass(new DefaultPass("mypass",
      Pass.defaultPassConfig.copy(constantPropagation = false)))

    val sigmaDslBuilderValue = new special.sigma.TestSigmaDslBuilder()
    val costedBuilderValue = new special.collection.ConcreteCostedBuilder()
    val monoidBuilderValue = new special.collection.MonoidBuilderInst()
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

  def reduce(env: Map[String, Any], name: String, script: String, ctx: Context, expected: Any): Unit = {
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

  def build(env: Map[String, Any], name: String, script: String, expected: SValue): Unit = {
    import IR._
    val costed = cost(env, script)
    val Pair(valueF, costF) = split(costed)
    emit(name, valueF, costF)
    verifyCostFunc(costF) shouldBe(Success(()))
    verifyIsValid(valueF) shouldBe(Success(()))
    IR.buildTree(valueF) shouldBe expected
  }

}
