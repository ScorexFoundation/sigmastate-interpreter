package sigmastate.eval

import sigmastate.SInt
import sigmastate.Values.{IntConstant, LongConstant}

import scalan.BaseCtxTests
import sigmastate.lang.{LangTests, TransformingSigmaBuilder}
import sigmastate.utxo.CostTable
import special.sigma.{Box, ContractsTestkit, AnyValue, Context, TestBox, TestContext => TContext}

import scala.util.Success

class EvaluationTest extends BaseCtxTests with LangTests with ContractsTestkit with ExampleContracts {
  lazy val IR = new TestContext with Evaluation {
    import TestSigmaDslBuilder._
    val sigmaDslBuilder = RTestSigmaDslBuilder()
    val builder = TransformingSigmaBuilder
  }

  val noEnv: Map[String, Any] = Map()
  val AliceId = Array[Byte](1) // 0x0001
  def newAliceBox(id: Byte, value: Long) = new TestBox(Array[Byte](0, id), value, Cols.fromArray(AliceId), noRegisters)

  def newContext(height: Long, self: Box, vars: AnyValue*): TContext = {
    new TContext(noInputs, noOutputs, height, self, vars.toArray)
  }

  val boxA1 = newAliceBox(1, 100)
  val boxA2 = newAliceBox(2, 200)

  def check(env: Map[String, Any], name: String, script: String, ctx: Context, expected: Any): Unit = {
    import IR._
    val costed = cost(env, script)
    val Pair(valueF, costF) = split(costed)
    emit(name, valueF, costF)
    verifyCostFunc(costF) shouldBe(Success(()))
    verifyIsValid(valueF) shouldBe(Success(()))
    val costFun = IR.compile[SInt.type](costF)
    val IntConstant(estimatedCost) = costFun(ctx)
    (estimatedCost < CostTable.ScriptLimit) shouldBe true
    val valueFun = IR.compile(valueF)
    valueFun(ctx) shouldBe expected
  }

  test("costed constants") {
    val ctx = newContext(height = 1, boxA1)

    check(noEnv, "one", "1", ctx,  IntConstant(1))
    check(noEnv, "oneL", "1L", ctx, LongConstant(1L))
  }

  test("Crowd Funding") {
    val ctx = newContext(height = 1, boxA1)
    check(envCF, "CrowdFunding", crowdFundingScript, ctx,  1)
  }
}
