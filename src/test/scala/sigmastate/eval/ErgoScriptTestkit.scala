package sigmastate.eval

import scala.util.Success
import sigmastate.{SInt, SType}
import sigmastate.Values.{Constant, SValue, IntConstant}
import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import sigmastate.utxo.CostTable
import special.sigma.{TestBox, ContractsTestkit, AnyValue, Box, TestContext => TContext, SigmaContract => SContract, Context => VContext}
import scalan.BaseCtxTests
import sigmastate.lang.TransformingSigmaBuilder
import org.scalatest.TryValues._

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

  type EsEnv = Map[String, Any]

  val noEnv: EsEnv = Map()
  val AliceId = Array[Byte](1) // 0x0001
  def newAliceBox(id: Byte, value: Long) = new TestBox(
    Cols.fromArray(Array[Byte](0, id)), value, Cols.fromArray(AliceId), noBytes, noBytes, noRegisters)

  def newContext(height: Long, self: Box, vars: AnyValue*): TContext = {
    new TContext(noInputs, noOutputs, height, self, emptyAvlTree, vars.toArray)
  }
  implicit class TContextOps(ctx: TContext) {
    def withInputs(inputs: Box*) = new TContext(inputs.toArray, ctx.outputs, ctx.height, ctx.selfBox, emptyAvlTree, ctx.vars)
    def withOutputs(outputs: Box*) = new TContext(ctx.inputs, outputs.toArray, ctx.height, ctx.selfBox, emptyAvlTree, ctx.vars)
  }

  val boxA1 = newAliceBox(1, 100)
  val boxA2 = newAliceBox(2, 200)

  implicit class ErgoBoxOps(ebox: ErgoBox) {
    def toTestBox: Box = {
      val rs = regs(ebox.additionalRegisters.map { case (k,v) => (k.number -> v) })
      new TestBox(Cols.fromArray(ebox.id), ebox.value, Cols.fromArray(ebox.propositionBytes), noBytes, noBytes, rs)
    }
  }

  implicit class ErgoLikeContextOps(ergoCtx: ErgoLikeContext) {
    def toTestContext: TContext = {
      val inputs = ergoCtx.boxesToSpend.toArray.map(_.toTestBox)
      val outputs = ergoCtx.spendingTransaction.outputs.toArray.map(_.toTestBox)
      val vars = contextVars(ergoCtx.extension.values)
      new TContext(inputs, outputs, ergoCtx.currentHeight, ergoCtx.self.toTestBox, emptyAvlTree, vars.arr)
    }
  }

  import IR._
  import Context._
  case class EsTestCase[T](
      name: String,  // name of the test case, used in forming file names in test-out directory
      env: EsEnv,
      script: String,
      ctx: Option[VContext] = None,
      testContract: Option[SContract] = None,
      expectedCalc: Option[Rep[Context] => Rep[T]] = None,
      expectedCost: Option[Rep[Context] => Rep[Int]] = None,
      expectedTree: Option[SValue] = None,
      expectedResult: Option[T] = None,
      printGraphs: Boolean = true)
  {
    lazy val expectedCalcF = expectedCalc.map(fun(_))
    lazy val expectedCostF = expectedCost.map(fun(_))

    def checkExpected[T](x: T, expected: Option[T]) = {
      if (expected.isDefined)
        x shouldBe expected.get
    }

    def doCosting: Rep[(Context => T, (Context => Int, Context => Long))] = {
      val costed = cost(env, script)
      val res @ Tuple(calcF, costF, sizeF) = split(costed.asRep[Context => Costed[T]])
      if (printGraphs) {
        val graphs = Seq(res) ++ expectedCalcF.toSeq ++ expectedCostF.toSeq
        emit(name, graphs:_*)
      }
      checkExpected(calcF, expectedCalcF)
      checkExpected(costF, expectedCostF)
      res
    }

    def doReduce(): T = {
      val Tuple(calcF, costF, _) = doCosting
      verifyCostFunc(costF) shouldBe Success(())
      verifyIsValid(calcF) shouldBe Success(())
      val costFun = IR.compile[SInt.type](getDataEnv, costF)
      val IntConstant(estimatedCost) = costFun(ctx.get)
      (estimatedCost < CostTable.ScriptLimit) shouldBe true
      val valueFun = IR.compile[SType](getDataEnv, calcF.asRep[Context => SType#WrappedType])
      val Constant(res: T @unchecked, _) = valueFun(ctx.get)
      checkExpected(res, expectedResult)
      res
    }
  }

  def checkInEnv[T](env: EsEnv, name: String, script: String,
      expectedCalc: Rep[Context] => Rep[T],
      expectedCost: Rep[Context] => Rep[Int],
      doChecks: Boolean = true ): Rep[(Context => T, (Context => Int, Context => Long))] =
  {
    val tc = EsTestCase[T](name, env, script,
      expectedCalc = if (doChecks) Some(expectedCalc) else None,
      expectedCost = if (doChecks) Some(expectedCost) else None)
    val res = tc.doCosting
    res
  }

  def check[T](name: String, script: String,
      expectedCalc: Rep[Context] => Rep[T],
      expectedCost: Rep[Context] => Rep[Int]): Rep[(Context => T, (Context => Int, Context => Long))] =
    checkInEnv(Map(), name, script, expectedCalc, expectedCost)

  def reduce(env: EsEnv, name: String, script: String, ctx: VContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase[SType#WrappedType](name, env, script, Some(ctx), expectedResult = Some(expectedResult.asInstanceOf[SType#WrappedType]))
    tcase.doReduce()
  }

  def build(env: Map[String, Any], name: String, script: String, expected: SValue): Unit = {
    val costed = cost(env, script)
    val Tuple(valueF, costF, sizeF) = split(costed)
    emit(name, valueF, costF)
    verifyCostFunc(costF) shouldBe(Success(()))
    verifyIsValid(valueF) shouldBe(Success(()))
    IR.buildTree(valueF) shouldBe expected
  }

}
