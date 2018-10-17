package sigmastate.eval

import scala.util.Success
import sigmastate.{SInt, AvlTreeData, SLong, SType}
import sigmastate.Values.{LongConstant, Constant, EvaluatedValue, SValue, TrueLeaf, SigmaPropConstant, IntConstant, BigIntArrayConstant}
import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox}
import sigmastate.utxo.CostTable
import special.sigma.{ContractsTestkit, Box => DBox, SigmaContract => DContract, Context => DContext, TestBox => DTestBox, TestContext => DTestContext}

import scalan.BaseCtxTests
import sigmastate.lang.LangTests
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.ScriptEnv

trait ErgoScriptTestkit extends ContractsTestkit with LangTests { self: BaseCtxTests =>

  implicit lazy val IR: TestContext with IRContext =
    new TestContext with IRContext with CompiletimeCosting

  import IR._
  import Liftables._
  import Context._
  import WBigInteger._



  def newErgoContext(height: Long, boxToSpend: ErgoBox, extension: Map[Byte, EvaluatedValue[SType]] = Map()): ErgoLikeContext = {
    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())
    val ergoCtx = ErgoLikeContext(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = boxToSpend,
      extension = ContextExtension(extension))
    ergoCtx
  }


  val boxA1 = newAliceBox(1, 100)
  val boxA2 = newAliceBox(2, 200)

  def contract(canOpen: DContext => Boolean) = new NoEnvContract(canOpen)

  lazy val dsl = sigmaDslBuilder
  lazy val bigSym = liftConst(big)
  lazy val n1Sym = liftConst(n1)

  val timeout = 100L
  val minToRaise = 1000L
  val backerPubKeyId = 1.toByte
  val projectPubKeyId = 2.toByte
  val backerProver = new ErgoLikeProvingInterpreter
  val projectProver = new ErgoLikeProvingInterpreter
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage
  val ctxVars = contextVars(Map(
    backerPubKeyId -> backerPubKey,
    projectPubKeyId -> projectPubKey,
    3.toByte -> bigIntArr1
  )).arr

  val boxToSpend = ErgoBox(10, TrueLeaf,
    additionalRegisters = Map(ErgoBox.R4 -> BigIntArrayConstant(bigIntArr1)))
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
      projectPubKeyId -> SigmaPropConstant(projectPubKey),
      3.toByte -> BigIntArrayConstant(bigIntArr1)
    )))

  case class Result(calc: Option[Any], cost: Option[Int], size: Option[Long])
  object Result {
    def Ignore = Result(None, None, None)
    def apply(calc: Any): Result = Result(Some(calc), None, None)
    def apply(calc: Any, cost: Int, size: Long): Result = Result(Some(calc), Some(cost), Some(size))
  }
  def NoResult = Result(None, None, None)
  case class EsTestCase(
      name: String,  // name of the test case, used in forming file names in test-out directory
      env: ScriptEnv,
      script: String,
      ergoCtx: Option[ErgoLikeContext] = None,
      testContract: Option[DContext => Any] = None,
      expectedCalc: Option[Rep[Context] => Rep[Any]] = None,
      expectedCost: Option[Rep[Context] => Rep[Int]] = None,
      expectedSize: Option[Rep[Context] => Rep[Long]] = None,
      expectedTree: Option[SValue] = None,
      expectedResult: Result = NoResult,
      printGraphs: Boolean = true,
      measureTime: Boolean = false)
  {
    lazy val expectedCalcF = expectedCalc.map(f => fun(removeIsValid(f)))
    lazy val expectedCostF = expectedCost.map(fun(_))
    lazy val expectedSizeF = expectedSize.map(fun(_))

    def checkExpected[T](block: => T, expected: Option[T], messageFmt: String) = {
      if (expected.isDefined) {
        val x = block
        x shouldBe expected.get
      }
//          String.format(messageFmt, x.asInstanceOf[AnyRef], expected.get.asInstanceOf[AnyRef]))
    }

    def pairify(xs: Seq[Sym]): Sym = xs match {
      case Seq(x) => x
      case Seq(a, b) => Pair(a, b)
      case _ => Pair(xs.head, pairify(xs.tail))
    }

    def doCosting: Rep[(Context => Any, (Context => Int, Context => Long))] = {
      val costed = cost(env, script)
      val res @ Tuple(calcF, costF, sizeF) = split3(costed.asRep[Context => Costed[Any]])
      if (printGraphs) {
        val str = struct(
          "calc" -> calcF,
          "cost" -> costF,
          "size" -> sizeF
        )
        val strExp = struct(
          expectedCalcF.map("calc" -> _).toSeq ++
          expectedCostF.map("cost" -> _).toSeq ++
          expectedSizeF.map("size" -> _).toSeq
        )
        val graphs = Seq(str, strExp)
        emit(name, graphs:_*)
      }
      checkExpected(calcF, expectedCalcF, "Calc function actual: %s, expected: %s")
      checkExpected(costF, expectedCostF, "Cost function actual: %s, expected: %s")
      checkExpected(sizeF, expectedSizeF, "Size function actual: %s, expected: %s")
      res
    }

    def doReduce(): Unit = {
      val Tuple(calcF, costF, sizeF) = doCosting
      verifyCostFunc(costF) shouldBe Success(())
      verifyIsValid(calcF) shouldBe Success(())

      checkExpected(IR.buildTree(calcF.asRep[Context => SType#WrappedType]), expectedTree,
        "Compiled Tree actual: %s, expected: %s")

      if (ergoCtx.isDefined) {
        val calcCtx = ergoCtx.get.toSigmaContext(IR, isCost = false)
        val testContractRes = testContract.map(_(calcCtx))
        testContractRes.foreach { res =>
          checkExpected(res, expectedResult.calc, "Test Contract actual: %s, expected: %s")
        }

        // check cost
        val costCtx = ergoCtx.get.toSigmaContext(IR, isCost = true)
        val costFun = IR.compile[SInt.type](getDataEnv, costF)
        val IntConstant(estimatedCost) = costFun(costCtx)
        checkExpected(estimatedCost, expectedResult.cost,
          "Cost evaluation: estimatedCost = %s, expectedResult.cost = %s")
        (estimatedCost < CostTable.ScriptLimit) shouldBe true

        // check size
        val sizeFun = IR.compile[SLong.type](getDataEnv, sizeF)
        val LongConstant(estimatedSize) = sizeFun(costCtx)
        checkExpected(estimatedSize, expectedResult.size,
          "Size evaluation: estimatedSize = %s, expectedResult.size: %s"
        )

        // check calc
        val valueFun = IR.compile[SType](getDataEnv, calcF.asRep[Context => SType#WrappedType])
        val res = valueFun(calcCtx) match {
          case Constant(res: Any, _) => res
          case v => v
        }
        checkExpected(res, expectedResult.calc,
          "Calc evaluation:\n value = %s,\n expectedResult.calc: %s\n")
      }
    }
  }

  def Case(env: ScriptEnv, name: String, script: String, ctx: ErgoLikeContext,
      calc: Rep[Context] => Rep[Any],
      cost: Rep[Context] => Rep[Int],
      size: Rep[Context] => Rep[Long],
      tree: SValue,
      result: Result) =
    EsTestCase(name, env, script, Option(ctx), None,
      Option(calc), Option(cost), Option(size),
      Option(tree), result)

  def checkAll(env: ScriptEnv, name: String, script: String, ergoCtx: ErgoLikeContext,
      calc: Rep[Context] => Rep[Any],
      cost: Rep[Context] => Rep[Int],
      size: Rep[Context] => Rep[Long],
      tree: SValue,
      result: Result): Unit =
  {
    val tcase = Case(env, name, script, ergoCtx, calc, cost, size, tree, result)
    tcase.doReduce()
  }

  def checkInEnv(env: ScriptEnv, name: String, script: String,
      expectedCalc: Rep[Context] => Rep[Any],
      expectedCost: Rep[Context] => Rep[Int] = null,
      expectedSize: Rep[Context] => Rep[Long] = null
  ): Rep[(Context => Any, (Context => Int, Context => Long))] =
  {
    val tc = EsTestCase(name, env, script, None, None,
      Option(expectedCalc),
      Option(expectedCost),
      Option(expectedSize), expectedTree = None, expectedResult = NoResult, printGraphs = true )
    val res = tc.doCosting
    res
  }

  def check(name: String, script: String,
      expectedCalc: Rep[Context] => Rep[Any],
      expectedCost: Rep[Context] => Rep[Int] = null,
      expectedSize: Rep[Context] => Rep[Long] = null
      ): Rep[(Context => Any, (Context => Int, Context => Long))] =
  {
    checkInEnv(Map(), name, script, expectedCalc, expectedCost, expectedSize)
  }

  def reduce(env: ScriptEnv, name: String, script: String, ergoCtx: ErgoLikeContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase(name, env, script, Some(ergoCtx), expectedResult = Result(expectedResult))
    tcase.doReduce()
  }

  def build(env: ScriptEnv, name: String, script: String, expected: SValue): Unit = {
    val costed = cost(env, script)
    val Tuple(valueF, costF, sizeF) = split3(costed)
    emit(name, valueF, costF, sizeF)
    verifyCostFunc(costF) shouldBe(Success(()))
    verifyIsValid(valueF) shouldBe(Success(()))
    IR.buildTree(valueF) shouldBe expected
  }

  def measure[T](nIters: Int, okShow: Boolean = true)(action: Int => Unit): Unit = {
    for (i <- 0 until nIters) {
      val start = System.currentTimeMillis()
      val res = action(i)
      val end = System.currentTimeMillis()
      val iterTime = end - start
      if (okShow)
        println(s"Iter $i: $iterTime ms")
    }
  }

}
