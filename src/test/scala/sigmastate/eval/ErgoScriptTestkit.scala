package sigmastate.eval

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix

import scala.util.Success
import sigmastate.{SInt, AvlTreeData, SLong, SType}
import sigmastate.Values.{LongConstant, Constant, EvaluatedValue, SValue, TrueLeaf, SigmaPropConstant, Value, IntConstant, BigIntArrayConstant}
import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox, ErgoScriptPredef}
import sigmastate.utxo.CostTable
import scalan.BaseCtxTests
import sigmastate.lang.{LangTests, SigmaCompiler}
import sigmastate.helpers.ContextEnrichingTestProvingInterpreter
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.serialization.ErgoTreeSerializer
import special.sigma.{ContractsTestkit, Context => DContext, _}
import sigmastate.eval.Extensions._

import scala.language.implicitConversions

trait ErgoScriptTestkit extends ContractsTestkit with LangTests { self: BaseCtxTests =>

  implicit lazy val IR: TestContext with IRContext =
    new TestContext with IRContext with CompiletimeCosting

  import IR._
  import Liftables._
  import Context._
  import Size._
//  import WBigInteger._
  import BigInt._

  lazy val compiler = new SigmaCompiler(TestnetNetworkPrefix, IR.builder)

  def newErgoContext(height: Int, boxToSpend: ErgoBox, extension: Map[Byte, EvaluatedValue[SType]] = Map()): ErgoLikeContext = {
    val tx1 = new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(boxToSpend))
    val ergoCtx = ErgoLikeContext(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx1,
      self = boxToSpend,
      extension = ContextExtension(extension))
    ergoCtx
  }


  lazy val boxA1 = newAliceBox(1, 100)
  lazy val boxA2 = newAliceBox(2, 200)

  def contract(canOpen: DContext => Boolean) = new NoEnvContract(canOpen)

  lazy val dsl = sigmaDslBuilder
  lazy val dslValue = sigmaDslBuilderValue
  lazy val bigSym = liftConst(dslValue.BigInt(big))
  lazy val n1Sym = liftConst(dslValue.BigInt(n1))

  val timeout = 100
  val minToRaise = 1000L
  val backerPubKeyId = 1.toByte
  val projectPubKeyId = 2.toByte
  lazy val backerProver = new ContextEnrichingTestProvingInterpreter
  lazy val projectProver = new ContextEnrichingTestProvingInterpreter
  lazy val backerPubKey = backerProver.dlogSecrets.head.publicImage
  lazy val projectPubKey = projectProver.dlogSecrets.head.publicImage
  lazy val ctxVars = contextVars(Map(
    backerPubKeyId -> backerPubKey.toAnyValue,
    projectPubKeyId -> projectPubKey.toAnyValue,
    3.toByte -> toAnyValue(bigIntegerArr1)
  )).toArray

  val boxToSpend = ErgoBox(10, ErgoScriptPredef.TrueProp, 0,
    additionalRegisters = Map(ErgoBox.R4 -> BigIntArrayConstant(bigIntegerArr1)))
  lazy val tx1Output1 = ErgoBox(minToRaise, projectPubKey, 0)
  lazy val tx1Output2 = ErgoBox(1, projectPubKey, 0)
  lazy val tx1 = new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
  lazy val ergoCtx = ErgoLikeContext(
    currentHeight = timeout - 1,
    lastBlockUtxoRoot = AvlTreeData.dummy,
    minerPubkey = ErgoLikeContext.dummyPubkey,
    boxesToSpend = IndexedSeq(boxToSpend),
    spendingTransaction = tx1,
    self = boxToSpend,
    extension = ContextExtension(Map(
      backerPubKeyId -> SigmaPropConstant(backerPubKey),
      projectPubKeyId -> SigmaPropConstant(projectPubKey),
      3.toByte -> BigIntArrayConstant(bigIntegerArr1)
    )))

  case class Result(calc: Option[Any], cost: Option[Int], size: Option[Long])
  object Result {
    def Ignore = Result(None, None, None)
    def apply(calc: Any): Result = Result(Some(calc), None, None)
    def apply(calc: Any, cost: Int, size: Long): Result = Result(Some(calc), Some(cost), Some(size))
  }
  def NoResult = Result(None, None, None)

  sealed trait Script
  case class Code(code: String) extends Script
  case class Tree(tree: Value[SType]) extends Script

  case class EsTestCase(
      name: String,  // name of the test case, used in forming file names in test-out directory
      env: ScriptEnv,
      script: Script,
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
    lazy val tree = script match {
      case Code(code) => compiler.typecheck(env, code)
      case Tree(t) => t
    }
    lazy val expectedCalcF = expectedCalc.map(f => fun(removeIsProven(f)))
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

    def doCosting: Rep[(Context => Any, (Size[Context] => Int, Size[Context] => Long))] = {
      val costed = cost[Any](env, tree)
      val calcF = costed.sliceCalc
      val costF = fun { sCtx: RSize[Context] => costed.sliceCost(Pair(0, sCtx)) }
      val sizeF = fun { sCtx: RSize[Context] => costed.sliceSize(sCtx).dataSize }
      val res = Tuple(calcF, costF, sizeF)
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
      val Pair(calcF, Pair(costF, sizeF)) = doCosting
      verifyCostFunc(asRep[Any => Int](costF)) shouldBe Success(())
      verifyIsProven(calcF) shouldBe Success(())

      if (expectedTree.isDefined) {
        val compiledTree = IR.buildTree(calcF.asRep[Context => SType#WrappedType])
        checkExpected(compiledTree, expectedTree, "Compiled Tree actual: %s, expected: %s")

        val compiledTreeBytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(compiledTree)
        checkExpected(ErgoTreeSerializer.DefaultSerializer.deserialize(compiledTreeBytes), Some(compiledTree),
          "(de)serialization round trip actual: %s, expected: %s")
      }

      if (ergoCtx.isDefined) {
        val calcCtx = ergoCtx.get.toSigmaContext(IR, isCost = false)
        val testContractRes = testContract.map(_(calcCtx))
        testContractRes.foreach { res =>
          checkExpected(res, expectedResult.calc, "Test Contract actual: %s, expected: %s")
        }

        // check cost
        val costCtx = ergoCtx.get.toSigmaContext(IR, isCost = true)
        val estimatedCost = IR.checkCost(costCtx, tree, costF, CostTable.ScriptLimit)

        // check size
        {
          val lA = sizeF.elem.eDom.liftable.asLiftable[SSize[SContext], Size[Context]]
          val sizeFun = IR.compile[SSize[SContext], Long, Size[Context], Long](getDataEnv, sizeF)(lA, liftable[Long, Long])
          val estimatedSize = sizeFun(Sized.sizeOf(costCtx))
          checkExpected(estimatedSize, expectedResult.size,
            "Size evaluation: estimatedSize = %s, expectedResult.size: %s"
          )
        }

        // check calc
        val lA = calcF.elem.eDom.liftable.asLiftable[SContext, Context]
        val lB = calcF.elem.eRange.liftable.asLiftable[Any, Any]
        val valueFun = IR.compile(getDataEnv, calcF)(lA, lB)
        val (res, _) = valueFun(calcCtx)
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
    EsTestCase(name, env, Code(script), Option(ctx), None,
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
                 expectedSize: Rep[Context] => Rep[Long] = null,
                 printGraphs: Boolean = true
                ): Rep[(Context => Any, (Size[Context] => Int, Size[Context] => Long))] =
  {
    val tc = EsTestCase(name, env, Code(script), None, None,
      Option(expectedCalc),
      Option(expectedCost),
      Option(expectedSize), expectedTree = None, expectedResult = NoResult, printGraphs)
    val res = tc.doCosting
    res
  }

  def check(name: String, script: String,
      expectedCalc: Rep[Context] => Rep[Any],
      expectedCost: Rep[Context] => Rep[Int] = null,
      expectedSize: Rep[Context] => Rep[Long] = null,
      printGraphs: Boolean = true
      ): Rep[(Context => Any, (Size[Context] => Int, Size[Context] => Long))] =
  {
    checkInEnv(Map(), name, script, expectedCalc, expectedCost, expectedSize, printGraphs)
  }

  def reduce(env: ScriptEnv, name: String, script: String, ergoCtx: ErgoLikeContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase(name, env, Code(script), Some(ergoCtx), expectedResult = Result(expectedResult))
    tcase.doReduce()
  }

  def reduce(env: ScriptEnv, name: String, tree: Value[SType], ergoCtx: ErgoLikeContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase(name, env, Tree(tree), Some(ergoCtx), expectedResult = Result(expectedResult))
    tcase.doReduce()
  }

  def compileAndCost[T](env: ScriptEnv, code: String): Rep[Costed[Context] => Costed[T]] = {
    val typed = compiler.typecheck(env, code)
    cost[T](env, typed)
  }

  def build(env: ScriptEnv, name: String, script: String, expected: SValue): Unit = {
    val costed = compileAndCost[Any](env, script)
    val valueF = costed.sliceCalc(true)
    val costF  = costed.sliceCost
    val sizeF  = costed.sliceSize
    
    emit(name, valueF, costF, sizeF)
    verifyCostFunc(asRep[Any => Int](costF)) shouldBe(Success(()))
    verifyIsProven(valueF) shouldBe(Success(()))
    IR.buildTree(valueF) shouldBe expected
  }

}
