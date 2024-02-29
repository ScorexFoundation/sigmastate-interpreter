package sigmastate.eval

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.validation.ValidationSpecification
import org.ergoplatform._
import scalan.BaseCtxTests
import sigma.VersionContext
import sigma.ast.{BigIntArrayConstant, ErgoTree, EvaluatedValue, SigmaPropConstant, Value}
import sigma.ast.SType
import sigma.ast.syntax.SValue
import sigma.data.AvlTreeData
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.interpreter.CErgoTreeEvaluator
import sigma.ast.syntax.ValueOps
import sigma.interpreter.{ContextExtension, SigmaMap}
import sigmastate.lang.{CompilerResult, CompilerSettings, LangTests, SigmaCompiler}
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.CompilerTestsBase
import sigma.{ContractsTestkit, Context => DContext}

import scala.annotation.unused
import scala.util.Success

trait ErgoScriptTestkit extends ContractsTestkit with LangTests
    with ValidationSpecification with CompilerTestsBase { self: BaseCtxTests =>

  implicit lazy val IR: TestContext with IRContext =
    new TestContext with IRContext

  import IR._
  import BigInt._
  import Context._
  import Liftables._

  override lazy val compiler = SigmaCompiler(CompilerSettings(
    TestnetNetworkPrefix,
    IR.builder,
    lowerMethodCalls = true
  ))

  def newErgoContext(height: Int, boxToSpend: ErgoBox, extension: Map[Byte, EvaluatedValue[SType]] = Map()): ErgoLikeContext = {
    val tx1 = new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(boxToSpend))
    val ergoCtx = ErgoLikeContextTesting(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx1,
      self = boxToSpend, activatedVersionInTests,
      extension = ContextExtension(SigmaMap(extension)))
            .withErgoTreeVersion(ergoTreeVersionInTests)
    ergoCtx
  }


  lazy val boxA1 = newAliceBox(1, 100)
  lazy val boxA2 = newAliceBox(2, 200)

  lazy val n1Sym = liftConst(n1)

  val timeout = 100
  val minToRaise = 1000L
  val backerPubKeyId = 1.toByte
  val projectPubKeyId = 2.toByte
  lazy val backerProver = new ContextEnrichingTestProvingInterpreter
  lazy val projectProver = new ContextEnrichingTestProvingInterpreter
  lazy val backerPubKey = backerProver.dlogSecrets.head.publicImage
  lazy val projectPubKey = projectProver.dlogSecrets.head.publicImage

  lazy val boxToSpend = testBox(10, TrueTree, 0,
    additionalRegisters = Map(ErgoBox.R4 -> BigIntArrayConstant(bigIntegerArr1)))
  lazy val tx1Output1 = testBox(minToRaise, ErgoTree.fromProposition(projectPubKey), 0)
  lazy val tx1Output2 = testBox(1, ErgoTree.fromProposition(projectPubKey), 0)
  lazy val tx1 = new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
  lazy val ergoCtx = ErgoLikeContextTesting(
    currentHeight = timeout - 1,
    lastBlockUtxoRoot = AvlTreeData.dummy,
    minerPubkey = ErgoLikeContextTesting.dummyPubkey,
    boxesToSpend = IndexedSeq(boxToSpend),
    spendingTransaction = tx1,
    self = boxToSpend,
    activatedVersionInTests,
    extension = ContextExtension(SigmaMap(Map(
      backerPubKeyId -> SigmaPropConstant(backerPubKey),
      projectPubKeyId -> SigmaPropConstant(projectPubKey),
      3.toByte -> BigIntArrayConstant(bigIntegerArr1)
    ))))

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
      expectedCalc: Option[Ref[Context] => Ref[Any]] = None,
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

    def checkExpected[T](block: => T, expected: Option[T], messageFmt: String) = {
      if (expected.isDefined) {
        val x = block
        withClue(messageFmt) {
          x shouldBe expected.get
        }
      }
    }

    def checkExpectedFunc[A,B](block: => Ref[A => B], expected: Option[Ref[A => B]], messageFmt: String) = {
      if (expected.isDefined) {
        val x = block
        assert(alphaEqual(x, expected.get), messageFmt)
      }
    }

    def doCosting: CompilerResult[IR.type] = {
      val res = compiler.compileTyped(env, tree)
      val calcF = res.compiledGraph
      if (printGraphs) {
        val str = calcF
        val strExp = expectedCalcF.toSeq
        val graphs = str +: strExp
        emit(name, graphs:_*)
      }
      checkExpectedFunc(calcF, expectedCalcF, "Calc function actual: %s, expected: %s")
      res
    }

    def doReduce(): Unit = {
      val res = doCosting
      verifyIsProven(res.compiledGraph) shouldBe Success(())
      val ergoTree = mkTestErgoTree(res.buildTree.asSigmaProp)

      if (expectedTree.isDefined) {
        checkExpected(res.buildTree, expectedTree, "Compiled Tree actual: %s, expected: %s")

        val compiledTreeBytes = DefaultSerializer.serializeErgoTree(ergoTree)
        checkExpected(DefaultSerializer.deserializeErgoTree(compiledTreeBytes), Some(ergoTree),
          "(de)serialization round trip actual: %s, expected: %s")
      }

      if (ergoCtx.isDefined) {
        val ectx = ergoCtx.get.withErgoTreeVersion(ergoTreeVersionInTests)
        VersionContext.withVersions(ectx.activatedScriptVersion, ergoTreeVersionInTests) {
          val calcCtx = ectx.toSigmaContext()
          val testContractRes = testContract.map(_(calcCtx))
          testContractRes.foreach { res =>
            checkExpected(res, expectedResult.calc, "Test Contract actual: %s, expected: %s")
          }

          // check calc
          val (res, _) = CErgoTreeEvaluator.eval(
            context = ectx,
            constants = ergoTree.constants,
            exp = ergoTree.toProposition(replaceConstants = false),
            evalSettings = CErgoTreeEvaluator.DefaultEvalSettings
          )
          checkExpected(res, expectedResult.calc,
            "Calc evaluation:\n value = %s,\n expectedResult.calc: %s\n")
        }
      }
    }
  }

  def Case(env: ScriptEnv, name: String, script: String, ctx: ErgoLikeContext,
           calc: Ref[Context] => Ref[Any],
           tree: SValue,
           result: Result) =
    EsTestCase(name, env, Code(script), Option(ctx), None,
      Option(calc), Option(tree), result)

  def reduce(env: ScriptEnv, name: String, script: String, ergoCtx: ErgoLikeContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase(name, env, Code(script), Some(ergoCtx), expectedResult = Result(expectedResult))
    tcase.doReduce()
  }

  def reduce(env: ScriptEnv, name: String, tree: Value[SType], ergoCtx: ErgoLikeContext, expectedResult: Any): Unit = {
    val tcase = EsTestCase(name, env, Tree(tree), Some(ergoCtx), expectedResult = Result(expectedResult))
    tcase.doReduce()
  }

  def build(env: ScriptEnv, @unused name: String, script: String, expected: SValue): Unit = {
    val tree = compile(env, script)
    tree  shouldBe expected
  }

}
