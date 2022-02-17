package special.sigma

import java.util
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, TestContractSpec}
import org.ergoplatform.validation.{SigmaValidationSettings, ValidationException, ValidationRules}
import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules.CheckSerializableTypeCode
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.frequency
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scalan.RType
import scalan.RType._
import scalan.util.BenchmarkUtil
import scalan.util.Extensions._
import scalan.util.CollectionUtil._
import sigmastate.SType.AnyOps
import sigmastate.Values.{ByteArrayConstant, Constant, ConstantNode, ErgoTree, IntConstant, SValue}
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.basics.{SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import sigmastate.eval.Extensions._
import sigmastate.eval.{CompiletimeIRContext, CostingBox, CostingDataContext, Evaluation, IRContext, SigmaDsl}
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter, SigmaPPrint}
import sigmastate.interpreter.EvalSettings.{AotEvaluationMode, EvaluationMode, JitEvaluationMode}
import sigmastate.interpreter.Interpreter.{ScriptEnv, VerificationResult}
import sigmastate.interpreter._
import sigmastate.lang.Terms.ValueOps
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.utils.Helpers._
import sigmastate.utxo.{DeserializeContext, DeserializeRegister}
import sigmastate.{SOption, SSigmaProp, SType, VersionContext, eval}
import special.collection.{Coll, CollType}
import spire.syntax.all.cfor

import scala.collection.mutable
import scala.math.Ordering
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class SigmaDslTesting extends PropSpec
    with PropertyChecks
    with Matchers
    with SigmaTestingData with SigmaContractSyntax
    with ObjectGenerators { suite =>
  override def Coll[T](items: T*)(implicit cT: RType[T]): Coll[T] = super.Coll(items:_*)

  lazy val spec: ContractSpec = TestContractSpec(suite)(new TestingIRContext)

  override def contractEnv: ScriptEnv = Map()

  def createIR(): IRContext = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
    override val okMeasureOperationTime: Boolean = true
  }

  def checkEq[A,B](scalaFunc: A => B)(g: A => (B, CostDetails)): A => Try[(B, CostDetails)] = { x: A =>
    val b1 = Try(scalaFunc(x))
    val b2 = Try(g(x))
    (b1, b2) match {
      case (Success(b1), res @ Success((b2, _))) =>
        assert(b1 == b2)
        res
      case (Failure(t1), res @ Failure(t2)) =>
        val c1 = rootCause(t1).getClass
        val c2 = rootCause(t2).getClass
        if (c1 != c2) {
          assert(c1 == c2,
            s"""Different errors:
              |First result: $t1
              |Second result: $t2
              |""".stripMargin)
        }
        res
      case _ =>
        val cause = if (b1.isFailure)
          rootCause(b1.asInstanceOf[Failure[_]].exception)
        else
          rootCause(b2.asInstanceOf[Failure[_]].exception)

        sys.error(
          s"""Should succeed with the same value or fail with the same exception, but was:
            |First result: $b1
            |Second result: $b2
            |Root cause: $cause
            |""".stripMargin)
    }

  }

  def checkEq2[A,B,R](f: (A, B) => R)(g: (A, B) => R): (A,B) => Unit = { (x: A, y: B) =>
    val r1 = f(x, y); val r2 = g(x, y)
    assert(r1.getClass == r2.getClass)
    assert(r1 == r2)
  }

  def getArrayIndex(len: Int): Int = {
    val index = Gen.choose(0, len - 1)
    index.sample.get
  }

  /** Generate indices for an array of a given length.
    * @return unordered array of indices with possibly repeated elements
    */
  def genIndices(arrLength: Int): Gen[Array[Int]] = for {
    nIndexes <- Gen.choose(0, arrLength)
    indices <- Gen.containerOfN[Array, Int](nIndexes, Gen.choose(0, arrLength - 1))
  } yield indices

  class FeatureProvingInterpreter extends ErgoLikeTestInterpreter()(new TestingIRContext) with ProverInterpreter {

    def decodeSecretInput(decimalStr: String): DLogProverInput = DLogProverInput(BigInt(decimalStr).bigInteger)

    val sk1: DLogProverInput = decodeSecretInput("416167686186183758173232992934554728075978573242452195968805863126437865059")
    val sk2: DLogProverInput = decodeSecretInput("34648336872573478681093104997365775365807654884817677358848426648354905397359")
    val sk3: DLogProverInput = decodeSecretInput("50415569076448343263191022044468203756975150511337537963383000142821297891310")

    val secrets: Seq[SigmaProtocolPrivateInput[_ <: SigmaProtocol[_], _ <: SigmaProtocolCommonInput[_]]] = {
      // Note, not all secrets are used, which is required by checkVerify
      // This is to make AtLeast to be unproved and thus the verify is successfull
      // because of the other condition in SigmaOr (see checkVerify)
      val dlogs: IndexedSeq[DLogProverInput] = Vector(sk1)
      dlogs
    }

    val pubKeys: Seq[ProveDlog] = Vector(sk1, sk2, sk3)
        .collect { case in: DLogProverInput => in.publicImage }
  }

  val LogScriptDefault: Boolean = false

  val isNewVersion = new scala.util.DynamicVariable(false)

  val predefScripts = Seq[String]()

  /** Descriptor of the language feature.
    * Each language feature is described by so called feature-function which exercises
    * some specific operation under test.
    *
    * For example, the following feature-function exercises binary `+` operation for `Int` type.
    * `{ (in: (Int, Int)) => in._1 + in._2 }`
    *
    * Feature function script is compiled into expression and then executed on a series of
    * test cases. The outputs if each test case is compared with expected values.
    *
    * Test cases are specified for all supported versions.
    * @see ExistingFeature, ChangedFeature
    */
  trait Feature[A, B] { feature =>

    /** Script containing this feature. */
    def script: String

    /** Semantics of this feature in ErgoTree v1 given by scala code */
    def scalaFunc: A => B

    /** Semantics of this feature in ErgoTree v2 given by scala code */
    def scalaFuncNew: A => B = scalaFunc

    /** Expression which represents the test case code. */
    def expectedExpr: Option[SValue]

    /** Function that executes the feature using v4.x interpreter implementation. */
    def oldImpl: () => CompiledFunc[A, B]

    /** Function that executes the feature using v5.x interpreter implementation. */
    def newImpl: () => CompiledFunc[A, B]

    def evalSettings: EvalSettings

    def printExpectedExpr: Boolean
    def logScript: Boolean

    /** Called to print test case expression (when it is not given).
      * Can be used to create regression test cases. */
    def printSuggestion(cf: CompiledFunc[_,_]): Unit = {
      print(s"No expectedExpr for ")
      SigmaPPrint.pprintln(cf.script, height = 150)
      print("Use ")
      SigmaPPrint.pprintln(cf.expr, height = 150)
      println()
    }

    /** Checks the result of the front-end compiler against expectedExpr.
      * Used to catch regression errors in front-end compiler.
      */
    def checkExpectedExprIn(cf: CompiledFunc[_,_]): Boolean = {
      expectedExpr match {
        case Some(e) =>
          if (cf.expr != null && cf.expr != e) {
            printSuggestion(cf)
            cf.expr shouldBe e
          }
        case None if printExpectedExpr =>
          printSuggestion(cf)
      }
      true
    }

    /** v3 and v4 implementation*/
    private var _oldF: CompiledFunc[A, B] = _
    def oldF: CompiledFunc[A, B] = {
      if (_oldF == null) {
        _oldF = oldImpl()
        checkExpectedExprIn(_oldF)
      }
      _oldF
    }

    /** v5 implementation*/
    private var _newF: CompiledFunc[A, B] = _
    def newF: CompiledFunc[A, B] = {
      if (_newF == null) {
        _newF = newImpl()
        checkExpectedExprIn(_newF)
      }
      _newF
    }

    /** Compares the old and new implementations against
      * semantic function (scalaFunc) on the given input.
      *
      * @param input          data which is used to execute feature
      * @param logInputOutput if true, then pretty-print input and output values
      * @return result of feature execution */
    def checkEquality(input: A, logInputOutput: Boolean = false): Try[(B, CostDetails)]

    /** Depending on the featureType compares the old and new implementations against
      * semantic function (scalaFunc) on the given input, also checking the given expected result.
      */
    def checkExpected(input: A, expectedResult: Expected[B]): Unit

    /** Tests this feature on the given input.
      * @param input data value
      * @param expectedResult the result which is expected
      */
    def testCase(input: A, expectedResult: Try[B],
                 printTestCases: Boolean = PrintTestCasesDefault,
                 failOnTestVectors: Boolean = FailOnTestVectorsDefault): Unit

    /** Tests this feature by embedding it in the verification script.
      * @param input data value
      * @param expectedResult the result values which are expected
      * @see checkVerify
      */
    def verifyCase(input: A, expectedResult: Expected[B],
                   printTestCases: Boolean = PrintTestCasesDefault,
                   failOnTestVectors: Boolean = FailOnTestVectorsDefault): Unit

    /** Creates a new ErgoLikeContext using given [[CostingDataContext]] as template.
      * Copies most of the data from ctx and the missing data is taken from the args.
      * This is a helper method to be used in tests only.
      */
    def createErgoLikeContext(ctx: CostingDataContext,
                              validationSettings: SigmaValidationSettings,
                              costLimit: Long,
                              initCost: Long
                         ): ErgoLikeContext = {
      val treeData = SigmaDsl.toAvlTreeData(ctx.lastBlockUtxoRootHash)
      val dataBoxes = ctx.dataInputs.toArray.map(SigmaDsl.toErgoBox)
      val boxesToSpend = ctx.inputs.toArray.map(SigmaDsl.toErgoBox)
      val txInputs = boxesToSpend.map(b => Input(b.id, ProverResult.empty))
      val txDataInputs = dataBoxes.map(b => DataInput(b.id))
      val txOutputCandidates = ctx.outputs.toArray.map(SigmaDsl.toErgoBox)
      val tx = new ErgoLikeTransaction(
        txInputs, txDataInputs, txOutputCandidates.toIndexedSeq)
      val selfIndex = boxesToSpend.indexWhere(b => util.Arrays.equals(b.id, ctx.selfBox.id.toArray))

      val extension = ContextExtension(
        values = ctx.vars.toArray.zipWithIndex.collect {
          case (v, i) if v != null =>
            val tpe = Evaluation.rtypeToSType(v.tVal)
            i.toByte -> ConstantNode(v.value.asWrappedType, tpe)
        }.toMap
      )
      new ErgoLikeContext(
        treeData, ctx.headers, ctx.preHeader,
        dataBoxes, boxesToSpend, tx, selfIndex,
        extension, validationSettings, costLimit, initCost,
        activatedVersionInTests)
    }

    /** Executes the default feature verification wrapper script for the specific ErgoTree
      * version using both v4.x and v5.x interpreters.
      * @param input the given test case input data
      * @param expected the given expected results (values and costs)
      */
    def checkVerify(input: A, expected: Expected[B]): Unit = {
      val tpeA = Evaluation.rtypeToSType(oldF.tA)
      val tpeB = Evaluation.rtypeToSType(oldF.tB)

      // Create synthetic ErgoTree which uses all main capabilities of evaluation machinery.
      // 1) first-class functions (lambdas); 2) Context variables; 3) Registers; 4) Equality
      // for all types; 5) Embedding of boolean to SigmaProp; 6) Sigma propositions (&&, ||, AtLeast)
      // 7) Deserialization from SELF and Context
      // Every language Feature is tested as part of this wrapper script.
      // Inclusion of all the features influences the expected cost estimation values
      def compiledTree(prover: FeatureProvingInterpreter) = {
        val code =
          s"""{
            |  val func = ${oldF.script}
            |  val res1 = func(getVar[${oldF.tA.name}](1).get)
            |  val res2 = SELF.R4[${oldF.tB.name}].get
            |  sigmaProp(res1 == res2) && pkAlice
            |}
          """.stripMargin

        val IR = new CompiletimeIRContext
        val pkAlice = prover.pubKeys.head.toSigmaProp
        val env = Map("pkAlice" -> pkAlice)

        // Compile script the same way it is performed by applications (i.e. via Ergo Appkit)
        val prop = ErgoScriptPredef.compileWithCosting(
          env, code, ErgoAddressEncoder.MainnetNetworkPrefix)(IR).asSigmaProp

        // Add additional oparations which are not yet implemented in ErgoScript compiler
        val multisig = sigmastate.AtLeast(
          IntConstant(2),
          Array(
            pkAlice,
            DeserializeRegister(ErgoBox.R5, SSigmaProp),  // deserialize pkBob
            DeserializeContext(2, SSigmaProp)))           // deserialize pkCarol
        val header = ErgoTree.headerWithVersion(ergoTreeVersionInTests)
        ErgoTree.withSegregation(header, sigmastate.SigmaOr(prop, multisig))
      }

      def ergoCtx(prover: FeatureProvingInterpreter, compiledTree: ErgoTree, expectedValue: B) = {
        val pkBobBytes = ValueSerializer.serialize(prover.pubKeys(1).toSigmaProp)
        val pkCarolBytes = ValueSerializer.serialize(prover.pubKeys(2).toSigmaProp)
        val newRegisters = Map(
          ErgoBox.R4 -> Constant[SType](expectedValue.asInstanceOf[SType#WrappedType], tpeB),
          ErgoBox.R5 -> ByteArrayConstant(pkBobBytes)
        )

        val ctx = input match {
          case ctx: CostingDataContext =>
            // the context is passed as function argument (see func in the script)
            // Since Context is singleton, we should use this instance as the basis
            // for execution of verify instead of a new dummy context.
            val self = ctx.selfBox.asInstanceOf[CostingBox]
            val newSelf = self.copy(
              ebox = updatedRegisters(self.ebox, newRegisters)
            )

            // We add ctx as it's own variable with id = 1
            val ctxVar = eval.Extensions.toAnyValue[special.sigma.Context](ctx)(special.sigma.ContextRType)
            val carolVar = eval.Extensions.toAnyValue[Coll[Byte]](pkCarolBytes.toColl)(RType[Coll[Byte]])
            val newCtx = ctx
                .withUpdatedVars(1 -> ctxVar, 2 -> carolVar)
                .copy(
                  selfBox = newSelf,
                  inputs = {
                    val selfIndex = ctx.inputs.indexWhere(b => b.id == ctx.selfBox.id, 0)
                    ctx.inputs.updated(selfIndex, newSelf)
                  })

            createErgoLikeContext(
              newCtx,
              ValidationRules.currentSettings,
              ScriptCostLimit.value,
              initCost = initialCostInTests.value
            )

          case _ =>
            val box = createBox(0, compiledTree, additionalRegisters = newRegisters)

            // make sure we are doing tests with the box with is actually serializable
            try roundTripTest(box)(ErgoBox.sigmaSerializer)
            catch {
              case ValidationException(_, r: CheckSerializableTypeCode.type, Seq(SOption.OptionTypeCode), _) =>
                // ignore the problem with Option serialization, but test all the other cases
            }

            ErgoLikeContextTesting.dummy(box, activatedVersionInTests)
              .withBindings(
                1.toByte -> Constant[SType](input.asInstanceOf[SType#WrappedType], tpeA),
                2.toByte -> ByteArrayConstant(pkCarolBytes))
              .withInitCost(initialCostInTests.value)
              .asInstanceOf[ErgoLikeContext]
        }
        ctx
      }

      val (evalMode, expectedResult, expectedCost) = if (activatedVersionInTests < VersionContext.JitActivationVersion)
        (AotEvaluationMode, expected.oldResult, expected.verificationCostOpt)
      else {
        val res = expected.newResults(ergoTreeVersionInTests)
        (JitEvaluationMode, res._1, res._1.verificationCost)
      }

      if (expectedResult.value.isSuccess) {
        // we can run both proving and verification
        val prover = new FeatureProvingInterpreter()
        val tree = compiledTree(prover)
        val ctx = ergoCtx(prover, tree, expectedResult.value.get)
        val pr = prover.prove(tree, ctx, fakeMessage).getOrThrow
        val verificationCtx = ctx.withExtension(pr.extension)
        val verifier = new ErgoLikeTestInterpreter()(createIR())
        val res = verifier.verify(tree, verificationCtx, pr, fakeMessage)
        checkExpectedResult(evalMode, res, expectedCost)

        if (expectedCost.isEmpty) {
          val (_, cost) = res.getOrThrow
          // new verification cost expectation is missing, print out actual cost results
          if (feature.evalSettings.printTestVectors) {
            printCostTestVector("Missing New Cost", input, cost.toInt)
          }
        }
      }
    }

    /** Prints the actual cost test vector (when it is not defined). */
    private def printCostTestVector(title: String, input: Any, actualCost: Int) = {
      println(
        s"""--  $title  ----------------------
          |ActivatedVersion: $activatedVersionInTests
          |ErgoTreeVersion: $ergoTreeVersionInTests
          |Input: $input
          |Script: $script
          |Actual New Verification Cost: $actualCost
          |""".stripMargin)
    }

    private def checkEqualResults(res1: Try[VerificationResult], res2: Try[VerificationResult]): Unit = {
      (res1, res2) match {
        case (Success((v1, c1)), Success((v2, c2))) =>
          v1 shouldBe v2
        case (Failure(t1), Failure(t2)) =>
          rootCause(t1) shouldBe rootCause(t2)
        case _ =>
          res1 shouldBe res2
      }
    }

    private def checkExpectedResult(
          evalMode: EvaluationMode,
          res: Try[VerificationResult], expectedCost: Option[Int]): Unit = {
      res match {
        case Success((ok, cost)) =>
          ok shouldBe true
          val verificationCost = cost.toIntExact
          if (expectedCost.isDefined) {
            assertResult(expectedCost.get,
              s"Evaluation Mode: ${evalMode.name}; Actual verify() cost $cost != expected ${expectedCost.get}")(verificationCost)
          }

        case Failure(t) => throw t
      }
    }

  }

  /** A number of times the newF function in each test feature is repeated.
    * In combination with [[sigmastate.eval.Profiler]] it allows to collect more accurate
    * timings for all operations.
    * @see SigmaDslSpecification */
  def nBenchmarkIters: Int = 0

  def warmUpBeforeAllTest(nTotalIters: Int)(block: => Unit) = {
    // each test case is executed nBenchmarkIters times in `check` method
    // so we account for that here
    val nIters = nTotalIters / (nBenchmarkIters + 1)
    repeatAndReturnLast(nIters)(block)
    System.gc()
    Thread.sleep(1000) // let GC to its job before running the tests
  }

  /** Derived class ExistingFeature is used to describe features which don't change from
    * v4.x to v5.0.
    *
    * @tparam A type of an input test data
    * @tparam B type of an output of the feature function
    * @param script            script of the feature function (see Feature trait)
    * @param scalaFunc         feature function written in Scala and used to simulate the behavior
    *                          of the script
    * @param expectedExpr      expected ErgoTree expression to be produced by ErgoScript
    *                          compiler for the given feature-function
    * @param printExpectedExpr if true, print test vectors for expectedExpr
    * @param logScript         if true, log scripts to console
    */
  case class ExistingFeature[A, B](
    script: String,
    scalaFunc: A => B,
    expectedExpr: Option[SValue],
    printExpectedExpr: Boolean = true,
    logScript: Boolean = LogScriptDefault
  )(implicit IR: IRContext, tA: RType[A], tB: RType[B],
             override val evalSettings: EvalSettings) extends Feature[A, B] {

    implicit val cs = compilerSettingsInTests

    val oldImpl = () => func[A, B](script)
    val newImpl = () => funcJit[A, B](script)

    def checkEquality(input: A, logInputOutput: Boolean = false): Try[(B, CostDetails)] = {
      // check the old implementation against Scala semantic function
      val oldRes = checkEq(scalaFunc)(oldF)(input)

      val newRes = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        checkEq(scalaFunc)({ x =>
          var y: (B, CostDetails) = null
          val N = nBenchmarkIters + 1
          cfor(0)(_ < N, _ + 1) { _ =>
            y = newF(x)
          }
          y
        })(input)
      }

      (oldRes, newRes) match {
        case (Success((oldRes, oldDetails)),
              Success((newRes, newDetails))) =>
          newRes shouldBe oldRes
          val oldCost = oldDetails.cost
          val newCost = newDetails.cost
          if (newDetails.cost != oldDetails.cost) {
            assertResult(true,
              s"""
                |New cost should not exceed old cost: (new: $newCost, old:$oldCost)
                |ExistingFeature.checkEquality(
                |  script = "$script",
                |  compiledTree = "${SigmaPPrint(newF.compiledTree, height = 550, width = 150)}"
                |)
                |""".stripMargin
            )(oldCost >= newCost / 20)

            if (evalSettings.isLogEnabled) {
              println(
                s"""Different Costs (new: $newCost, old:$oldCost)
                  |  input = ${SigmaPPrint(input, height = 550, width = 150)}
                  |  script = "$script"
                  |
                  |""".stripMargin)
            }
          }
        case _ =>
          checkResult(rootCause(newRes), rootCause(oldRes), failOnTestVectors = true)
      }

      if (logInputOutput) {
        val scriptComment = if (logScript) " // " + script else ""
        val inputStr = SigmaPPrint(input, height = 550, width = 150)
        val oldResStr = SigmaPPrint(oldRes, height = 550, width = 150)
        println(s"($inputStr, $oldResStr),$scriptComment")
      }

      newRes
    }

    /** Depending on the featureType compares the old and new implementations against
      * semantic function (scalaFunc) on the given input, also checking the given expected result.
      */
    override def checkExpected(input: A, expected: Expected[B]): Unit = {
      // check the old implementation with Scala semantic
      val (oldRes, _) = checkEq(scalaFunc)(oldF)(input).get
      oldRes shouldBe expected.value.get

      if (!(newImpl eq oldImpl)) {
        // check the new implementation with Scala semantic
        val (newRes, _) = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          checkEq(scalaFunc)(newF)(input).get
        }
        newRes shouldBe expected.value.get
      }
    }

    override def testCase(input: A,
                          expectedResult: Try[B],
                          printTestCases: Boolean,
                          failOnTestVectors: Boolean): Unit = {
      val res = checkEquality(input, printTestCases).map(_._1)
      checkResult(res, expectedResult, failOnTestVectors)
    }

    override def verifyCase(input: A,
                            expected: Expected[B],
                            printTestCases: Boolean,
                            failOnTestVectors: Boolean): Unit = {
      val funcRes = checkEquality(input, printTestCases) // NOTE: funcRes comes from newImpl

      checkResult(funcRes.map(_._1), expected.value, failOnTestVectors)

      val newRes = expected.newResults(ergoTreeVersionInTests)
      val expectedTrace = newRes._2.fold(Seq.empty[CostItem])(_.trace)
      if (expectedTrace.isEmpty) {
        // new cost expectation is missing, print out actual cost results
        if (evalSettings.printTestVectors) {
          funcRes.foreach { case (_, newCost) =>
            printCostDetails(script, newCost)
          }
        }
      }
      else {
        // new cost expectation is specified, compare it with the actual result
// TODO mainnet v5.0: uncomment to enable test vectors
//        funcRes.foreach { case (_, newCost) =>
//          if (newCost.trace != expectedTrace) {
//            printCostDetails(script, newCost)
//            newCost.trace shouldBe expectedTrace
//          }
//        }
      }

      checkVerify(input, expected)
    }
  }

  /** Descriptor of a language feature which is changed in v5.0.
    *
    * @tparam A type of an input test data
    * @tparam B type of an output of the feature function
    * @param script            script of the feature function (see Feature trait)
    * @param scalaFunc         feature function written in Scala and used to simulate the behavior
    *                          of the script
    * @param scalaFuncNew      function in Scala to simulate the behavior of the script in v5.0
    * @param expectedExpr      expected ErgoTree expression to be produced by ErgoScript
    *                          compiler for the given feature-function
    * @param printExpectedExpr if true, print test vectors for expectedExpr
    * @param logScript         if true, log scripts to console
    * @param allowNewToSucceed if true, we allow some scripts which fail in v4.x to pass in v5.0.
    *                          Before activation, all ErgoTrees with version < JitActivationVersion are
    *                          processed by v4.x interpreter, so this difference is not a problem.
    *                          After activation, all ErgoTrees with version < JitActivationVersion are
    *                          processed by v5.0 interpreter (which is 90% of nodes) and the rest are
    *                          accepting without check.
    *                          This approach allows to fix bugs in the implementation of
    *                          some of v4.x operations.
    */
  case class ChangedFeature[A: RType, B: RType](
    script: String,
    scalaFunc: A => B,
    override val scalaFuncNew: A => B,
    expectedExpr: Option[SValue],
    printExpectedExpr: Boolean = true,
    logScript: Boolean = LogScriptDefault,
    allowNewToSucceed: Boolean = false
  )(implicit IR: IRContext, override val evalSettings: EvalSettings)
    extends Feature[A, B] {

    implicit val cs = compilerSettingsInTests

    val oldImpl = () => func[A, B](script)
    val newImpl = () => funcJit[A, B](script)

    def checkEquality(input: A, logInputOutput: Boolean = false): Try[(B, CostDetails)] = {
      // check the old implementation against Scala semantic function
      var oldRes: Try[(B, CostDetails)] = null
      if (ergoTreeVersionInTests < VersionContext.JitActivationVersion)
        oldRes = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          checkEq(scalaFunc)(oldF)(input)
        }

      val newRes = {
        // check the new implementation against Scala semantic function
        val newRes = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          checkEq(scalaFuncNew)(newF)(input)
        }
        if (ergoTreeVersionInTests < VersionContext.JitActivationVersion) {
          (oldRes, newRes) match {
            case (_: Failure[_], _: Success[_]) if allowNewToSucceed =>
              // NOTE, we are in ChangedFeature (compare with ExistingFeature)
              // this is the case when old v4.x version fails with exception (e.g. due to AOT costing),
              // but the new v5.0 produces result. (See property("Option fold workaround method"))
              // Thus, we allow some scripts which fail in v4.x to pass in v5.0.
              // see ScalaDoc for allowNewToSucceed
            case _ =>
              val inputStr = SigmaPPrint(input, height = 550, width = 150)
              checkResult(oldRes.map(_._1), newRes.map(_._1), true,
                s"ChangedFeature.checkEquality($inputStr): use allowNewToSucceed=true to allow v5.0 to succeed when v4.x fails")
          }
        }
        newRes
      }
      if (logInputOutput && oldRes != null) {
        val inputStr = SigmaPPrint(input, height = 550, width = 150)
        val oldResStr = SigmaPPrint(oldRes, height = 550, width = 150)
        val scriptComment = if (logScript) " // " + script else ""
        println(s"($inputStr, $oldResStr),$scriptComment")
      }
      newRes
    }

    /** compares the old and new implementations against
      * semantic function (scalaFunc) on the given input, also checking the given expected result.
      */
    override def checkExpected(input: A, expected: Expected[B]): Unit = {
      // check the old implementation with Scala semantic
      val (oldRes, _) = checkEq(scalaFunc)(oldF)(input).get
      oldRes shouldBe expected.value.get

      if (!(newImpl eq oldImpl)) {
        // check the new implementation with Scala semantic
        val (newRes, _) = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          checkEq(scalaFuncNew)(newF)(input).get
        }
        val newExpectedRes = expected.newResults(ergoTreeVersionInTests)
        newRes shouldBe newExpectedRes._1.value.get
      }
    }

    override def testCase(input: A,
                          expectedResult: Try[B],
                          printTestCases: Boolean,
                          failOnTestVectors: Boolean): Unit = {
      val res = checkEquality(input, printTestCases).map(_._1)
      checkResult(res, expectedResult, failOnTestVectors)
    }

    override def verifyCase(input: A,
                            expected: Expected[B],
                            printTestCases: Boolean,
                            failOnTestVectors: Boolean): Unit = {
      checkEquality(input, printTestCases)
      checkVerify(input, expected)
    }
  }

  /** Derived class NewFeature is used to describe features which should be introduced
    * from specific version.
    * This in not yet implemented and will be finished in v6.0.
    * In v5.0 is only checks that some features are NOT implemented, i.e. work for
    * negative tests.
    */
  case class NewFeature[A: RType, B: RType](
    script: String,
    override val scalaFuncNew: A => B,
    expectedExpr: Option[SValue],
    printExpectedExpr: Boolean = true,
    logScript: Boolean = LogScriptDefault
  )(implicit IR: IRContext, override val evalSettings: EvalSettings)
    extends Feature[A, B] {
    override def scalaFunc: A => B = { x =>
      sys.error(s"Semantic Scala function is not defined for old implementation: $this")
    }
    implicit val cs = compilerSettingsInTests

    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // funcJit[A, B](script) // TODO v6.0 (16h): use actual new implementation here

    /** In v5.x this method just checks the old implementations fails on the new feature. */
    override def checkEquality(input: A, logInputOutput: Boolean = false): Try[(B, CostDetails)] = {
      val oldRes = Try(oldF(input))
      oldRes.isFailure shouldBe true
      if (!(newImpl eq oldImpl)) {
        val newRes = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          checkEq(scalaFuncNew)(newF)(input)
        }
      }
      oldRes
    }

    override def checkExpected(input: A, expected: Expected[B]): Unit = {
      Try(oldF(input)).isFailure shouldBe true
      if (!(newImpl eq oldImpl)) {
        val (newRes, _) = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          checkEq(scalaFuncNew)(newF)(input).get
        }
        val newExpectedRes = expected.newResults(ergoTreeVersionInTests)
        newRes shouldBe newExpectedRes._1.value.get
      }
    }

    override def testCase(input: A,
                          expectedResult: Try[B],
                          printTestCases: Boolean,
                          failOnTestVectors: Boolean): Unit = {
      val res = checkEquality(input, printTestCases).map(_._1)
      res.isFailure shouldBe true
      Try(scalaFuncNew(input)) shouldBe expectedResult
    }

    override def verifyCase(input: A,
                            expected: Expected[B],
                            printTestCases: Boolean,
                            failOnTestVectors: Boolean): Unit = {
      val funcRes = checkEquality(input, printTestCases)
      funcRes.isFailure shouldBe true
      Try(scalaFunc(input)) shouldBe expected.value
    }
  }

  /** Represents expected result, verification cost and costing trace for a single
    * interpreter run.
    * @param value expected results returned by feature function (and the corresponding Scala function)
    * @param verificationCost  expected cost value of the verification execution
    */
  case class ExpectedResult[+A](value: Try[A], verificationCost: Option[Int])

  /** Represents expected results (aka test vectors) for a single feature test case.
    * @param oldResult expected results returned by v4.x interpreter
    * @see [[testCases]]
    */
  case class Expected[+A](oldResult: ExpectedResult[A]) {
    /** Expected results returned by v4.x interpreter on the feature function. */
    def value: Try[A] = oldResult.value

    /** Expected verification cost returned by v4.x interpreter on the feature function. */
    def verificationCostOpt: Option[Int] = oldResult.verificationCost

    /** One expected result for each supported ErgoTree version.
      * By default (and for most operations) the new values are equal to the old value for
      * all versions, which means there are no changes in operation semantics.
      * However, new verification costs are different for the old ones. To simplify
      * augmentation of test cases with new test vectors, the default value of None
      * signals that the test vectors should be defined. The test harness code can print
      * suggestions for new test vectors.
      */
    final def defaultNewResults: Seq[(ExpectedResult[A], Option[CostDetails])] = {
      val n = VersionContext.MaxSupportedScriptVersion + 1
      // NOTE: by default, tests vectors for both verification cost and cost details are not defined
      val res = ExpectedResult(oldResult.value, None)
      Array.fill(n)((res, None))
    }

    /** One expected result for each supported ErgoTree version.
      * This expectations are applied to v5.+ interpreter (i.e. new JITC based implementation).
      */
    val newResults: Seq[(ExpectedResult[A], Option[CostDetails])] = defaultNewResults
  }

  object Expected {
    /** Used when exception is expected.
      * @param error expected during execution
      */
    def apply[A](error: Throwable) = new Expected[A](ExpectedResult(Failure(error), None))

    /** Used when the old and new value and costs are the same for all versions.
      * @param value expected result of tested function
      * @param cost  expected verification cost
      */
    def apply[A](value: Try[A], cost: Int): Expected[A] =
      new Expected(ExpectedResult(value, Some(cost)))

    /** Used when the old and new value and costs are the same for all versions.
      * @param value expected result of tested function
      * @param cost  expected verification cost
      * @param expectedDetails expected cost details for all versions
      */
    def apply[A](value: Try[A], cost: Int, expectedDetails: CostDetails): Expected[A] =
      new Expected(ExpectedResult(value, Some(cost))) {
        override val newResults = defaultNewResults.map { case (r, _) =>
          (r, Some(expectedDetails))
        }
      }

    /** Used when the old and new value and costs are the same for all versions.
      *
      * @param value           expected result of tested function
      * @param cost            expected verification cost
      * @param expectedDetails expected cost details for all versions
      * @param expectedNewCost expected new verification cost for all versions
      */
    def apply[A](value: Try[A],
                 cost: Int,
                 expectedDetails: CostDetails,
                 expectedNewCost: Int): Expected[A] =
      new Expected(ExpectedResult(value, Some(cost))) {
        override val newResults = defaultNewResults.map {
          case (ExpectedResult(v, _), _) =>
            (ExpectedResult(v, Some(expectedNewCost)), Some(expectedDetails))
        }
      }

    /** Used when operation semantics changes in new versions. For those versions expected
      * test vectors can be specified.
      *
      * @param value               value returned by feature function v4.x
      * @param cost                expected cost value of the verification execution (v4.x)
      * @param newDetails          expected cost details for all versions
      * @param newCost             expected new verification cost for all versions
      * @param newVersionedResults new results returned by each changed feature function in
      *                            v5.+ for each ErgoTree version.
      */
    def apply[A](value: Try[A], cost: Int,
                 newDetails: CostDetails, newCost: Int,
                 newVersionedResults: Seq[(Int, (ExpectedResult[A], Option[CostDetails]))]): Expected[A] =
      new Expected[A](ExpectedResult(value, Some(cost))) {
        override val newResults = {
          val commonNewResults = defaultNewResults.map {
            case (res, _) =>
              (ExpectedResult(res.value, Some(newCost)), Option(newDetails))
          }
          commonNewResults.updateMany(newVersionedResults)
        }
      }
  }

  /** Describes existing language feature which should be equally supported in both
    * Script v1 (v3.x and v4.x releases) and Script v2 (v5.x) versions of the language.
    * A behavior of the given `script` is tested against semantic function.
    *
    * @param scalaFunc    semantic function for both v1 and v2 script interpretations
    * @param script       the script to be tested against semantic function
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def existingFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String,
       expectedExpr: SValue = null)
      (implicit IR: IRContext, evalSettings: EvalSettings): Feature[A, B] = {
    ExistingFeature(
      script, scalaFunc, Option(expectedExpr))
  }

  /** Describes existing language feature which should be differently supported in both
    * Script v1 (v3.x and v4.x releases) and Script v2 (v5.x) versions of the language.
    * The behavior of the given `script` is tested against the given semantic functions.
    *
    * @param scalaFunc    semantic function of v1 language version
    * @param scalaFuncNew semantic function of v2 language version
    * @param script       the script to be tested against semantic functions
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def changedFeature[A: RType, B: RType]
      (scalaFunc: A => B,
       scalaFuncNew: A => B,
       script: String,
       expectedExpr: SValue = null,
       allowNewToSucceed: Boolean = false)
      (implicit IR: IRContext, evalSettings: EvalSettings): Feature[A, B] = {
    ChangedFeature(script, scalaFunc, scalaFuncNew, Option(expectedExpr),
      allowNewToSucceed = allowNewToSucceed)
  }

  /** Describes a NEW language feature which must NOT be supported in v4 and
    * must BE supported in v5 of the language.
    *
    * @param scalaFunc    semantic function which defines expected behavior of the given script
    * @param script       the script to be tested against semantic function
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def newFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext, es: EvalSettings): Feature[A, B] = {
    NewFeature(script, scalaFunc, Option(expectedExpr))
  }

  val contextGen: Gen[Context] = ergoLikeContextGen.map(c => c.toSigmaContext(isCost = false))
  implicit val arbContext: Arbitrary[Context] = Arbitrary(contextGen)

  /** NOTE, this should be `def` to allow overriding of generatorDrivenConfig in derived Spec classes. */
  def DefaultMinSuccessful: MinSuccessful = MinSuccessful(generatorDrivenConfig.minSuccessful)

  val PrintTestCasesDefault: Boolean = false // true
  val FailOnTestVectorsDefault: Boolean = true

  private def checkResult[B](res: Try[B], expectedRes: Try[B], failOnTestVectors: Boolean, hint: String = ""): Unit = {
    (res, expectedRes) match {
      case (Failure(exception), Failure(expectedException)) =>
        rootCause(exception).getClass shouldBe expectedException.getClass
      case _ =>
        if (failOnTestVectors) {
          val actual = rootCause(res)
          if (expectedRes != actual) {
            val actualPrinted = SigmaPPrint(actual, height = 150).plainText
            val expectedPrinted = SigmaPPrint(expectedRes, height = 150).plainText
            assert(false, s"$hint\nActual: $actualPrinted;\nExpected: $expectedPrinted\n")
          }
        }
        else {
          if (expectedRes != res) {
            print("\nSuggested Expected Result: ")
            SigmaPPrint.pprintln(res, height = 150)
          }
        }
    }
  }

  /** Test the given test cases with expected results (aka test vectors).
    * NOTE, in some cases (such as Context, Box, etc) sample generation is time consuming, so it
    * makes sense to factor it out.
    * @param preGeneratedSamples  optional pre-generated samples to reduce execution time
    */
  def testCases[A: Ordering : Arbitrary : ClassTag, B]
      (cases: Seq[(A, Try[B])],
       f: Feature[A, B],
       printTestCases: Boolean = PrintTestCasesDefault,
       failOnTestVectors: Boolean = FailOnTestVectorsDefault,
       preGeneratedSamples: Option[Seq[A]] = None): Unit = {
    System.gc() // force GC to avoid occasional OOM exception
    val table = Table(("x", "y"), cases:_*)
    forAll(table) { (x: A, expectedRes: Try[B]) =>
      f.testCase(x, expectedRes, printTestCases, failOnTestVectors)
    }
    test(preGeneratedSamples, f, printTestCases)
  }

  /** Test the given test cases with expected results AND costs (aka test vectors).
    * For all Success cases `f.checkVerify` is executed to exercise the whole
    * `Interpreter.verify` execution and assert the expected cost.
    *
    * NOTE, in some cases (such as Context, Box, etc) sample generation is time consuming, so it
    * makes sense to factor it out.
    *
    * @param preGeneratedSamples  optional pre-generated samples to reduce execution time
    *                             if None, then the given Arbitrary is used to generate samples
    */
  def verifyCases[A: Ordering : Arbitrary : ClassTag, B]
      (cases: Seq[(A, Expected[B])],
       f: Feature[A, B],
       printTestCases: Boolean = PrintTestCasesDefault,
       failOnTestVectors: Boolean = FailOnTestVectorsDefault,
       preGeneratedSamples: Option[Seq[A]] = None): Unit = {

    val table = Table(("x", "y"), cases:_*)
    forAll(table) { (x: A, expectedRes: Expected[B]) =>
      f.verifyCase(x, expectedRes, printTestCases, failOnTestVectors)
    }
    test(preGeneratedSamples, f, printTestCases)
  }

  def verifyCasesMany[A: Ordering : Arbitrary : ClassTag, B]
      (cases: Seq[(A, Expected[B])],
       features: Seq[Feature[A, B]],
       printTestCases: Boolean = PrintTestCasesDefault,
       failOnTestVectors: Boolean = FailOnTestVectorsDefault,
       preGeneratedSamples: Option[Seq[A]] = None): Unit = {
    features.foreach { f =>
      verifyCases(cases, f, printTestCases, failOnTestVectors, preGeneratedSamples)
    }
  }

  case class MeasureInfo[A](input: A, iteration: Int, nIters: Int, measuredTime: Long)

  type MeasureFormatter[A] = MeasureInfo[A] => String

  def benchmarkCases[A: Ordering : Arbitrary : ClassTag, B]
      (cases: Seq[A], f: Feature[A, B], nIters: Int, formatter: MeasureFormatter[A])
      (implicit IR: IRContext, evalSettings: EvalSettings): Seq[Long] = {
    val fNew = f.newF
    implicit val tA = fNew.tA
    implicit val tB = fNew.tB
    implicit val cs = defaultCompilerSettings
    val func = funcJitFast[A, B](f.script)
    val noTraceSettings = evalSettings.copy(
      isMeasureOperationTime = false,
      costTracingEnabled = false)
    val funcNoTrace = funcJitFast[A, B](f.script)(tA, tB, IR, noTraceSettings, cs)
    var iCase = 0
    val (res, total) = BenchmarkUtil.measureTimeNano {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        cases.map { x =>
          assert(func(x)._1 == f.newF(x)._1)
          iCase += 1
          def benchmarkCase(func: CompiledFunc[A,B], printOut: Boolean) = {
            val (_, t) = BenchmarkUtil.measureTimeNano {
              cfor(0)(_ < nIters, _ + 1) { i =>
                val res = func(x)
              }
            }
            if (printOut) {
              val info = MeasureInfo(x, iCase, nIters, t)
              val out = formatter(info)
              println(out)
            }
            t
          }
          benchmarkCase(func, printOut = false)
          benchmarkCase(funcNoTrace, printOut = true)
        }
      }
    }
    println(s"Total time: ${total / 1000000} msec")
    res
  }

  /** Generate samples in sorted order.
    * @param gen  generator to be used for sample generation
    * @param config generation configuration
    * @return array-backed ordered sequence of samples
    */
  def genSamples[A: Ordering: ClassTag](gen: Gen[A], config: PropertyCheckConfigParam): Seq[A] = {
    implicit val arb: Arbitrary[A] = Arbitrary(gen)
    genSamples[A](config)
  }

  /** Generate samples in sorted order.
    * @param config generation configuration
    * @return array-backed ordered sequence of samples
    */
  def genSamples[A: Arbitrary: Ordering: ClassTag](config: PropertyCheckConfigParam): Seq[A] = {
    genSamples[A](config, Some(implicitly[Ordering[A]]))
  }

  /** Generate samples with optional sorted order.
    * @param config generation configuration
    * @param optOrd optional ordering of the generated samples in the resuting sequence
    * @return array-backed ordered sequence of samples
    */
  def genSamples[A: Arbitrary: ClassTag](config: PropertyCheckConfigParam, optOrd: Option[Ordering[A]]): Seq[A] = {
    val inputs = scala.collection.mutable.ArrayBuilder.make[A]()
    forAll(config) { x: A =>
      inputs += x
    }
    optOrd.fold(inputs.result())(implicit ord => inputs.result.sorted)
  }

  /** Test the given samples or generate new samples using the given Arbitrary.
    * For each sample `f.checkEquality` is executed.
    */
  def test[A: Arbitrary: Ordering : ClassTag, B]
      (preGeneratedSamples: Option[Seq[A]],
       f: Feature[A, B],
       printTestCases: Boolean): Unit = {
    // either get provides or generate new samples (in sorted order)
    val samples = preGeneratedSamples.getOrElse(genSamples[A](DefaultMinSuccessful))

    // then tests them, this will output a nice log of test cases (provided printTestCases == true)
    samples.foreach { x =>
      f.checkEquality(x, printTestCases)
    }
  }

  def test[A: Arbitrary : Ordering : ClassTag, B](samples: Seq[A], f: Feature[A, B]): Unit = {
    test(Some(samples), f, PrintTestCasesDefault)
  }

  def test[A: Arbitrary : Ordering : ClassTag, B]
      (f: Feature[A, B],
       printTestCases: Boolean = PrintTestCasesDefault): Unit = {
    test(None, f, printTestCases)
  }

  /** Represents generated samples for the type `A`. */
  abstract class Sampled[A] {
    /** An instance of [[Arbitrary]] which is used to generate samples. */
    def arbitrary: Arbitrary[A]

    /** Return a sequence of samples. */
    def samples: Seq[A]
  }

  /** Default implementation of [[Sampled]]. */
  case class SampledData[A](samples: Seq[A])(implicit val arbitrary: Arbitrary[A])
      extends Sampled[A]

  /** Arbitrary instance for each type descriptor. */
  private val arbitraryCache = new mutable.HashMap[RType[_], Arbitrary[_]]

  /** Lookup [[Arbitrary]] in the cache by type descriptor or create new instance and
    * add it to the cache.
    */
  def lookupArbitrary[A](t: RType[A]): Arbitrary[A] = (arbitraryCache.get(t) match {
    case Some(arb) => arb
    case None =>
      val arb = (t match {
        case BooleanType => arbBool
        case ByteType => arbByte
        case ShortType => arbShort
        case IntType => arbInt
        case LongType => arbLong
        case BigIntRType => arbBigInt
        case GroupElementRType => arbGroupElement
        case SigmaPropRType => arbSigmaProp
        case BoxRType => arbBox
        case PreHeaderRType => arbPreHeader
        case HeaderRType => arbHeader
        case AvlTreeRType => arbAvlTree
        case AnyType => arbAnyVal
        case UnitType => arbUnit
        case p: PairType[a, b] =>
          implicit val arbA: Arbitrary[a] = lookupArbitrary[a](p.tFst)
          implicit val arbB: Arbitrary[b] = lookupArbitrary[b](p.tSnd)
          arbTuple2[a,b]
        case opt: OptionType[a] =>
          Arbitrary(frequency((5, None), (5, for (x <- lookupArbitrary(opt.tA).arbitrary) yield Some(x))))
        case coll: CollType[a] =>
          implicit val elemArb: Arbitrary[a] = lookupArbitrary(coll.tItem)
          implicit val elemT: RType[a] = coll.tItem
          Arbitrary(collGen[a])
      }).asInstanceOf[Arbitrary[A]]
      arbitraryCache.put(t, arb)
      arb
  }).asInstanceOf[Arbitrary[A]]

  /** Update cached [[Arbitrary]] with a new instance, which generates its data from the
    * given [[Sampled]] instance (randomly selects oneOf sample).
    */
  def updateArbitrary[A](t: RType[A], sampled: Sampled[A]) = {
    t match {
      case BigIntRType | GroupElementRType | SigmaPropRType |
           BoxRType | PreHeaderRType | HeaderRType | AvlTreeRType |
           _: CollType[_] | _: PairType[_,_] | _: OptionType[_] =>
        val newArb = Arbitrary(Gen.oneOf(sampled.samples))
        arbitraryCache.put(t, newArb)
      case _ =>
    }
  }

  /** Sampled test data from each data type. */
  private val sampledCache = new mutable.HashMap[RType[_], Sampled[_]]

  /** Lookup [[Sampled]] test data in the cache by type descriptor or create a new instance and
    * add it to the cache.
    */
  implicit def lookupSampled[A](implicit t: RType[A]): Sampled[A] = (sampledCache.get(t) match {
    case Some(s) => s
    case _ =>
      implicit val tagA = t.classTag
      implicit val arb = lookupArbitrary(t)
      val res = new SampledData[A](
        samples = genSamples[A](DefaultMinSuccessful, None))
      sampledCache.put(t, res)
      updateArbitrary(t, res)
      res
  }).asInstanceOf[Sampled[A]]

  /** Call this function to prepare samples for the given type.
    * They can later be retrieved using `lookupSampled`. */
  def prepareSamples[A](implicit t: RType[A]) = {
    lookupSampled[A]
  }


  /** Helper implementation for ordering samples. */
  trait GroupElementOrdering extends Ordering[GroupElement] {
    /** Compares `x: ECPoint` string representation with `y: ECPoint` string for order.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: GroupElement, y: GroupElement): Int = {
      SigmaDsl.toECPoint(x).toString.compareTo(SigmaDsl.toECPoint(y).toString)
    }
  }
  implicit object GroupElementOrdering extends GroupElementOrdering

  /** Helper implementation for ordering samples. */
  trait AvlTreeOrdering extends Ordering[AvlTree] {
    /** Compares this `x: AvlTree` string representation with `y: AvlTree` string for order.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: AvlTree, y: AvlTree): Int = {
      x.toString.compareTo(y.toString)
    }
  }
  implicit object AvlTreeOrdering extends AvlTreeOrdering

  /** Helper implementation for ordering samples. */
  class CollOrdering[T: Ordering] extends Ordering[Coll[T]] {
    val O = Ordering[Iterable[T]]

    /** Compares this `x: Coll` with `y: Coll` using Ordering for underlying Array.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Coll[T], y: Coll[T]): Int = {
      O.compare(x.toArray, y.toArray)
    }
  }
  implicit def collOrdering[T: Ordering]: Ordering[Coll[T]] = new CollOrdering[T]

  /** Helper implementation for ordering samples. */
  trait BoxOrdering extends Ordering[Box] {
    /** Compares this `x: Box` string representation with `y: Box` string for order.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Box, y: Box): Int = {
      x.toString.compareTo(y.toString)
    }
  }
  implicit object BoxOrdering extends BoxOrdering

  /** Helper implementation for ordering samples. */
  trait PreHeaderOrdering extends Ordering[PreHeader] {
    /** Compares this `x: PreHeader` with `y: PreHeader` using block height.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: PreHeader, y: PreHeader): Int = {
      Ordering.Int.compare(x.height, y.height)
    }
  }
  implicit object PreHeaderOrdering extends PreHeaderOrdering

  /** Helper implementation for ordering samples. */
  trait HeaderOrdering extends Ordering[Header] {
    /** Compares this `x: Header` with `y: Header` using block height.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Header, y: Header): Int = {
      Ordering.Int.compare(x.height, y.height)
    }
  }
  implicit object HeaderOrdering extends HeaderOrdering

  /** Helper implementation for ordering samples. */
  trait ContextOrdering extends Ordering[Context] {
    val O: Ordering[(Int, Coll[Byte])] = Ordering[(Int, Coll[Byte])]

    /** Compares this `x: Context` with `y: Context` using block height and SELF.id.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Context, y: Context): Int = {
      O.compare((x.HEIGHT, x.SELF.id), (y.HEIGHT, y.SELF.id))
    }
  }
  implicit object ContextOrdering extends ContextOrdering

  /** Helper implementation for ordering samples. */
  trait SigmaPropOrdering extends Ordering[SigmaProp] {
    /** Compares this `x: SigmaProp` with `y: SigmaProp` using string representation.
      * @return a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: SigmaProp, y: SigmaProp): Int = {
      x.toString.compareTo(y.toString)
    }
  }
  implicit object SigmaPropOrdering extends SigmaPropOrdering
}
