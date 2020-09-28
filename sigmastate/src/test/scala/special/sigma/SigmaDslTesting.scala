package special.sigma

import java.util

import org.ergoplatform.{ErgoAddressEncoder, ErgoLikeTransaction, ErgoLikeContext, ErgoLikeInterpreter, Input, ErgoBox, DataInput, ErgoScriptPredef}
import org.scalatest.prop.PropertyChecks
import sigmastate.interpreter.Interpreter.ScriptEnv
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{PropSpec, Matchers}

import scala.util.{Success, Failure, Try}
import sigmastate.Values.{Constant, SValue, ConstantNode, ByteArrayConstant, IntConstant, ErgoTree}
import scalan.RType
import scalan.util.Extensions._
import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec, ContractSpec}
import org.ergoplatform.validation.{ValidationRules, SigmaValidationSettings}
import sigmastate.{eval, SSigmaProp, SType}
import SType.AnyOps
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import sigmastate.basics.DLogProtocol.{ProveDlog, DLogProverInput}
import sigmastate.basics.{SigmaProtocol, SigmaProtocolPrivateInput, SigmaProtocolCommonInput}
import sigmastate.eval.{CompiletimeIRContext, Evaluation, CostingBox, SigmaDsl, IRContext, CostingDataContext}
import sigmastate.eval.Extensions._
import sigmastate.utils.Helpers._
import sigmastate.lang.Terms.ValueOps
import sigmastate.helpers.{ErgoLikeContextTesting, SigmaPPrint}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.{ProverResult, ContextExtension, ProverInterpreter}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.{DeserializeContext, DeserializeRegister}
import special.collection.Coll

import scala.math.Ordering
import scala.reflect.ClassTag

class SigmaDslTesting extends PropSpec
    with PropertyChecks
    with Matchers
    with SigmaTestingData with SigmaContractSyntax
    with SigmaTypeGens { suite =>

  lazy val spec: ContractSpec = TestContractSpec(suite)(new TestingIRContext)

  override def contractEnv: ScriptEnv = Map()

  def createIR(): IRContext = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
    override val okMeasureOperationTime: Boolean = true
  }

  def checkEq[A,B](scalaFunc: A => B)(g: A => (B, Int)): A => Try[(B, Int)] = { x: A =>
    val b1 = Try(scalaFunc(x)); val b2 = Try(g(x))
    (b1, b2) match {
      case (Success(b1), res @ Success((b2, _))) =>
        assert(b1 == b2)
        res
      case (Failure(t1), res @ Failure(t2)) =>
        val c1 = rootCause(t1).getClass
        val c2 = rootCause(t2).getClass
        c1 shouldBe c2
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

  class FeatureProvingInterpreter extends ErgoLikeInterpreter with ProverInterpreter {
    override type CTX = ErgoLikeContext

    override def createIR(): IRContext = new CompiletimeIRContext

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

  /** Type of the language feature to be tested. */
  sealed trait FeatureType
  case object ExistingFeature extends FeatureType
  case object AddedFeature extends FeatureType

  val LogScriptDefault: Boolean = false

  /** Test case descriptor of the language feature.
    *
    * @param featureType   type of the language feature
    * @param scalaFunc    feature semantic given by scala code
    * @param expectedExpr expression which represents the test case code
    * @param oldImpl      function that executes the feature using v3 interpreter implementation
    * @param newImpl      function that executes the feature using v4 interpreter implementation
    */
  case class FeatureTest[A, B](featureType: FeatureType,
                               script: String,
                               scalaFunc: A => B,
                               expectedExpr: Option[SValue],
                               oldImpl: () => CompiledFunc[A, B],
                               newImpl: () => CompiledFunc[A, B],
                               printExpectedExpr: Boolean = true,
                               logScript: Boolean = LogScriptDefault
                              ) {
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
          if (cf.expr != e) {
            printSuggestion(cf)
            cf.expr shouldBe e
          }
        case None if printExpectedExpr =>
          printSuggestion(cf)
      }
      true
    }

    /** v3 implementation*/
    private var _oldF: CompiledFunc[A, B] = _
    def oldF: CompiledFunc[A, B] = {
      if (_oldF == null) {
        _oldF = oldImpl()
        checkExpectedExprIn(_oldF)
      }
      _oldF
    }

    /** v4 implementation*/
    private var _newF: CompiledFunc[A, B] = _
    def newF: CompiledFunc[A, B] = {
      if (_newF == null) {
        _newF = newImpl()
        checkExpectedExprIn(_newF)
      }
      _newF
    }

    /** Depending on the featureType compares the old and new implementations against
      * semantic function (scalaFunc) on the given input.
      * @param input  data which is used to execute feature
      * @return result of feature execution */
    def checkEquality(input: A, logInputOutput: Boolean = false): Try[(B, Int)] = featureType match {
      case ExistingFeature =>
        // check both implementations with Scala semantic
        val oldRes = checkEq(scalaFunc)(oldF)(input)

        if (!(newImpl eq oldImpl)) {
          val newRes = checkEq(scalaFunc)(newF)(input)
          newRes shouldBe oldRes
        }
        if (logInputOutput)
          println(s"(${SigmaPPrint(input, height = 550, width = 150)}, ${SigmaPPrint(oldRes, height = 550, width = 150)}),${if (logScript) " // " + script else ""}")
        oldRes
      case AddedFeature =>
        val oldRes = Try(oldF(input))
        oldRes.isFailure shouldBe true
        if (!(newImpl eq oldImpl)) {
          val newRes = checkEq(scalaFunc)(newF)(input)
          newRes shouldBe oldRes
        }
        oldRes
    }

    /** Depending on the featureType compares the old and new implementations against
      * semantic function (scalaFunc) on the given input, also checking the given expected result.
      */
    def checkExpected(input: A, expectedResult: B): Unit = {
      featureType match {
        case ExistingFeature =>
          // check both implementations with Scala semantic
          val (oldRes, _) = checkEq(scalaFunc)(oldF)(input).get
          oldRes shouldBe expectedResult

          if (!(newImpl eq oldImpl)) {
            val (newRes, _) = checkEq(scalaFunc)(newF)(input).get
            newRes shouldBe expectedResult
          }

        case AddedFeature =>
          Try(oldF(input)).isFailure shouldBe true
          if (!(newImpl eq oldImpl)) {
            val (newRes, _) = checkEq(scalaFunc)(newF)(input).get
            newRes shouldBe expectedResult
          }
      }
    }

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
        extension, validationSettings, costLimit, initCost)
    }

    /** Executes the default feature verification wrapper script using:
      * 1) the given input
      * 2) the given expected intermediate result
      * 3) the total expected execution cost of the verification
      */
    def checkVerify(input: A, expectedRes: B, expectedCost: Int): Unit = {
      val tpeA = Evaluation.rtypeToSType(oldF.tA)
      val tpeB = Evaluation.rtypeToSType(oldF.tB)

      val prover = new FeatureProvingInterpreter()

      // Create synthetic ErgoTree which uses all main capabilities of evaluation machinery.
      // 1) first-class functions (lambdas); 2) Context variables; 3) Registers; 4) Equality
      // for all types; 5) Embedding of boolean to SigmaProp; 6) Sigma propositions (&&, ||, AtLeast)
      // 7) Deserialization from SELF and Context
      // Every language Feature is tested as part of this wrapper script.
      // Inclusion of all the features influences the expected cost estimation values
      val compiledTree = {
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
          Seq(
            pkAlice,
            DeserializeRegister(ErgoBox.R5, SSigmaProp),  // deserialize pkBob
            DeserializeContext(2, SSigmaProp)))           // deserialize pkCarol
        ErgoTree.fromProposition(sigmastate.SigmaOr(prop, multisig))
      }

      val pkBobBytes = ValueSerializer.serialize(prover.pubKeys(1).toSigmaProp)
      val pkCarolBytes = ValueSerializer.serialize(prover.pubKeys(2).toSigmaProp)
      val newRegisters = Map(
        ErgoBox.R4 -> Constant[SType](expectedRes.asInstanceOf[SType#WrappedType], tpeB),
        ErgoBox.R5 -> ByteArrayConstant(pkBobBytes)
      )

      val ergoCtx = input match {
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
            initCost = 0L
          )

        case _ =>
          ErgoLikeContextTesting.dummy(
            createBox(0, compiledTree, additionalRegisters = newRegisters)
          ).withBindings(
              1.toByte -> Constant[SType](input.asInstanceOf[SType#WrappedType], tpeA),
              2.toByte -> ByteArrayConstant(pkCarolBytes)
            ).asInstanceOf[ErgoLikeContext]
      }

      val pr = prover.prove(compiledTree, ergoCtx, fakeMessage).getOrThrow

      implicit val IR: IRContext = createIR()

      val verifier = new ErgoLikeInterpreter() {
        type CTX = ErgoLikeContext
        override def createIR(): IRContext = suite.createIR()
      }

      val verificationCtx = ergoCtx.withExtension(pr.extension)

      val vres = verifier.verify(compiledTree, verificationCtx, pr, fakeMessage)
      vres match {
        case Success((ok, cost)) =>
          ok shouldBe true
          val verificationCost = cost.toIntExact
// NOTE: you can uncomment this line and comment the assertion in order to
// simplify adding new test vectors for cost estimation
//          if (expectedCost != verificationCost) {
//            println(s"Script: $script")
//            println(s"Cost: $verificationCost\n")
//          }
          assertResult(expectedCost,
            s"Actual verify() cost $cost != expected $expectedCost")(verificationCost)

        case Failure(t) => throw t
      }
    }
  }
  object FeatureTest {
    /** Cost of the feature verify script.
      * @see checkVerify() */
    val VerifyScriptCost = 6317
  }

  /** Represents expected result of successful feature test exectuion.
    * @param value value returned by feature function (and the corresponding Scala function)
    * @param cost  expected cost value of the verification execution
    * @see [[testCases]]
    */
  case class Expected[+A](value: A, cost: Int)

  /** Describes existing language feature which should be equally supported in both v3 and
    * v4 of the language.
    *
    * @param scalaFunc    semantic function which defines expected behavior of the given script
    * @param script       the script to be tested against semantic function
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def existingFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext): FeatureTest[A, B] = {
    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // TODO HF: use actual new implementation here
    FeatureTest(ExistingFeature, script, scalaFunc, Option(expectedExpr), oldImpl, newImpl)
  }

  /** Describes a NEW language feature which must NOT be supported in v3 and
    * must BE supported in v4 of the language.
    *
    * @param scalaFunc    semantic function which defines expected behavior of the given script
    * @param script       the script to be tested against semantic function
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def newFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext): FeatureTest[A, B] = {
    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // TODO HF: use actual new implementation here
    FeatureTest(AddedFeature, script, scalaFunc, Option(expectedExpr), oldImpl, newImpl)
  }

  val contextGen: Gen[Context] = ergoLikeContextGen.map(c => c.toSigmaContext(isCost = false))
  implicit val arbContext: Arbitrary[Context] = Arbitrary(contextGen)

  /** NOTE, this should be `def` to allow overriding of generatorDrivenConfig in derived Spec classes. */
  def DefaultMinSuccessful: MinSuccessful = MinSuccessful(generatorDrivenConfig.minSuccessful)

  val PrintTestCasesDefault: Boolean = false
  val FailOnTestVectorsDefault: Boolean = true

  private def checkResult[B](res: Try[B], expectedRes: Try[B], failOnTestVectors: Boolean): Unit = {
    (res, expectedRes) match {
      case (Failure(exception), Failure(expectedException)) =>
        rootCause(exception).getClass shouldBe expectedException.getClass
      case _ =>
        if (failOnTestVectors) {
          assertResult(expectedRes, s"Actual: ${SigmaPPrint(res, height = 150).plainText}")(res)
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
       f: FeatureTest[A, B],
       printTestCases: Boolean = PrintTestCasesDefault,
       failOnTestVectors: Boolean = FailOnTestVectorsDefault,
       preGeneratedSamples: Option[Seq[A]] = None): Unit = {

    val table = Table(("x", "y"), cases:_*)
    forAll(table) { (x: A, expectedRes: Try[B]) =>
      val res = f.checkEquality(x, printTestCases).map(_._1)

      // TODO HF: remove this `if` once newImpl is implemented
      f.featureType match {
        case ExistingFeature =>
          checkResult(res, expectedRes, failOnTestVectors)
        case AddedFeature =>
          res.isFailure shouldBe true
          Try(f.scalaFunc(x)) shouldBe expectedRes
      }
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
      (cases: Seq[(A, Try[Expected[B]])],
       f: FeatureTest[A, B],
       printTestCases: Boolean = PrintTestCasesDefault,
       failOnTestVectors: Boolean = FailOnTestVectorsDefault,
       preGeneratedSamples: Option[Seq[A]] = None): Unit = {

    val table = Table(("x", "y"), cases:_*)
    forAll(table) { (x: A, expectedRes: Try[Expected[B]]) =>
      val funcRes = f.checkEquality(x, printTestCases)

      val expectedResValue = expectedRes.map(_.value)
      // TODO HF: remove this `match` once newImpl is implemented
      f.featureType match {
        case ExistingFeature =>
          checkResult(funcRes.map(_._1), expectedResValue, failOnTestVectors)

          (funcRes, expectedRes) match {
            case (Success((y, _)), Success(Expected(_, expectedCost))) =>
              f.checkVerify(x, y, expectedCost)
            case _ =>
          }
        case AddedFeature =>
          funcRes.isFailure shouldBe true
          Try(f.scalaFunc(x)) shouldBe expectedResValue
      }
    }
    test(preGeneratedSamples, f, printTestCases)
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
    val inputs = scala.collection.mutable.ArrayBuilder.make[A]()
    forAll(config) { x: A =>
      inputs += x
    }
    inputs.result().sorted
  }

  /** Test the given samples or generate new samples using the given Arbitrary.
    * For each sample `f.checkEquality` is executed.
    */
  def test[A: Arbitrary: Ordering : ClassTag, B]
      (preGeneratedSamples: Option[Seq[A]],
       f: FeatureTest[A, B],
       printTestCases: Boolean): Unit = {
    // either get provides or generate new samples (in sorted order)
    val samples = preGeneratedSamples.getOrElse(genSamples[A](DefaultMinSuccessful))

    // then tests them, this will output a nice log of test cases (provided printTestCases == true)
    samples.foreach { x =>
      f.checkEquality(x, printTestCases)
    }
  }

  def test[A: Arbitrary : Ordering : ClassTag, B](samples: Seq[A], f: FeatureTest[A, B]): Unit = {
    test(Some(samples), f, PrintTestCasesDefault)
  }

  def test[A: Arbitrary : Ordering : ClassTag, B]
      (f: FeatureTest[A, B],
       printTestCases: Boolean = PrintTestCasesDefault): Unit = {
    test(None, f, printTestCases)
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
