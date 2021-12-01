package sigmastate

import org.ergoplatform.ErgoBox.AdditionalRegisters
import org.ergoplatform._
import scorex.util.ModifierId
import sigmastate.Values.ErgoTree.{DefaultHeader, updateVersionBits}
import sigmastate.Values._
import sigmastate.eval._
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.ErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.interpreter.EvalSettings.EvaluationMode
import sigmastate.interpreter.{ErgoTreeEvaluator, EvalSettings, Interpreter, ProverResult}
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.utxo._
import sigmastate.utils.Helpers._
import special.collection._
import special.sigma.{SigmaDslTesting, Box}

import scala.util.Success

/** Specification to verify that the interpreter behaves according to docs/aot-jit-switch.md. */
class ScriptVersionSwitchSpecification extends SigmaDslTesting
  with CrossVersionProps {
  override implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)
  override implicit val evalSettings: EvalSettings =
    ErgoTreeEvaluator.DefaultEvalSettings.copy(
      costTracingEnabled = true,  // should always be enabled in tests (and false by default)
      evaluationMode = Some(EvalSettings.TestEvaluationMode)
    )

  implicit def IR = createIR()

  val b1 = CostingBox(
    false,
    new ErgoBox(
      1L,
      new ErgoTree(
        0.toByte,
        Vector(),
        Right(BoolToSigmaProp(OR(ConcreteCollection(Array(FalseLeaf, AND(ConcreteCollection(Array(FalseLeaf, FalseLeaf), SBoolean))), SBoolean))))
      ),
      Coll(),
      Map(),
      ModifierId @@ ("008677ffff7ff36dff00f68031140400007689ff014c9201ce8000a9ffe6ceff"),
      32767.toShort,
      32827
    )
  )

  def createErgoTree(headerFlags: Byte)(implicit IR: IRContext): ErgoTree = {
    val code =
      s"""{
        |  val func = { (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }
        |  val v = func(getVar[Coll[Box]](1).get)
        |  val r = SELF.R4[Coll[Box]].get
        |  sigmaProp(v == r)
        |}
      """.stripMargin
    val env = Interpreter.emptyEnv

    // The following ops are performed by frontend: typecheck, create graphs, compile to Tree
    val compiledTree = {
      val internalProp = compiler.typecheck(env, code)
      val costingRes = getCostingResult(env, internalProp)(IR)
      val calcF = costingRes.calcF
      val tree = IR.buildTree(calcF)
      tree
    }
    ErgoTree.withSegregation(headerFlags, compiledTree)
  }

  def testProve(ergoTree: ErgoTree, activatedScriptVersion: Byte, evalMode: Option[EvaluationMode] = None) = {
    val tpeA = SCollection(SBox)
    val input = Coll[Box](b1)
    val newRegisters: AdditionalRegisters = Map(
      ErgoBox.R4 -> Constant[SType](Coll[Box]().asInstanceOf[SType#WrappedType], tpeA)
    )

    val ctx = ErgoLikeContextTesting.dummy(
      createBox(0, ergoTree, additionalRegisters = newRegisters),
      activatedScriptVersion
    ).withBindings(
      1.toByte -> Constant[SType](input.asInstanceOf[SType#WrappedType], tpeA)
    ).asInstanceOf[ErgoLikeContext]
    val prover = new FeatureProvingInterpreter() {
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = true,
        evaluationMode = evalMode)
    }
    val pr = prover.prove(ergoTree, ctx, fakeMessage).getOrThrow
    pr
  }

  def testVerify(ergoTree: ErgoTree, activatedScriptVersion: Byte, evalMode: Option[EvaluationMode] = None) = {
    val tpeA = SCollection(SBox)
    val input = Coll[Box](b1)
    val newRegisters: AdditionalRegisters = Map(
      ErgoBox.R4 -> Constant[SType](Coll[Box]().asInstanceOf[SType#WrappedType], tpeA)
    )

    val ctx = ErgoLikeContextTesting.dummy(
      createBox(0, ergoTree, additionalRegisters = newRegisters),
      activatedScriptVersion
    ).withBindings(
      1.toByte -> Constant[SType](input.asInstanceOf[SType#WrappedType], tpeA)
    ).asInstanceOf[ErgoLikeContext]

    val verifier = new ErgoLikeTestInterpreter() {
      override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
        isMeasureOperationTime = true,
        isDebug = true,
        isTestRun = true,
        evaluationMode = evalMode)
    }
    val pr = ProverResult(ProverResult.empty.proof, ctx.extension)

    // NOTE: exactly this overload should also be called in Ergo
    verifier.verify(ergoTree, ctx, pr, fakeMessage).getOrThrow
  }

  property("new versions of scripts will require size bit in the header") {
    (1 to 7).foreach { version =>
      assertExceptionThrown(
        createErgoTree(headerFlags = updateVersionBits(DefaultHeader, version.toByte)),
        { t =>
          t.isInstanceOf[IllegalArgumentException] &&
              t.getMessage.contains("For newer version the size bit is required")
        })
    }
  }

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 1    | inactive | candidate | Script v1      | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    * 5    | inactive | mined     | Script v1      | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    */
  property("Rules 1,5 | inactive SF | candidate or mined block | Script v1") {
    val samples = genSamples[Coll[Box]](collOfN[Box](5), MinSuccessful(20))

    // this execution corresponds to the normal validation action (R4.0-AOT-cost, R4.0-AOT-verify)
    verifyCases(
      {
        def success[T](v: T, c: Int) = Expected(Success(v), c)
        Seq(
          (Coll[Box](), success(Coll[Box](), 37297)),
          (Coll[Box](b1), success(Coll[Box](), 37397))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.filter({ (b: Box) => b.value > 1 }) },
      "{ (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        Filter(
          ValUse(1, SCollectionType(SBox)),
          FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
        )
      )),
      preGeneratedSamples = Some(samples))
  }

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 3    | inactive | candidate | Script v2      | v4.0    | skip-pool-tx (cannot handle)
    * 7    | inactive | mined     | Script v2      | v4.0    | skip-reject (cannot handle)
    */
  property("Rules 3, 7 | inactive SF | candidate or mined block | Script v2") {
    val headerFlags = ErgoTree.headerWithVersion( 2 /* Script v2 */)
    val ergoTree = createErgoTree(headerFlags = headerFlags)

    // the prove is successful (since it is not part of consensus)
    testProve(ergoTree, activatedScriptVersion = 1 /* SF Status: inactive */)

    // and verify is rejecting
    assertExceptionThrown(
      testVerify(ergoTree, activatedScriptVersion = 1 /* SF Status: inactive */),
      { t =>
        t.isInstanceOf[InterpreterException] &&
            t.getMessage.contains(s"ErgoTree version ${ergoTree.version} is higher than activated 1")
      })
  }

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 9    | active   | candidate | Script v1      | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    */
  property("Rule 9 | active SF | candidate block | Script v1") {
    val headerFlags = ErgoTree.headerWithVersion( 1 /* Script v1 */)
    val ergoTree = createErgoTree(headerFlags = headerFlags)

    // Even though the SF is active and the v2 scripts can be accepted in `mined` blocks
    // this test case is for `candidate` block, so we MUST use
    // Interpreter.MaxSupportedScriptVersion

    // both prove and verify are accepting with full evaluation
    val expectedCost = 5464L
    val pr = testProve(
      ergoTree,
      activatedScriptVersion = Interpreter.MaxSupportedScriptVersion, // special case for *candidate* block
      evalMode = Some(EvalSettings.AotEvaluationMode) // v4.x
    )
    pr.proof shouldBe Array.emptyByteArray
    pr.cost shouldBe expectedCost
    
    val (ok, cost) = testVerify(
      ergoTree,
      activatedScriptVersion = Interpreter.MaxSupportedScriptVersion, // special case for *candidate* block
      evalMode = Some(EvalSettings.AotEvaluationMode) // v4.x
    )
    ok shouldBe true
    cost shouldBe expectedCost
  }

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 11   | active   | candidate | Script v2      | v4.0    | skip-pool-tx (cannot handle)
    * 15   | active   | mined     | Script v2      | v4.0    | skip-accept (rely on majority)
    */
  property("Rule 11, 15 | active SF | candidate or mined block | Script v2") {
    val unsupportedVersion = Interpreter.MaxSupportedScriptVersion + 1
    val headerFlags = ErgoTree.headerWithVersion(unsupportedVersion.toByte /* Script v2 */)
    val ergoTree = createErgoTree(headerFlags = headerFlags)

    val activatedVersion = unsupportedVersion.toByte // SF Status: active

    // the prove is doing normal reduction and proof generation
    val pr = testProve(ergoTree,
          activatedScriptVersion = activatedVersion,
          evalMode = Some(EvalSettings.AotEvaluationMode))
    pr.proof shouldBe Array.emptyByteArray
    pr.cost shouldBe 5464

    // and verify is accepting without evaluation
    val expectedCost = 0L
    val (ok, cost) = testVerify(ergoTree, activatedScriptVersion = activatedVersion)
    ok shouldBe true
    cost shouldBe expectedCost
  }

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 13   | active   | mined     | Script v1      | v4.0    | skip-accept (rely on majority)
    */
  property("Rule 13 | active SF | mined block | Script v1") {
    val supportedVersion = Interpreter.MaxSupportedScriptVersion
    val unsupportedVersion = supportedVersion + 1
    val headerFlags = ErgoTree.headerWithVersion(supportedVersion /* Script v1 */)
    val ergoTree = createErgoTree(headerFlags = headerFlags)

    val activatedVersion = unsupportedVersion.toByte // SF Status: active

    // the prove is doing normal reduction and proof generation
    val pr = testProve(ergoTree,
          activatedScriptVersion = activatedVersion,
          evalMode = Some(EvalSettings.AotEvaluationMode))
    pr.proof shouldBe Array.emptyByteArray
    pr.cost shouldBe 5464

    // and verify is accepting without evaluation
    val (ok, cost) = testVerify(ergoTree, activatedScriptVersion = activatedVersion)
    ok shouldBe true
    cost shouldBe 0L
  }

}
