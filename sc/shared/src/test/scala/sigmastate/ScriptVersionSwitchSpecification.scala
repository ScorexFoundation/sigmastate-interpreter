package sigmastate

import org.ergoplatform.ErgoBox.AdditionalRegisters
import org.ergoplatform._
import scorex.util.ModifierId
import sigmastate.Values.ErgoTree.{DefaultHeader, updateVersionBits}
import sigmastate.Values._
import sigma.VersionContext.MaxSupportedScriptVersion
import sigmastate.eval._
import sigmastate.exceptions.InterpreterException
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.ErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.interpreter.EvalSettings.EvaluationMode
import sigmastate.interpreter.{CostedProverResult, ErgoTreeEvaluator, EvalSettings, Interpreter, ProverResult}
import sigmastate.lang.Terms.ValueOps
import sigmastate.utils.Helpers._
import sigma.{Box, SigmaDslTesting}

/** Specification to verify that the interpreter behaves according to docs/aot-jit-switch.md.
  *
  * NOTE, this suite doesn't inherit CrossVersionProps because each test case require explicit
  * setup of activatedScriptVersion and ErgoTree.version.
  */
class ScriptVersionSwitchSpecification extends SigmaDslTesting {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 30)
  override implicit val evalSettings: EvalSettings =
    ErgoTreeEvaluator.DefaultEvalSettings.copy(
      costTracingEnabled = true  // should always be enabled in tests (and false by default)
    )

  implicit def IR: IRContext = createIR()

  lazy val b1 = CostingBox(
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

  /** Creates ErgoTree with segregated constants and also the given header flags. */
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
      val res = compiler.compile(env, code)
      checkCompilerResult(res)
      res.buildTree.asSigmaProp
    }
    ErgoTree.withSegregation(headerFlags, compiledTree)
  }

  /** Proves the given ergoTree in a dummy context with the given activatedScriptVersion.
    * @param activatedScriptVersion used to create the context
    * @param evalMode can be specified to force specific interpreter regardless of
    *                 activatedScriptVersion
    */
  def testProve(
        ergoTree: ErgoTree,
        activatedScriptVersion: Byte,
        evalMode: Option[EvaluationMode] = None): CostedProverResult = {
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

  /** Proves the given ergoTree in a dummy context with the given activatedScriptVersion
    * and using empty proof.
    * @param activatedScriptVersion used to create the context
    * @param evalMode can be specified to force specific interpreter regardless of
    *                 activatedScriptVersion
    */
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
        exceptionLike[IllegalArgumentException]("For newer version the size bit is required")
      )
    }
  }

  /** Rule#| BlockVer | Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 1    | 1,2      | candidate | Script v0/v1   | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    * 2    | 1,2      | candidate | Script v0/v1   | v5.0    | R4.0-AOT-cost, R4.0-AOT-verify
    * 5    | 1,2      | mined     | Script v0/v1   | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    * 6    | 1,2      | mined     | Script v0/v1   | v5.0    | R4.0-AOT-cost, R4.0-AOT-verify
    */
  property("Rules 1,2,5,6 | BlockVer 1,2 | candidate or mined block | Script v0/v1") {
    // this test verifies the normal validation action (R4.0-AOT-cost, R4.0-AOT-verify)
    // See SigmaDslSpecification for a full suite of v4.x vs. v5.0 equivalence tests

    forEachActivatedScriptVersion(activatedVers = Array[Byte](0, 1)) // for BlockVersions 1,2
    {
      // tree versions that doesn't exceed activated
      val treeVers = (0 to activatedVersionInTests).map(_.toByte).toArray[Byte]

      forEachErgoTreeVersion(treeVers) {
        // SF inactive: check cost vectors of v4.x interpreter
        val headerFlags = ErgoTree.headerWithVersion(ergoTreeVersionInTests)
        val ergoTree = createErgoTree(headerFlags)

        // both prove and verify are accepting with full evaluation
        val expectedCost = 24L
        val pr = testProve(ergoTree, activatedScriptVersion = activatedVersionInTests)
        pr.proof shouldBe Array.emptyByteArray
        pr.cost shouldBe expectedCost

        val (ok, cost) = testVerify(ergoTree, activatedScriptVersion = activatedVersionInTests)
        ok shouldBe true
        cost shouldBe expectedCost
      }
    }
  }

  /** Rule#| BlockVer | Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 3    | 1,2      | candidate | Script v2      | v4.0    | skip-pool-tx (cannot handle)
    * 4    | 1,2      | candidate | Script v2      | v5.0    | skip-pool-tx (wait activation)
    * 7    | 1,2      | mined     | Script v2      | v4.0    | skip-reject (cannot handle)
    * 8    | 1,2      | mined     | Script v2      | v5.0    | skip-reject (wait activation)
    */
  property("Rules 3,4,7,8 | Block Version 1,2 | candidate or mined block | Script v2") {
    forEachActivatedScriptVersion(Array[Byte](0, 1)) { // Block Versions 1, 2

      forEachErgoTreeVersion(ergoTreeVers = Array[Byte](2)) { // only Script v2
        val headerFlags = ErgoTree.headerWithVersion(ergoTreeVersionInTests /* Script v2 */)
        val ergoTree = createErgoTree(headerFlags = headerFlags)

        // prover is rejecting ErgoTree versions higher than activated
        assertExceptionThrown(
          testProve(ergoTree, activatedScriptVersion = activatedVersionInTests),
          exceptionLike[InterpreterException](s"ErgoTree version ${ergoTree.version} is higher than activated $activatedVersionInTests")
        )

        // verifier is rejecting ErgoTree versions higher than activated
        assertExceptionThrown(
          testVerify(ergoTree, activatedScriptVersion = activatedVersionInTests),
          exceptionLike[InterpreterException](s"ErgoTree version ${ergoTree.version} is higher than activated $activatedVersionInTests")
        )
      }
    }
  }

  /** Rule#| BlockVer | Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 10   | 3        | candidate | Script v0/v1   | v5.0    | R5.0-JIT-verify
    * 12   | 3        | candidate | Script v2      | v5.0    | R5.0-JIT-verify
    * 14   | 3        | mined     | Script v0/v1   | v5.0    | R5.0-JIT-verify
    * 16   | 3        | mined     | Script v2      | v5.0    | R5.0-JIT-verify
    */
  property("Rules 10,12,14,16 | BlockVer 3 | candidate or mined block | Script v0/v1/v2") {
    // this test verifies the normal validation action (R5.0-JIT-verify)
    // See SigmaDslSpecification for a full suite of v4.x vs. v5.0 equivalence tests

    forEachActivatedScriptVersion(activatedVers = Array[Byte](2)) // Block version 3
    {
      // tree versions that doesn't exceed activated
      val treeVers = (0 to activatedVersionInTests).map(_.toByte).toArray[Byte]

      forEachErgoTreeVersion(treeVers) {
        // SF inactive: check cost vectors of v4.x interpreter
        val ergoTree = createErgoTree(ergoTreeHeaderInTests)

        // both prove and verify are accepting with full evaluation
        val pr = testProve(ergoTree, activatedScriptVersion = activatedVersionInTests)
        pr.proof shouldBe Array.emptyByteArray
        pr.cost shouldBe 24L

        val (ok, cost) = testVerify(ergoTree, activatedScriptVersion = activatedVersionInTests)
        ok shouldBe true
        cost shouldBe 24L
      }
    }
  }

  /** Rule#| BlockVer | Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 17   | 3        | candidate | Script v3      | v5.0    | skip-reject (cannot handle)
    * 18   | 3        | mined     | Script v3      | v5.0    | skip-reject (cannot handle)
    */
  property("Rules 17,18 | Block v3 | candidate or mined block | Script v3") {
    forEachActivatedScriptVersion(activatedVers = Array[Byte](2)) // version for Block v3
    {
      forEachErgoTreeVersion(ergoTreeVers = Array[Byte](3, 4)) { // scripts >= v3
        val headerFlags = ErgoTree.headerWithVersion(ergoTreeVersionInTests)
        val ergoTree = createErgoTree(headerFlags)

        // prover is rejecting ErgoTree versions higher than activated
        assertExceptionThrown(
          testProve(ergoTree, activatedScriptVersion = activatedVersionInTests),
          exceptionLike[InterpreterException](s"ErgoTree version ${ergoTree.version} is higher than activated $activatedVersionInTests")
        )

        // verifier is rejecting ErgoTree versions higher than activated
        assertExceptionThrown(
          testVerify(ergoTree, activatedScriptVersion = activatedVersionInTests),
          exceptionLike[InterpreterException](s"ErgoTree version ${ergoTree.version} is higher than activated $activatedVersionInTests")
        )
      }
    }
  }

  /** Rule#| BlockVer | Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 19   | 4        | candidate | Script v3      | v6.0    | R6.0-JIT-verify
    * 20   | 4        | mined     | Script v3      | v6.0    | R6.0-JIT-verify
    */
  property("Rules 19,20 | Block v4 | candidate or mined block | Script v3") {
    forEachActivatedScriptVersion(activatedVers = Array[Byte](3)) // version for Block v4
    {
      forEachErgoTreeVersion(ergoTreeVers = Array[Byte](3)) { // scripts >= v3
        val headerFlags = ErgoTree.headerWithVersion(ergoTreeVersionInTests)
        val ergoTree = createErgoTree(headerFlags)

        // both prove and verify are accepting with full evaluation
        val pr = testProve(ergoTree, activatedScriptVersion = activatedVersionInTests)
        pr.proof shouldBe Array.emptyByteArray
        pr.cost shouldBe 24L

        // and verify is accepting without evaluation
        val (ok, cost) = testVerify(ergoTree, activatedScriptVersion = activatedVersionInTests)
        ok shouldBe true
        cost shouldBe 24L
      }
    }
  }

  /** Rule#| BlockVer | Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 21   | 4        | candidate | Script v0/v1   | v5.0    | R6.0-JIT-verify
    * 22   | 4        | candidate | Script v2      | v5.0    | R6.0-JIT-verify
    * 23   | 4        | candidate | Script v3      | v5.0    | R6.0-JIT-verify
    * 24   | 4        | mined     | Script v0/v1   | v5.0    | R6.0-JIT-verify
    * 25   | 4        | mined     | Script v2      | v5.0    | R6.0-JIT-verify
    * 26   | 4        | mined     | Script v3      | v5.0    | R6.0-JIT-verify
    */
  property("Rules 21,22,23,24,25,26 | Block v4 | candidate or mined block | Script v0/v1/v2/v3") {
    // this test verifies the normal validation action R5.0-JIT-verify of v5.x releases
    // when Block v4 already activated, and the script is v0, v1, v2, or v3.

    forEachActivatedScriptVersion(Array[Byte](3)) // version for Block v4
    {
      forEachErgoTreeVersion(Array[Byte](0, 1, 2, 3)) { // tree versions supported by v6.x
        // SF inactive: check cost vectors of v4.x interpreter
        val headerFlags = ErgoTree.headerWithVersion(ergoTreeVersionInTests)
        val ergoTree = createErgoTree(headerFlags)

        // both prove and verify are accepting with full evaluation
        val pr = testProve(ergoTree, activatedScriptVersion = activatedVersionInTests)
        pr.proof shouldBe Array.emptyByteArray
        pr.cost shouldBe 24L

        val (ok, cost) = testVerify(ergoTree, activatedScriptVersion = activatedVersionInTests)
        ok shouldBe true
        cost shouldBe 24L
      }
    }
  }

}
