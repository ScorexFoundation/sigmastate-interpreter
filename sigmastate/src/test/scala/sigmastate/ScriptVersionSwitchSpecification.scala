package sigmastate

import org.ergoplatform.ErgoBox.AdditionalRegisters
import org.ergoplatform._
import scorex.util.ModifierId
import sigmastate.Values.ErgoTree.{DefaultHeader, updateVersionBits, SizeFlag}
import sigmastate.Values._
import sigmastate.eval._
import sigmastate.helpers.ErgoLikeContextTesting
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.Interpreter
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.utxo._
import sigmastate.utils.Helpers._
import special.collection._
import special.sigma.{SigmaDslTesting, Box}
import sigmastate.helpers.TestingHelpers._

import scala.util.Success

/** Specification to verify that the interpreter behaves according to docs/aot-jit-switch.md. */
class ScriptVersionSwitchSpecification extends SigmaDslTesting {
  override implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)
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

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 1    | inactive | candidate | Script v1      | v4.0    | R4.0-AOT-cost, R4.0-AOT-verify
    */
  property("Rule 1 | inactive SF | candidate block | Script v1") {
    val samples = genSamples[Coll[Box]](collOfN[Box](5), MinSuccessful(20))

    verifyCases(
      {
        def success[T](v: T, c: Int) = Success(Expected(v, c))
        Seq(
          (Coll[Box](), success(Coll[Box](), 37297)),
          (Coll[Box](b1), success(Coll[Box](), 37397)),
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

  def createErgoTree(headerFlags: Byte)(implicit IR: IRContext): ErgoTree = {
    import ErgoTree._
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

  def testProve(ergoTree: ErgoTree, activatedScriptVersion: Byte) = {
    val tpeA = SCollection(SBox)
    val input = Coll[Box](b1)
    val newRegisters: AdditionalRegisters = Map(
      ErgoBox.R4 -> Constant[SType](Coll[Box]().asInstanceOf[SType#WrappedType], tpeA)
    )

    val ctx = copyContext(ErgoLikeContextTesting.dummy(
      createBox(0, ergoTree, additionalRegisters = newRegisters)
    ).withBindings(
      1.toByte -> Constant[SType](input.asInstanceOf[SType#WrappedType], tpeA),
    ).asInstanceOf[ErgoLikeContext])(activatedScriptVersion = activatedScriptVersion)
    val prover = new FeatureProvingInterpreter()
    val pr = prover.prove(ergoTree, ctx, fakeMessage).getOrThrow
    pr
  }

  /** Rule#| SF Status| Block Type| Script Version | Release | Validation Action
    * -----|----------|-----------|----------------|---------|--------
    * 3    | inactive | candidate | Script v2      | v4.0    | skip-pool-tx (cannot handle)
    */
  property("Rule 3 | inactive SF | candidate block | Script v2") {

    assertExceptionThrown(
      createErgoTree(headerFlags = updateVersionBits(DefaultHeader, 1)),
      { t =>
        t.isInstanceOf[IllegalArgumentException] &&
        t.getMessage.contains("For newer version the size bit is required")
      })

    val headerFlags = ErgoTree.SizeFlag | updateVersionBits(DefaultHeader, 1 /* Script v2 */)
    val ergoTree = createErgoTree(headerFlags = headerFlags.toByte)
    
    assertExceptionThrown(
      testProve(ergoTree, activatedScriptVersion = 0 /* SF Status: inactive */),
      { t =>
        t.isInstanceOf[InterpreterException] &&
        t.getMessage.contains(s"Not supported script version ${ergoTree.version}")
      })
  }

}
