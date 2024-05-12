package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.Parameter
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.VersionContext
import sigma.ast.ErgoTree.HeaderType
import sigma.ast.{BinAnd, BoolToSigmaProp, ConstantPlaceholder, ErgoTree, FalseLeaf, Height, LT, SBoolean, SInt, SLong, SString, TrueLeaf}
import sigma.exceptions.TyperException
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.Interpreter.ScriptEnv

class SigmaTemplateCompilerTest extends AnyPropSpec with ScalaCheckPropertyChecks with LangTests {
  val templateCompiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)

  property("compiles full contract template") {
    val source =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |* lines to describe functions
        |* @param param3 the final parameter
        |* @return
        |*/
        |@contract def contractName(p1: Int = 5, p2: String = "default string", param3: Long) = {
        |  sigmaProp(true)
        |}""".stripMargin
    val template = templateCompiler.compile(source)

    template.name shouldBe "contractName"
    template.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p1", "describe p1", 0),
      Parameter("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions", 1),
      Parameter("param3", "the final parameter", 2)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SString, SLong)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(5),
      Some("default string"),
      None
    )

    val expectedExpr = BoolToSigmaProp(TrueLeaf)
    template.expressionTree shouldBe expectedExpr
  }

  property("compiles contract template without braces") {
    val source =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |
        |* lines to describe functions
        |* @return
        |*/
        |@contract def contractName(p1: Int = 5, p2: String = "default string") = sigmaProp(true)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.name shouldBe "contractName"
    template.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p1", "describe p1", 0),
      Parameter("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions", 1)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SString)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(5),
      Some("default string")
    )

    val expectedExpr = BoolToSigmaProp(TrueLeaf)
    template.expressionTree shouldBe expectedExpr
  }

  property("uses default value from parameter definition") {
    val source =
      """/**/
        |@contract def contractName(p: Boolean = true) = sigmaProp(p)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p", "", 0)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SBoolean)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(true)
    )

    val expectedExpr = BoolToSigmaProp(ConstantPlaceholder(0, SBoolean))
    template.expressionTree shouldBe expectedExpr

    val expectedTree = new ErgoTree(
      HeaderType @@ 26.toByte,   // use ErgoTreeUtils to get explanation
      Vector(TrueLeaf),
      Right(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean))))

    expectedTree.version shouldBe VersionContext.JitActivationVersion
    expectedTree.hasSize shouldBe true
    expectedTree.isConstantSegregation shouldBe true

    // apply using default values declared in the parameters
    template.applyTemplate(
      version = Some(VersionContext.JitActivationVersion),
      paramValues = Map.empty
    ) shouldBe expectedTree

    // apply overriding the default values
    template.applyTemplate(
      version = Some(VersionContext.JitActivationVersion),
      paramValues = Map("p" -> FalseLeaf)
    ) shouldBe expectedTree.copy(constants = Vector(FalseLeaf))
  }

  property("uses given environment when provided (overriding default value)") {
    val source =
      """/**/
        |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("low", "", 0),
      Parameter("high", "", 1)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SInt)
    // check parsed default values
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(0),
      None
    )

    val expectedExpr = BoolToSigmaProp(
      BinAnd(
        LT(ConstantPlaceholder(0, SInt), Height),
        LT(Height, ConstantPlaceholder(1, SInt))
      )
    )
    template.expressionTree shouldBe expectedExpr

    val explicitEnv = Map("low" -> 10, "high" -> 100)

  }

  property("fails when the parameter is not provided") {
    // NOTE: parameter `condition` is not provided */
    val source      =
      """/**/
       |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high) && condition
       |""".stripMargin
    val env: ScriptEnv = Map.empty // no value for "high"
    assertExceptionThrown(
      templateCompiler.compile(source),
      exceptionLike[TyperException]("Cannot assign type for variable 'condition' because it is not found in env")
    )
  }

}


