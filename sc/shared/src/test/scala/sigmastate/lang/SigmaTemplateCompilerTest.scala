package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.Parameter
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.{SBoolean, SInt, SLong, SString}
import sigma.exceptions.TyperException
import sigmastate.eval.CompiletimeIRContext
import sigmastate.interpreter.Interpreter.ScriptEnv

class SigmaTemplateCompilerTest extends AnyPropSpec with ScalaCheckPropertyChecks with LangTests {
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
    val compiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val template = compiler.compile(Map.empty, source)

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

    val sigmaCompiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    implicit val ir = new CompiletimeIRContext
    val result = sigmaCompiler.compile(Map.empty, "{ sigmaProp(true) }")

    template.expressionTree shouldBe result.buildTree
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
    val compiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val template = compiler.compile(Map.empty, source)

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

    val sigmaCompiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    implicit val ir = new CompiletimeIRContext
    val result = sigmaCompiler.compile(Map.empty, "sigmaProp(true)")

    template.expressionTree shouldBe result.buildTree
  }

  property("uses default value from parameter definition") {
    val source =
      """/**/
        |@contract def contractName(p: Boolean = true) = sigmaProp(p)
        |""".stripMargin
    val compiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val template = compiler.compile(Map.empty, source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p", "", 0)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SBoolean)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(true)
    )

    val sigmaCompiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    implicit val ir = new CompiletimeIRContext
    val result = sigmaCompiler.compile(Map("p" -> true), "sigmaProp(p)")

    template.expressionTree shouldBe result.buildTree
  }

  property("uses given environment when provided (overriding default value)") {
    val explicitEnv = Map("low" -> 10, "high" -> 100)
    val source =
      """/**/
        |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high)
        |""".stripMargin
    val compiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val template = compiler.compile(explicitEnv, source)

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

    val sigmaCompiler = new SigmaCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    implicit val ir = new CompiletimeIRContext
    val result = sigmaCompiler.compile(
      env = explicitEnv,
      code = "sigmaProp(low < HEIGHT && HEIGHT < high)"
    )

    template.expressionTree shouldBe result.buildTree
  }

  property("fails when constant value in not provided") {
    // NOTE: parameter `high` without default value */
    val source      =
      """/**/
       |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high)
       |""".stripMargin
    val compiler    = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)
    val env: ScriptEnv = Map.empty // no value for "high"
    assertExceptionThrown(
      compiler.compile(env, source),
      exceptionLike[TyperException]("Cannot assign type for variable 'high' because it is not found in env")
    )
  }

}


