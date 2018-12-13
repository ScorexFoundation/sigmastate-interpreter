package sigmastate.utxo

import sigmastate.{GE, ModQ, SType}
import sigmastate.Values._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms._

/**
  * Specification for compile function
  */
class SigmaCompilerSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

  private def compile(code: String, env: ScriptEnv = Map()): Value[SType] = compileWithCosting(env, code)

  property(">= compile") {
    val elementId = 1: Byte
    val env = Map("elementId" -> elementId)
    val propTree = GE(GetVarInt(elementId).get, IntConstant(120))
    val propComp = compileWithCosting(env,
      """{
        |  getVar[Int](elementId).get >= 120
        |}""".stripMargin).asBoolValue
    propComp shouldBe propTree
  }

  property("modQ") {
    compile("10.toBigInt.modQ") shouldEqual ModQ(BigIntConstant(10))
  }
}
