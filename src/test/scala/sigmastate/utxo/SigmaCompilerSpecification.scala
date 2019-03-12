package sigmastate.utxo

import sigmastate.{GE, ModQ, SType}
import sigmastate.Values._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms._
import sigmastate._

/**
  * Specification for compile function
  */
class SigmaCompilerSpecification extends SigmaTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def compile(code: String, env: ScriptEnv = Map()): Value[SType] = compile(env, code)

  property(">= compile") {
    val elementId = 1: Byte
    val env = Map("elementId" -> elementId)
    val propTree = GE(GetVarInt(elementId).get, IntConstant(120))
    val propComp = compile(env,
      """{
        |  getVar[Int](elementId).get >= 120
        |}""".stripMargin).asBoolValue
    propComp shouldBe propTree
  }

  property("modular arithmetic ops: ModQ") {
    compile("10.toBigInt.modQ") shouldEqual ModQ(BigIntConstant(10))
  }

  // TODO: enable after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/324 is done
  ignore("modular arithmetic ops: BinOps") {
    compile("10.toBigInt.plusModQ(2.toBigInt)") shouldEqual
        PlusModQ(BigIntConstant(10), BigIntConstant(2))
    compile("10.toBigInt.minusModQ(2.toBigInt)") shouldEqual
        MinusModQ(BigIntConstant(10), BigIntConstant(2))
  }
}
