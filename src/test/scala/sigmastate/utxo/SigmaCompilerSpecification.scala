package sigmastate.utxo

import sigmastate.GE
import sigmastate.Values.IntConstant
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.lang.Terms._
import sigmastate.utxo.GetVar._

/**
  * Specification for compile function
  */
class SigmaCompilerSpecification extends SigmaTestingCommons {

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
}
