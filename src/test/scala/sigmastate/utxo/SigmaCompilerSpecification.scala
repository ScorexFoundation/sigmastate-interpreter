package sigmastate.utxo

import sigmastate.GE
import sigmastate.Values._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.lang.Terms._

/**
  * Specification for compile function
  */
class SigmaCompilerSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

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
}
