package sigmastate.utxo

import sigmastate.Values.{IntConstant, TaggedInt}
import sigmastate.lang.Terms._
import sigmastate.GE
import sigmastate.helpers.SigmaTestingCommons

/**
  * Specification for compile function
  */
class SigmaCompilerSpecification extends SigmaTestingCommons {

  property(">= compile") {
    val elementId = 1: Byte
    val env = Map("elementId" -> elementId)
    val propTree = GE(TaggedInt(elementId), IntConstant(120))
    val propComp = compile(env,
      """{
        |  taggedInt(elementId) >= 120
        |}""".stripMargin).asBoolValue
    propComp shouldBe propTree
  }
}
