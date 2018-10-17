package sigmastate.serialization

import sigmastate.Values.{ConstantPlaceholder, IntConstant}
import sigmastate._

class ErgoScriptSerializerSpecification extends SerializationSpecification {

  property("extract constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ErgoScriptSerializer.extractConstants(script) shouldBe (extractedConstants, scriptWithPlaceholders)
  }
}
