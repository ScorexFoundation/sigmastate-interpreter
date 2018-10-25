package sigmastate.serialization

import sigmastate.Values.{ConstantPlaceholder, IntConstant}
import sigmastate._
import sigmastate.helpers.SigmaTestingCommons

class ErgoTreeSerializerSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR = new TestingIRContext

  property("extract constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ErgoTreeSerializer(IR).extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("deserializeRaw") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    val bytes = ErgoTreeSerializer(IR).serialize(script)
    ErgoTreeSerializer(IR).deserializeRaw(bytes) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("(de)serialize round trip") {
    val script = Plus(10, 20)
    val bytes = ErgoTreeSerializer(IR).serialize(script)
    ErgoTreeSerializer(IR).deserialize(bytes) shouldEqual script
  }
}
