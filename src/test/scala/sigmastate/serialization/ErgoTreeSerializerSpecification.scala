package sigmastate.serialization

import org.ergoplatform.Self
import sigmastate.Values.{BlockValue, Constant, ConstantPlaceholder, IntConstant, ValDef, ValUse}
import sigmastate._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utxo.{ExtractAmount, ExtractBytes, SizeOf}

class ErgoTreeSerializerSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR = new TestingIRContext

  property("extract constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ErgoTreeSerializer(IR).extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("no constants to extract") {
    val script = Plus(ExtractAmount(Self), ExtractAmount(Self))
    val extractedConstants = Seq()
    val scriptWithPlaceholders = BlockValue(Vector(ValDef(1,List(),ExtractAmount(Self))),Plus(ValUse(1,SLong),ValUse(1,SLong)))
    ErgoTreeSerializer(IR).extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("inject constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20)).asInstanceOf[Seq[Constant[SType]]]
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ErgoTreeSerializer(IR).injectConstants(extractedConstants, scriptWithPlaceholders) shouldEqual script
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
