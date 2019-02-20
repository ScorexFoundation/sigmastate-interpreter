package sigmastate.serialization

import sigmastate.interpreter.CryptoConstants


class GroupElementSerializerSpecification extends SerializationSpecification {

  property("Identity point serialization") {
    val group = CryptoConstants.dlogGroup
    val identity = group.identity

    val bytes = GroupElementSerializer.toBytes(identity)
    bytes.length shouldBe CryptoConstants.EncodedGroupElementLength
    bytes.forall(_ == 0) shouldBe true
    GroupElementSerializer.parse(SigmaSerializer.startReader(bytes, 0)).isInfinity shouldBe true
  }

  property("point roundtrip") {
    forAll(groupElementConstGen){ge =>
      val bytes = GroupElementSerializer.toBytes(ge.value)
      bytes.length shouldBe CryptoConstants.EncodedGroupElementLength
      val restored = GroupElementSerializer.parse(SigmaSerializer.startReader(bytes, 0))
      restored.normalize().getAffineXCoord shouldBe ge.value.normalize().getAffineXCoord
      restored.normalize().getAffineYCoord shouldBe ge.value.normalize().getAffineYCoord
    }
  }
}
