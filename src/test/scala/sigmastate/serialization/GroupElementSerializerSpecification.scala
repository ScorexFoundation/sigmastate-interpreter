package sigmastate.serialization

import sigmastate.interpreter.CryptoConstants


class GroupElementSerializerSpecification extends SerializationSpecification {

  property("Identity point serialization") {
    val group = CryptoConstants.dlogGroup
    val identity = group.identity

    val bytes = GroupElementSerializer.toBytes(identity)

    bytes.forall(_ == 0) shouldBe true

    GroupElementSerializer.parseBody(Serializer.startReader(bytes, 0)).isInfinity shouldBe true
  }

  property("point roundtrip") {
    forAll(groupElementConstGen){ge =>
      val bytes = GroupElementSerializer.toBytes(ge.value)
      val restored = GroupElementSerializer.parseBody(Serializer.startReader(bytes, 0))
      restored.normalize().getAffineXCoord shouldBe ge.value.normalize().getAffineXCoord
    }
  }

}
