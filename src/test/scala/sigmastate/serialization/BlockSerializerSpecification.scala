package sigmastate.serialization

class BlockSerializerSpecification extends SerializationSpecification {

  property("ValDef: serializer round trip") {
    forAll(valOrFunDefGen) { v =>
      roundTripTest(v)
    }
  }

  property("ValUse: serializer round trip") {
    forAll(valUseGen) { v =>
      roundTripTest(v)
    }
  }

  property("BlockValue: serializer round trip") {
    forAll(blockValueGen) { v =>
      roundTripTest(v)
    }
  }
}
