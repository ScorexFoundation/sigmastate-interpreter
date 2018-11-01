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

  property("ConstantPlaceholder: serializer round trip") {
    forAll(constantPlaceholderGen) { v =>
      roundTripTest(v)
    }
  }

  property("FuncValue: serializer round trip") {
    forAll(funcValueGen) { v =>
      roundTripTest(v)
    }
  }
}
