package sigmastate.serialization

class ValDefSerializerSpecification extends SerializationSpecification {

  property("ValDef: serializer round trip") {
    forAll(valOrFunDefGen) { v =>
      roundTripTest(v)
    }
  }
}
