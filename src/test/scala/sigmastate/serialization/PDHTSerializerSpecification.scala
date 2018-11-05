package sigmastate.serialization

import sigmastate.basics.ProveDiffieHellmanTuple

class PDHTSerializerSpecification extends SerializationSpecification {

  property("ProveDiffieHellmanTupleSerializer: Serializer round trip") {
    forAll { i: ProveDiffieHellmanTuple =>
      roundTripTest(i)
    }
  }

}
