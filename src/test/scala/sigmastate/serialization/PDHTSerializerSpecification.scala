package sigmastate.serialization

import scapi.sigma.ProveDiffieHellmanTuple

class PDHTSerializerSpecification extends SerializationSpecification {


  property("ProveDiffieHellmanTupleSerializer: Serializer round trip") {
    forAll { i: ProveDiffieHellmanTuple =>
      roundTripTest(i)
    }
  }

}
