package sigmastate.serialization

import scapi.sigma.DLogProtocol.ProveDlog

class ProveDlogSerializerSpec extends SerializationSpecification {

  property("ProveDlog: Serializer round trip") {
    forAll { pd: ProveDlog =>
      roundTripTest(pd)
    }
  }

}
