package sigmastate.serialization

import sigmastate.crypto.DLogProtocol.ProveDlog

class ProveDlogSerializerSpec extends SerializationSpecification {

  property("ProveDlog: Serializer round trip") {
    forAll { pd: ProveDlog =>
      roundTripTest(pd.toSigmaProp)
    }
  }

}
