package sigmastate.serialization

import sigmastate.basics.DLogProtocol.ProveDlog

class ProveDlogSerializerSpec extends SerializationSpecification {

  property("ProveDlog: Serializer round trip") {
    forAll { pd: ProveDlog =>
      roundTripTest(pd.toSigmaProp)
    }
  }

}
