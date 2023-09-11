package sigmastate.serialization

import sigmastate.crypto.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions.SigmaBooleanOps

class ProveDlogSerializerSpec extends SerializationSpecification {

  property("ProveDlog: Serializer round trip") {
    forAll { pd: ProveDlog =>
      roundTripTest(pd.toSigmaProp)
    }
  }

}
