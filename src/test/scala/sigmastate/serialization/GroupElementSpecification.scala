package sigmastate.serialization

import sigmastate.Values.GroupElementConstant

class GroupElementSpecification extends SerializationSpecification {

  property("GroupElement: Serializer round trip") {
    forAll { ge: GroupElementConstant =>
      roundTripTest(ge)
    }
  }

}