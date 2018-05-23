package sigmastate.serialization

import org.ergoplatform._
import sigmastate.Values.{FalseLeaf, GroupGenerator, TrueLeaf}
import sigmastate.utxo._

class CaseObjectSerializationSpec extends SerializationSpecification {


  property("TrueLeaf: Serialization round trip") {
    roundTripTest(TrueLeaf)
  }

  property("FalseLeaf: Serialization round trip") {
    roundTripTest(FalseLeaf)
  }

  property("Height: Serialization round trip") {
    roundTripTest(Height)
  }

  property("Inputs: Serialization round trip") {
    roundTripTest(Inputs)
  }

  property("Outputs: Serialization round trip") {
    roundTripTest(Outputs)
  }

  property("LastBlockUtxoRootHash: Serialization round trip") {
    roundTripTest(LastBlockUtxoRootHash)
  }

  property("Self: Serialization round trip") {
    roundTripTest(Self)
  }

  property("GroupGenerator: Serialization round trip") {
    roundTripTest(GroupGenerator)
  }
}
