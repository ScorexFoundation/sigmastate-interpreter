package sigmastate.serialization

import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.utxo.CostTableStat

class PDHTSerializerSpecification extends SerializationSpecification {

  property("ProveDiffieHellmanTupleSerializer: Serializer round trip") {
    forAll { i: ProveDiffieHellmanTuple =>
      roundTripTest(i)
    }
    // In IntelliJ IDEA this test is executed last, at this point all statistics has been collected
    // We output it here in the console
    println(CostTableStat.print)
  }

}
