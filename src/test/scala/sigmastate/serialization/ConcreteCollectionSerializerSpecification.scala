package sigmastate.serialization

import sigmastate.SInt
import sigmastate.Values.{ConcreteCollection, TaggedInt, IntConstant}
import sigmastate.lang.Terms._

import scala.util.Random

class ConcreteCollectionSerializerSpecification extends SerializationSpecification {

  property("ConcreteCollection: Serializer round trip") {
    forAll { col: ConcreteCollection[SInt.type] =>
      roundTripTest(col)
    }
  }

  property("ConcreteCollection: Serializer round trip with different types seq") {
    forAll { (i: IntConstant, ti: TaggedInt) =>
      val seq = Random.shuffle(IndexedSeq(i.asIntValue, ti.asIntValue))
      roundTripTest(ConcreteCollection(seq))
    }
  }
}
