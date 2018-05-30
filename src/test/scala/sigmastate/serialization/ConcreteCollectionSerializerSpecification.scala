package sigmastate.serialization

import sigmastate.SLong
import sigmastate.Values.{ConcreteCollection, LongConstant, TaggedInt}

import scala.util.Random

class ConcreteCollectionSerializerSpecification extends SerializationSpecification {

  property("ConcreteCollection: Serializer round trip") {
    forAll { col: ConcreteCollection[SLong.type] =>
      roundTripTest(col)
    }
  }

  property("ConcreteCollection: Serializer round trip with different types seq") {
    forAll { (i: LongConstant, ti: TaggedInt) =>
      val seq = Random.shuffle(IndexedSeq(i, ti))
      roundTripTest(ConcreteCollection(seq))
    }
  }
}
