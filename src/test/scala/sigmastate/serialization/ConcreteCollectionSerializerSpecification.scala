package sigmastate.serialization

import scorex.crypto.encode.Base58
import sigmastate._

class ConcreteCollectionSerializerSpecification extends SerializationSpecification {

  property("ConcreteCollection serializer roundtrip") {
    roundTripTest(ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1))))))
  }

  property("ConcreteCollection serializer roundtrip with different types seq") {
    roundTripTest(ConcreteCollection(IndexedSeq(IntConstant(5), TaggedInt(21))))
  }

  property("ConcreteCollection deserialize from predefined bytes") {
    val col = ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))
    val bytes = Base58.decode("yo18XXEkLgyWrJvWUX4t").get
    predefinedBytesTest(bytes, col)
  }
}
