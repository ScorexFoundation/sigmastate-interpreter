package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, FalseLeaf, IntConstant, TaggedInt, TrueLeaf}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.ConcreteCollectionCode
import sigmastate.{SBoolean, SInt}

import scala.util.Random

class ConcreteCollectionSerializerSpecification extends TableSerializationSpecification {

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

  override def objects = Table(
    ("object", "bytes"),
    (ConcreteCollection(TrueLeaf, FalseLeaf, TrueLeaf), Array[Byte](ConcreteCollectionCode, 3, SBoolean.typeCode, 5)) // bits: 00000101
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
