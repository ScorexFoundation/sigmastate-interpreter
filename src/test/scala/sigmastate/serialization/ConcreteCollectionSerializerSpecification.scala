package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, Constant, FalseLeaf, IntConstant, TaggedInt, TrueLeaf}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.ConcreteCollectionCode
import sigmastate._

import scala.util.Random

class ConcreteCollectionSerializerSpecification extends TableSerializationSpecification {

    private def testCollection[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { x: Array[T#WrappedType] =>
      roundTripTest(ConcreteCollection[T](x.map(v => Constant(v, tpe)),
        tpe))
    }
  }

  property("ConcreteCollection: Serializer round trip ") {
    testCollection(SBoolean)
    testCollection(SByte)
    testCollection(SShort)
    testCollection(SInt)
    testCollection(SLong)
    testCollection(SBigInt)
    testCollection(SGroupElement)
    testCollection(SUnit)
    testCollection(SBox)
    testCollection(SAvlTree)
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
