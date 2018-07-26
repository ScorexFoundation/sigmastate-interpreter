package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, Constant, FalseLeaf, IntConstant, TaggedInt, TrueLeaf}
import sigmastate._
import sigmastate.lang.Terms._

import scala.util.Random

class ConcreteCollectionSerializerSpecification extends TableSerializationSpecification {

  private def testCollectionWithConstant[T <: SType](tpe: T) = {
    implicit val wWrapped = wrappedTypeGen(tpe)
    implicit val tag = tpe.classTag[T#WrappedType]
    forAll { x: Array[T#WrappedType] =>
      roundTripTest(ConcreteCollection[T](x.map(v => Constant(v, tpe)),
        tpe))
    }
  }

  property("ConcreteCollection (Constant[SBoolean.type]): Serializer round trip ") {
    testCollectionWithConstant(SBoolean)
  }

  property("ConcreteCollection (Constant): Serializer round trip ") {
    testCollectionWithConstant(SByte)
    testCollectionWithConstant(SShort)
    testCollectionWithConstant(SInt)
    testCollectionWithConstant(SLong)
    testCollectionWithConstant(SBigInt)
    testCollectionWithConstant(SGroupElement)
    testCollectionWithConstant(SUnit)
    testCollectionWithConstant(SBox)
    testCollectionWithConstant(SAvlTree)
  }

  property("ConcreteCollection: Serializer round trip with different types seq") {
    forAll { (i: IntConstant, ti: TaggedInt) =>
      val seq = Random.shuffle(IndexedSeq(i.asIntValue, ti.asIntValue))
      roundTripTest(ConcreteCollection(seq))
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (ConcreteCollection(TrueLeaf, FalseLeaf, TrueLeaf),
      Array[Byte](OpCodes.ConcreteCollectionBooleanConstantCode, 3, 5)) // bits: 00000101
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

  property("ConcreteCollection: deserialize collection of a crazy size") {
    val bytes = Array[Byte](OpCodes.ConcreteCollectionCode) ++
      Serializer.startWriter().putUInt(Int.MaxValue).toBytes
    an[AssertionError] should be thrownBy ValueSerializer.deserialize(bytes)
  }
}
