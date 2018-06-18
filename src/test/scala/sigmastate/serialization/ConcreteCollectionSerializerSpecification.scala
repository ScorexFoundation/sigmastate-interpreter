package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, Constant, FalseLeaf, IntConstant, TaggedInt, TrueLeaf}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.ConcreteCollectionCode

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

  property("ConcreteCollection (boolean constants): Serializer round trip ") {
    // both BooleanConstant and Constant[SBoolean.type]
    roundTripTest(ConcreteCollection[SBoolean.type](TrueLeaf,
      FalseLeaf,
      Constant[SBoolean.type](true, SBoolean),
      Constant[SBoolean.type](false, SBoolean)))
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
    (ConcreteCollection(TrueLeaf, FalseLeaf, TrueLeaf), Array[Byte](ConcreteCollectionCode, 3, SBoolean.typeCode, 5)) // bits: 00000101
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
