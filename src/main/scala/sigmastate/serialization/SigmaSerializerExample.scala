package sigmastate.serialization

import scala.util.Try
import sigmastate._
import sigmastate.serialization.ValueSerializer.{deserialize, serialize}

object SigmaSerializerExample extends App {
  //todo: convert cases below into tests:

  println(deserialize(Array[Byte](21, 11, 0, 0, 0, 0, 0, 0, 0, 2,
    11, 0, 0, 0, 0, 0, 0, 0, 3)))

  val s: Value[SInt.type] = IntConstant(4)
  println(serialize(s))

  assert(deserialize(serialize(s)) == s)

  assert(Try(deserialize(Array[Byte](21, 12, 13))).isFailure, "LT(bool, bool) must fail")

  val gt = GT(IntConstant(6), IntConstant(5))
  println(deserialize(serialize(gt)))
  assert(deserialize(serialize(gt)) == gt)

  val eq = EQ(TrueLeaf, FalseLeaf)
  println(serialize(eq).mkString(","))
  assert(deserialize(serialize(eq)) == eq)

  //todo: make this expression does not compile?
  val eq2 = EQ(TrueLeaf, IntConstant(5))

  //concrete collection
  val cc = ConcreteCollection(IndexedSeq(IntConstant(5), IntConstant(6), IntConstant(7)))
  assert(deserialize(serialize(cc)) == cc)

  val tb = TaggedBox(21: Byte)
  assert(deserialize(serialize(tb)) == tb)

  val cc2 = ConcreteCollection(IndexedSeq(IntConstant(5), TaggedInt(21)))
  assert(deserialize(serialize(cc2)) == cc2)
}
