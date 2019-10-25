package special

import scalan.{BaseTests, RType}
import RType._
import Types._

class TypesTests extends BaseTests {

  test("RType has name") {
    def test[A](t: RType[A], n: String) = {
      t.name shouldBe n
    }
    test(tupleRType(Array(IntType, LongType, RType[(String, Double)], RType[Option[Boolean]])),
           "(Int, Long, (String, Double), Option[Boolean])")
  }

  test("RType implements equality") {
    def test[A: RType, B: RType] = {
      val x = RType[A]; val y = RType[B]
      assert(x == y)
    }

    def tuple = tupleRType(Array(RType[Int], RType[Long]))
    assert(tuple == tuple, "compare two different but equal instances")

    def tuple2 = tupleRType(Array(RType[Long], RType[Int]))
    assert(tuple != tuple2, "compare two different types")

    def struct = structRType(Array("x", "y"), Array(RType[Int], RType[Long]))
    assert(struct == struct, "compare two different but equal instances")

    def struct2 = structRType(Array("x", "y2"), Array(RType[Int], RType[Long]))
    assert(struct != struct2, "changed single field name")

    def struct3 = structRType(Array("x", "y"), Array(RType[Int], RType[Int]))
    assert(struct != struct3, "changed single field type")
  }
}
