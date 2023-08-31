package special

import scalan.{BaseTests, RType}
import RType._
import Types._

class TypesTests extends BaseTests {

  test("RType has name") {
    def test[A](t: RType[A], n: String) = {
      t.name shouldBe n
    }
    test(tupleRType(Array(IntType, LongType, RType[(Byte, special.sigma.BigInt)], RType[Option[Boolean]])),
           "(Int, Long, (Byte, BigInt), Option[Boolean])")
  }

  test("RType implements equality") {

    def tuple = tupleRType(Array(RType[Int], RType[Long]))
    assert(tuple == tuple, "compare two different but equal instances")

    def tuple2 = tupleRType(Array(RType[Long], RType[Int]))
    assert(tuple != tuple2, "compare two different types")

  }
}
