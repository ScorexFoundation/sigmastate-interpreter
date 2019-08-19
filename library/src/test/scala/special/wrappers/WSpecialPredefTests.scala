package special.wrappers


import scala.language.reflectiveCalls
import scalan.RType

class WSpecialPredefTests extends WrappersTests {

  lazy val ctx = new WrappersCtx
  import ctx._
  import WSpecialPredef._
  import CCostedBuilder._
  import CostedBuilder._

  lazy val SPCM = WSpecialPredefCompanionMethods
  lazy val CCB = CostedBuilderMethods

  test("some") {
    val x: Ref[Int] = 10
    val opt = RWSpecialPredef.some(x)
    opt match {
      case SPCM.some(_x) => _x shouldBe x
      case _ => assert(false)
    }
  }

  test("costedValue") {
    val cost: Ref[Int] = 10
    val optCost = RWSpecialPredef.some(cost)
    val b: Ref[CostedBuilder] = RCCostedBuilder()
    val x: Ref[Long] = 1L
    val value = b.costedValue(x, optCost)
    value match {
      case CCB.costedValue(_b, _x, SPCM.some(_cost)) =>
        _b shouldBe b
        _x shouldBe x
        _cost shouldBe cost
      case _ => assert(false)
    }
  }

}
