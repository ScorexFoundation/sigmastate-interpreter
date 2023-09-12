package special.wrappers


import scala.language.reflectiveCalls
import sigma.data.RType

class WSpecialPredefTests extends WrappersTests {

  lazy val ctx = new WrappersCtx
  import ctx._
  import WSpecialPredef._

  lazy val SPCM = WSpecialPredefCompanionMethods

  test("some") {
    val x: Ref[Int] = 10
    val opt = RWSpecialPredef.some(x)
    opt match {
      case SPCM.some(_x) => _x shouldBe x
      case _ => assert(false)
    }
  }

}
