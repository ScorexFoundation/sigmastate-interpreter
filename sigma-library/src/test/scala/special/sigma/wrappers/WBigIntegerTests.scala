package special.sigma.wrappers

import java.math.BigInteger
import special.wrappers.WrappersTests
import scala.collection.mutable
import scala.language.reflectiveCalls

class WBigIntegerTests extends WrappersTests {
  class Ctx extends WrappersCtx with WrappersModule {
  }

  test("invokeUnlifted") {
    val ctx = new Ctx
    import ctx._
    import Liftables._
    import WBigInteger._
    import EnvRep._
    
    val obj = BigInteger.valueOf(10L)

    check(obj, { env: EnvRep[WBigInteger] =>
      for { xs <- env; one <- lifted(BigInteger.ONE) } yield xs.add(one) }, obj.add(BigInteger.ONE))
    check(obj, { env: EnvRep[WBigInteger] => for { xs <- env } yield xs.multiply(xs) }, obj.multiply(obj))
  }
}
