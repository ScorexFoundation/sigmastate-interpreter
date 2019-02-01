package special.sigma.wrappers

import java.math.BigInteger

import org.bouncycastle.crypto.ec.CustomNamedCurves
import special.wrappers.WrappersTests
import scala.language.reflectiveCalls

class WECPointTests extends WrappersTests {
  class Ctx extends WrappersCtx with WrappersModule {
  }

  test("invokeUnlifted") {
    val ctx = new Ctx
    import ctx._
    import WECPoint._
    import WBigInteger._
    import Liftables._
    import EnvRep._

    val obj = CustomNamedCurves.getByName("curve25519").getG
    val ten = BigInteger.valueOf(10L)
    check(obj, { env: EnvRep[WECPoint] => for { xs <- env } yield xs.add(xs) }, obj.add(obj))
    check(obj, { env: EnvRep[WECPoint] => for { xs <- env; tenL <- lifted(ten) } yield xs.multiply(tenL) }, obj.multiply(ten))
    check(obj, { env: EnvRep[WECPoint] => for { xs <- env; arg <- lifted(true) } yield xs.getEncoded(arg) }, obj.getEncoded(true))
  }
}
