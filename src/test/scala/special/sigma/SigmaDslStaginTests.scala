package special.sigma

import special.wrappers.WrappersTests

import scala.language.reflectiveCalls
import scalan.SigmaLibrary
import special.sigma.Extensions._

class SigmaDslStaginTests extends WrappersTests with ContractsTestkit {
  class Ctx extends WrappersCtx with SigmaLibrary {
    import TestSigmaDslBuilder._
    val sigmaDslBuilder = RTestSigmaDslBuilder()
  }

  test("invokeUnlifted") {
    val cake = new Ctx
    import cake._
    import Liftables._
    import Context._
    import Box._
    import SigmaProp._
    import SigmaDslBuilder._
    import EnvRep._

    val dsl: SSigmaDslBuilder = SigmaDsl
    type RSigmaDslBuilder = cake.SigmaDslBuilder
    type RContext = cake.Context
    type RBox = cake.Box
    type RSigmaProp = cake.SigmaProp
    val boxA1 = newAliceBox(1, 100, Map(1 -> toAnyValue(20), 3 -> toAnyValue((10 -> SigmaDsl.Colls.fromArray(Array.emptyByteArray)))))
    val boxA2 = newAliceBox(2, 200)
    val ctx: SContext = newContext(10, boxA1)
      .withInputs(boxA2)
      .withVariables(Map(1 -> toAnyValue(30), 2 -> toAnyValue(40)))
    val p1: SSigmaProp = new special.sigma.MockSigma(true)
    val p2: SSigmaProp = new special.sigma.MockSigma(false)

    check(dsl,  { env: EnvRep[RSigmaDslBuilder] =>
      for { dsl <- env; arg <- lifted(true) } yield dsl.sigmaProp(arg) }, dsl.sigmaProp(true))

    check(ctx, { env: EnvRep[RContext] => for { obj <- env } yield obj.SELF }, ctx.SELF)
    check(ctx, { env: EnvRep[RContext] =>
      for { obj <- env; id <- lifted(1.toByte) } yield obj.getVar[Int](id) }, ctx.getVar[Int](1))

    check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.value }, boxA1.value)
    check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.creationInfo }, boxA1.creationInfo)
    check(boxA1, { env: EnvRep[RBox] => for { obj <- env; arg <- lifted(1) } yield obj.getReg[Int](arg) }, boxA1.getReg[Int](1))
    check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.registers }, boxA1.registers)

    check(p1, { env: EnvRep[RSigmaProp] => for { p1 <- env; arg <- lifted(true) } yield p1 && arg }, p1 && true)
    check(p1, { env: EnvRep[RSigmaProp] => for { p1 <- env; arg <- lifted(p2) } yield p1 && arg }, p1 && p2)

    val th = () => p2
//    check(p1, { env: EnvRep[RSigmaProp] => for { p1 <- env; thL <- lifted(th) } yield p1.lazyAnd(thL) }, p1.lazyAnd(th()))
  }
}
