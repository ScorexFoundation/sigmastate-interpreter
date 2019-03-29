package special.sigma

import special.wrappers.WrappersTests
import special.collection._
import scala.language.reflectiveCalls
import scalan.{SigmaLibrary, BaseCtxTests, BaseLiftableTests}
import sigmastate.eval.Extensions._
import sigmastate.eval.{IRContext, ErgoScriptTestkit}
import sigmastate.helpers.SigmaTestingCommons

class SigmaDslStaginTests extends BaseCtxTests with ErgoScriptTestkit with BaseLiftableTests {
  class Ctx extends TestContext with IRContext with LiftableTestKit {
  }

  test("invokeUnlifted") {
    val cake = new Ctx
    import cake._
    import Liftables._
    import Context._
    import Coll._
    import Box._
    import SigmaProp._
    import SigmaDslBuilder._
    import EnvRep._

    val dsl: SSigmaDslBuilder = sigmastate.eval.SigmaDsl
    type RSigmaDslBuilder = cake.SigmaDslBuilder
    type RContext = cake.Context
    type RBox = cake.Box
    type RSigmaProp = cake.SigmaProp
    val boxA1 = newAliceBox(1, 100)
    val boxA2 = newAliceBox(2, 200)
    val ctx: SContext = newContext(10, boxA1)
      .withInputs(boxA2)
      .withVariables(Map(1 -> toAnyValue(30), 2 -> toAnyValue(40)))
    val p1: SSigmaProp = new special.sigma.MockSigma(true)
    val p2: SSigmaProp = new special.sigma.MockSigma(false)

    cake.check(dsl,  { env: EnvRep[RSigmaDslBuilder] =>
      for { dsl <- env; arg <- lifted(true) } yield dsl.sigmaProp(arg) }, dsl.sigmaProp(true))

    cake.check(ctx, { env: EnvRep[RContext] => for { obj <- env } yield obj.SELF }, ctx.SELF)
    cake.check(ctx, { env: EnvRep[RContext] =>
      for { obj <- env; id <- lifted(1.toByte) } yield obj.getVar[Int](id) }, ctx.getVar[Int](1))

    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.value }, boxA1.value)
    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.creationInfo }, boxA1.creationInfo)
    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env; arg <- lifted(1) } yield obj.getReg[Coll[Byte]](arg) }, boxA1.getReg[special.collection.Coll[Byte]](1))
    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.registers }, boxA1.registers)

    cake.check(p1, { env: EnvRep[RSigmaProp] => for { p1 <- env; arg <- lifted(true) } yield p1 && arg }, p1 && true)
    cake.check(p1, { env: EnvRep[RSigmaProp] => for { p1 <- env; arg <- lifted(p2) } yield p1 && arg }, p1 && p2)
  }
}
