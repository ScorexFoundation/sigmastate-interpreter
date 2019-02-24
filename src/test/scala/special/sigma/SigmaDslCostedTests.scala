package special.sigma

import org.scalatest.{Matchers, FunSuite}
import special.sigma.Extensions._

class SigmaDslCostedTests extends FunSuite with ContractsTestkit with Matchers {
  val boxA1 = newAliceBox(1, 100, Map(1 -> toAnyValue(20)))
  val boxA2 = newAliceBox(2, 200)
  val ctx = newContext(10, boxA1)
      .withInputs(boxA2)
      .withVariables(Map(1 -> toAnyValue(30), 2 -> toAnyValue(40)))
  val p1: SigmaProp = new special.sigma.MockSigma(true)
  val p2: SigmaProp = new special.sigma.MockSigma(false)
  val dsl: SigmaDslBuilder = SigmaDsl

  test("CostedContext") {
    val ctxC = new CCostedContext(ctx)
    ctx.cost shouldBe 48
    ctxC.INPUTS.cost shouldBe 2
    ctxC.OUTPUTS.cost shouldBe 1
  }
}
