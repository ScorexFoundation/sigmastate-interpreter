package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scalan.{SigmaLibrary}

class SigmaCosterTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  val compiler = new SigmaCompiler
  val ctx = new CosterCtx {
    override val currentPass = new DefaultPass("mypass",
      Pass.defaultPassConfig.copy(constantPropagation = false))
  }
  import ctx._
  val coster = new SigmaCoster(ctx)

  def cost(env: Map[String, Any], x: String) = {
    val compiled = compiler.compile(env, x)
    val cg = coster.buildCostedGraph(compiled)
    cg
  }

  def test[T](script: String, expectedCalc: Rep[Context] => Rep[T], expectedCost: Rep[Context] => Rep[Long]) = {
    val cf = cost(env, script)
    val Pair(calcF, costF) = cf match { case cf: RFunc[Context, Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)
    calcF shouldBe expCalc
    costF shouldBe expCost
  }

  property("costed constants") {
    test("1", _ => 1)
    test("1L", _ => 1L)
  }

  property("costed operations") {
    test("1 + 1", _ => toRep(1) + toRep(1))
    test("1L + 1L", _ => toRep(1L) + toRep(1L))
  }

  property("costed context data") {
    test("HEIGHT + 1L", ctx => ctx.HEIGHT + 1L)
    test("INPUTS.size + OUTPUTS.size", ctx => ctx.INPUTS.length + ctx.OUTPUTS.length)
  }


}
