package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scalan.{SigmaLibrary}

class SigmaCosterTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  val compiler = new SigmaCompiler
  val ctx = new SigmaLibrary() {
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

  property("costed constants") {
    cost(env, "1") should matchPattern { case Def(Const(1)) => }
    cost(env, "1L") should matchPattern { case Def(Const(1L)) => }
  }

  property("costed operations") {
    cost(env, "1 + 1") should matchPattern {
      case Def(ApplyBinOp(op, Def(Const(1)), Def(Const(1)))) if op.opName == "+" =>
    }
    cost(env, "1L + 1L") should matchPattern {
      case Def(ApplyBinOp(op, Def(Const(1L)), Def(Const(1L)))) if op.opName == "+" =>
    }
  }

}
