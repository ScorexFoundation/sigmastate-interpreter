package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import scalan.SigmaLibrary
import sigmastate.utxo.CostTable.Cost

class SigmaCosterTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  val compiler = new SigmaCompiler
  val ctx = new CosterCtx {
//    override lazy val currentPass = new DefaultPass("mypass",
//      Pass.defaultPassConfig.copy(constantPropagation = false))
  }
  import ctx._
//  val coster = new SigmaCoster(ctx)

  def cost(env: Map[String, Any], x: String) = {
    val compiled = compiler.compile(env, x)
    val cg = ctx.buildCostedGraph(compiled)
    cg
  }

  def test[T](script: String, expectedCalc: Rep[Context] => Rep[T], expectedCost: Rep[Context] => Rep[Long]): Rep[(Context => T, Context => Long)] = {
    val cf = cost(env, script)
    val Pair(calcF, costF) = cf match { case cf: RFunc[Context, Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)
    calcF shouldBe expCalc
    costF shouldBe expCost
    Pair(calcF.asRep[Context => T], costF.asRep[Context => Long])
  }

  import Cost._

  property("costed constants") {
    test("1", _ => 1, _ => ConstantNode.toLong)
    test("1L", _ => 1L, _ => ConstantNode.toLong)
  }

  property("costed operations") {
    test("1 + 1", _ => 1 + 1, _ => ConstantNode.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
    test("1L + 1L", _ => 1L + 1L, _ => ConstantNode.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
  }

  property("costed context data") {
    test("HEIGHT + 1L", ctx => ctx.HEIGHT + 1L, _ => HeightAccess.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
    test("INPUTS.size + OUTPUTS.size",
      ctx => ctx.INPUTS.length + ctx.OUTPUTS.length,
      ctx => InputsAccess.toLong + SizeOfDeclaration.toLong + OutputsAccess.toLong + SizeOfDeclaration.toLong + TripleDeclaration.toLong)
    test("SELF.value + 1L", ctx => ctx.SELF.value + 1L, ctx => SelfAccess.toLong + ExtractAmount.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
  }

  def measure[T](nIters: Int, okShow: Boolean = true)(action: Int => Unit): Unit = {
    for (i <- 0 until nIters) {
      val start = System.currentTimeMillis()
      val res = action(i)
      val end = System.currentTimeMillis()
      val iterTime = end - start
      if (okShow)
        println(s"Iter $i: $iterTime ms")
    }
  }

  property("measure: costed context data") {
    var res: Rep[Any] = null
    measure(2) { j => // 10 warm up iterations when j == 0
      measure(j*500 + 10, false) { i =>
        res = test(s"INPUTS.size + OUTPUTS.size + $i",
          ctx => ctx.INPUTS.length + ctx.OUTPUTS.length + i,
          ctx => InputsAccess.toLong + SizeOfDeclaration.toLong + OutputsAccess.toLong + SizeOfDeclaration.toLong + 2 * TripleDeclaration.toLong + ConstantNode.toLong)
      }
    }
    res.show
  }
}
