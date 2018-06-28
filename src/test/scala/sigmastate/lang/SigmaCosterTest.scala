package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scalan.SigmaLibrary
import sigmastate.utxo.CostTable.Cost

import scalanizer.collections.BaseCostedTests

class SigmaCosterTest extends BaseCostedTests with LangTests {
  val compiler = new SigmaCompiler
  lazy val ctx = new TestContext with CosterCtx {
  }
  import ctx._
//  val coster = new SigmaCoster(ctx)

  def cost(env: Map[String, Any], x: String) = {
    val compiled = compiler.compile(env, x)
    val cg = ctx.buildCostedGraph(compiled)
    cg
  }

  def check[T](script: String, expectedCalc: Rep[Context] => Rep[T], expectedCost: Rep[Context] => Rep[Long]): Rep[(Context => T, Context => Long)] = {
    val cf = cost(env, script)
    val Pair(calcF, costF) = cf match { case cf: RFunc[Context, Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)
    val res = Pair(calcF.asRep[Context => T], costF.asRep[Context => Long])
    calcF shouldBe expCalc
    costF shouldBe expCost
    res
  }

  import Cost._

  test("costed constants") {
    check("1", _ => 1, _ => ConstantNode.toLong)
    check("1L", _ => 1L, _ => ConstantNode.toLong)
  }

  test("costed operations") {
    check("1 + 1", _ => 1 + 1, _ => ConstantNode.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
    check("1L - 1L", _ => 1L - 1L, _ => ConstantNode.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
    check("1 > 1", _ => false, _ => ConstantNode.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
  }

  test("costed context data") {
    check("HEIGHT + 1L", ctx => ctx.HEIGHT + 1L, _ => HeightAccess.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
    check("HEIGHT > 1L", ctx => ctx.HEIGHT > 1L, _ => HeightAccess.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
    check("INPUTS.size + OUTPUTS.size",
      ctx => ctx.INPUTS.length + ctx.OUTPUTS.length,
      ctx => InputsAccess.toLong + SizeOfDeclaration.toLong + OutputsAccess.toLong + SizeOfDeclaration.toLong + TripleDeclaration.toLong)
    check("SELF.value + 1L", ctx => ctx.SELF.value + 1L, ctx => SelfAccess.toLong + ExtractAmount.toLong + ConstantNode.toLong + TripleDeclaration.toLong)
  }

  test("costed collection ops") {
    val cost = (ctx: Rep[Context]) => toRep(OutputsAccess.toLong) +
        (toRep(VariableAccess.toLong) + ExtractAmount.toLong + ConstantNode.toLong + TripleDeclaration.toLong) *
            ctx.OUTPUTS.length.toLong
    check("OUTPUTS.exists(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.exists(fun(out => { out.value >= 0L })), cost)
    check("OUTPUTS.forall(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.forall(fun(out => { out.value >= 0L })), cost)
    check("OUTPUTS.map(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.map(fun(out => { out.value >= 0L })), cost)
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

  ignore("measure: costed context data") {
    var res: Rep[Any] = null
    measure(2) { j => // 10 warm up iterations when j == 0
      measure(j*500 + 10, false) { i =>
        res = check(s"INPUTS.size + OUTPUTS.size + $i",
          ctx => ctx.INPUTS.length + ctx.OUTPUTS.length + i,
          ctx => InputsAccess.toLong + SizeOfDeclaration.toLong + OutputsAccess.toLong + SizeOfDeclaration.toLong + 2 * TripleDeclaration.toLong + ConstantNode.toLong)
      }
    }
    res.show
  }

  test("split cols") {
    ctx.emit("split_cols",
      split(fun { in: Rep[(Col[Int], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split pair cols") {
    ctx.emit("split_pair_col",
      split(fun { in: Rep[(Col[(Int, Short)], Byte)] =>
        dataCost(in)
      })
    )
    ctx.emit("split_pair_cols2",
      split(fun { in: Rep[(Col[(Int, (Short, Boolean))], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split nested cols") {
    ctx.emit("split_nested_cols",
      split(fun { in: Rep[(Col[Col[Int]], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split nested pair cols") {
    ctx.emit("split_nested_pair_cols",
      split(fun { in: Rep[(Col[Col[(Int, Short)]], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split nested nested cols") {
    ctx.emit("split_nested_nested_cols",
      split(fun { in: Rep[(Col[Col[Col[Int]]], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split nested nested pair cols") {
    ctx.emit("split_nested_nested_pair_cols",
      split(fun { in: Rep[(Col[Col[Col[(Int, Short)]]], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split complex1 cols") {
    ctx.emit("split_complex1_cols",
      split(fun { in: Rep[(Col[Col[(Col[(Int, Short)], Boolean)]], Byte)] =>
        dataCost(in)
      })
    )
  }

  test("split complex2 cols") {
    ctx.emit("split_complex2_cols",
      split(fun { in: Rep[(Col[(Col[(Col[(Int, Boolean)], Short)], Char)], Byte)] =>
        dataCost(in)
      })
    )
  }

}
