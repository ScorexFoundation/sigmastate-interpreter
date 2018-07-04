package sigmastate.cost

import com.google.common.base.Strings
import sigmastate.lang.{CosterCtx, LangTests, SigmaCompiler}
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

  def check[T](name: String, script: String,
      expectedCalc: Rep[(SigmaContract, Context)] => Rep[T],
      expectedCost: Rep[(SigmaContract, Context)] => Rep[Int]
  ): Rep[(((SigmaContract, Context)) => T, ((SigmaContract, Context)) => Int)] = {
    val cf = cost(env, script)
    val p @ Pair(calcF, costF) = cf match { case cf: RFunc[Context, Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)

    if (!Strings.isNullOrEmpty(name))
      emit(name, p, expCalc, expCost)

    val res = Pair(calcF.asRep[((SigmaContract, Context)) => T], costF.asRep[((SigmaContract, Context)) => Int])
    calcF shouldBe expCalc
    costF shouldBe expCost
    res
  }

  import Cost._

  test("costed constants") {
    check("one", "1", _ => 1, _ => ConstantNode)
    check("oneL", "1L", _ => 1L, _ => ConstantNode)
  }

  test("costed operations") {
    check("one+one", "1 + 1", _ => 1 + 1, _ => ConstantNode * 2 + TripleDeclaration)
    check("oneL+oneL", "1L - 1L", _ => 1L - 1L, _ => ConstantNode * 2 + TripleDeclaration)
    check("one_gt_one", "1 > 1", _ => false, _ => ConstantNode * 2 + TripleDeclaration)
    check("or", "1 > 1 || 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + OrDeclaration)
    check("or2", "1 > 1 || 2 < 1 || 2 > 1", _ => true,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + OrDeclaration)
    check("or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
      { case Pair(c, ctx) => c.anyOf(colBuilder.apply(ctx.OUTPUTS.length > 1, ctx.OUTPUTS.length < 1)) },
      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + OrDeclaration)

    check("and", "1 > 1 && 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + AndDeclaration)
    check("and2", "1 > 1 && 2 < 1 && 2 > 1", _ => false,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + AndDeclaration)
    check("and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1",
    { case Pair(c, ctx) => c.allOf(colBuilder.apply(ctx.OUTPUTS.length > 1, ctx.OUTPUTS.length < 1)) },
    _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + AndDeclaration)
  }

  test("costed context data") {
    check("height1", "HEIGHT + 1L", in => in._2.HEIGHT + 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    check("height2", "HEIGHT > 1L", in => in._2.HEIGHT > 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    check("size", "INPUTS.size + OUTPUTS.size",
      in => {val Pair(_, ctx) = in; ctx.INPUTS.length + ctx.OUTPUTS.length},
      _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + TripleDeclaration)
    check("value", "SELF.value + 1L",
      in => in._2.SELF.value + 1L,
      _ => SelfAccess + ExtractAmount + ConstantNode + TripleDeclaration)
  }

  test("costed collection ops") {
    val cost = (in: Rep[(SigmaContract, Context)]) => {
      val Pair(_, ctx) = in
      toRep(OutputsAccess) +
          (toRep(VariableAccess) + ExtractAmount + ConstantNode + TripleDeclaration) *
              ctx.OUTPUTS.length
    }
    check("exists", "OUTPUTS.exists(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.exists(fun(out => { out.value >= 0L })), cost)
    check("forall", "OUTPUTS.forall(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.forall(fun(out => { out.value >= 0L })), cost)
    check("map", "OUTPUTS.map(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.map(fun(out => { out.value >= 0L })), cost)
    check("where", "OUTPUTS.where(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.filter(fun(out => { out.value >= 0L })), cost)
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
        res = check("", s"INPUTS.size + OUTPUTS.size + $i",
          in => in._2.INPUTS.length + in._2.OUTPUTS.length + i,
          _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + 2 * TripleDeclaration + ConstantNode)
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
