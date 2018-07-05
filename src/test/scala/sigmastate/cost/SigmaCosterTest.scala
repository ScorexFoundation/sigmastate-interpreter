package sigmastate.cost

import com.google.common.base.Strings
import sigmastate.SType
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.lang.{CosterCtx, LangTests, SigmaCompiler}
import sigmastate.utxo.CostTable.Cost

import scalanizer.collections.BaseCostedTests

class SigmaCosterTest extends BaseCostedTests with LangTests {
  val compiler = new SigmaCompiler
  lazy val ctx = new TestContext with CosterCtx {
  }
  import ctx._

  def cost[SC <: SigmaContract:Elem](env: Map[String, Any], x: String) = {
    val compiled = compiler.compile(env, x)
    val cg = ctx.buildCostedGraph[SC, SType](compiled)
    cg
  }

  def checkSC[T](name: String, script: String,
      expectedCalc: Rep[(SigmaContract, Context)] => Rep[T],
      expectedCost: Rep[(SigmaContract, Context)] => Rep[Int]
  ): Rep[(((SigmaContract, Context)) => T, ((SigmaContract, Context)) => Int)] =
    checkInEnv(env, name, script, expectedCalc, expectedCost)

  def check[SC <: SigmaContract:Elem, T](name: String, script: String,
      expectedCalc: Rep[(SC, Context)] => Rep[T],
      expectedCost: Rep[(SC, Context)] => Rep[Int]
  ): Rep[(((SC, Context)) => T, ((SC, Context)) => Int)] =
    checkInEnv(env, name, script, expectedCalc, expectedCost)

  def checkInEnv[SC <: SigmaContract:Elem, T](env: Map[String, Any], name: String, script: String,
      expectedCalc: Rep[(SC, Context)] => Rep[T],
      expectedCost: Rep[(SC, Context)] => Rep[Int]
  ): Rep[(((SC, Context)) => T, ((SC, Context)) => Int)] = {
    val cf = cost[SC](env, script)
    val p @ Pair(calcF, costF) = cf match { case cf: RFunc[(SC, Context), Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)

    if (!Strings.isNullOrEmpty(name))
      emit(name, p, expCalc, expCost)

    val res = Pair(calcF.asRep[((SC, Context)) => T], costF.asRep[((SC, Context)) => Int])
    calcF shouldBe expCalc
    costF shouldBe expCost
    res
  }

  import Cost._

  test("costed constants") {
    checkSC("one", "1", _ => 1, _ => ConstantNode)
    checkSC("oneL", "1L", _ => 1L, _ => ConstantNode)
  }

  test("costed operations") {
    checkSC("one+one", "1 + 1", _ => 1 + 1, _ => ConstantNode * 2 + TripleDeclaration)
    checkSC("oneL+oneL", "1L - 1L", _ => 1L - 1L, _ => ConstantNode * 2 + TripleDeclaration)
    checkSC("one_gt_one", "1 > 1", _ => false, _ => ConstantNode * 2 + TripleDeclaration)
    checkSC("or", "1 > 1 || 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + OrDeclaration)
    checkSC("or2", "1 > 1 || 2 < 1 || 2 > 1", _ => true,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + OrDeclaration)
    checkSC("or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
      { case Pair(c, ctx) => c.anyOf(colBuilder.apply(ctx.OUTPUTS.length > 1, ctx.OUTPUTS.length < 1)) },
      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + OrDeclaration)

    checkSC("and", "1 > 1 && 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + AndDeclaration)
    checkSC("and2", "1 > 1 && 2 < 1 && 2 > 1", _ => false,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + AndDeclaration)
    checkSC("and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1",
    { case Pair(c, ctx) => c.allOf(colBuilder.apply(ctx.OUTPUTS.length > 1, ctx.OUTPUTS.length < 1)) },
    _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + AndDeclaration)
  }

  test("costed context data") {
    checkSC("height1", "HEIGHT + 1L", in => in._2.HEIGHT + 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    checkSC("height2", "HEIGHT > 1L", in => in._2.HEIGHT > 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    checkSC("size", "INPUTS.size + OUTPUTS.size",
      in => {val Pair(_, ctx) = in; ctx.INPUTS.length + ctx.OUTPUTS.length},
      _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + TripleDeclaration)
    checkSC("value", "SELF.value + 1L",
      in => in._2.SELF.value + 1L,
      _ => SelfAccess + ExtractAmount + ConstantNode + TripleDeclaration)
  }

  test("Crowd Funding") {
    val timeout = 100L
    val minToRaise = 1000
    val backerPubKeyId = 1.toByte
    val projectPubKeyId = 2.toByte
    val env = Map(
      "timeout" -> timeout,
      "minToRaise" -> minToRaise,
      "backerPubKeyId" -> backerPubKeyId,
      "projectPubKeyId" -> projectPubKeyId
    )
    checkInEnv[CrowdFunding, Boolean](env, "CrowdFunding",
    """{
     | let backerPubKey = getVar[Boolean](backerPubKeyId)
     | let projectPubKey = getVar[Boolean](projectPubKeyId)
     | let c1 = HEIGHT >= timeout && backerPubKey
     | let c2 = allOf(Array(
     |   HEIGHT < timeout,
     |   projectPubKey,
     |   OUTPUTS.exists(fun (out: Box) = {
     |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
     |   })
     | ))
     | c1 || c2
     | }
    """.stripMargin,
    { in: Rep[(CrowdFunding, Context)] =>
      val Pair(cntr, ctx) = in
      val c1 = ctx.HEIGHT >= cntr.timeout && cntr.backerPubKey.isValid
      val c2 = cntr.allOf(colBuilder(
        ctx.HEIGHT < cntr.timeout,
        cntr.projectPubKey.isValid,
        ctx.OUTPUTS.exists(fun { out =>
          out.value >= cntr.minToRaise && out.propositionBytes == cntr.projectPubKey.propBytes
        })
      ))
      c1 || c2
    },
    { in: Rep[(CrowdFunding, Context)] => HeightAccess + ConstantNode + TripleDeclaration})
  }

  test("costed collection ops") {
    val cost = (in: Rep[(SigmaContract, Context)]) => {
      val Pair(_, ctx) = in
      toRep(OutputsAccess) +
          (toRep(VariableAccess) + ExtractAmount + ConstantNode + TripleDeclaration) *
              ctx.OUTPUTS.length
    }
    checkSC("exists", "OUTPUTS.exists(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.exists(fun(out => { out.value >= 0L })), cost)
    checkSC("forall", "OUTPUTS.forall(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.forall(fun(out => { out.value >= 0L })), cost)
    checkSC("map", "OUTPUTS.map(fun (out: Box) = { out.value >= 0L })",
      in => in._2.OUTPUTS.map(fun(out => { out.value >= 0L })), cost)
    checkSC("where", "OUTPUTS.where(fun (out: Box) = { out.value >= 0L })",
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
        res = checkSC("", s"INPUTS.size + OUTPUTS.size + $i",
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
