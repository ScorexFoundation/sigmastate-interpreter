package sigmastate.cost

import com.google.common.base.Strings
import sigmastate.SType
import sigmastate.Values.{SValue, ProofConstant}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.lang.{CosterCtx, LangTests, SigmaCompiler}
import sigmastate.utxo.CostTable.Cost
import scalan.BaseCtxTests

class SigmaCosterTest extends BaseCtxTests with LangTests {
  val compiler = new SigmaCompiler
  lazy val ctx = new TestContext with CosterCtx {
    import TestSigmaDslBuilder._
    val sigmaDslBuilder = RTestSigmaDslBuilder()
  }
  import ctx._
  import Context._; import SigmaContract._
  import Cost._; import ColBuilder._; import Col._; import Box._; import Sigma._; import CrowdFunding._

  def cost[SC <: SigmaContract:Elem](env: Map[String, Any], ctxVars: Map[Byte, SValue], x: String) = {
    val compiled = compiler.compile(env, x)
    val cg = ctx.buildCostedGraph[SC, SType](ctxVars, compiled)
    cg
  }

  def checkInEnv[SC <: SigmaContract:Elem, T](env: Map[String, Any], ctxVars: Map[Byte, SValue], name: String, script: String,
      expectedCalc: Rep[(SC, Context)] => Rep[T],
      expectedCost: Rep[(SC, Context)] => Rep[Int],
      doChecks: Boolean = true
  ): Rep[(((SC, Context)) => T, ((SC, Context)) => Int)] = {
    val cf = cost[SC](env, ctxVars, script)
    val p @ Pair(calcF, costF) = cf match { case cf: RFunc[(SC, Context), Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)

    if (!Strings.isNullOrEmpty(name)) {
      emit(name, p, expCalc, expCost)
    }

    val res = Pair(calcF.asRep[((SC, Context)) => T], costF.asRep[((SC, Context)) => Int])
    if (doChecks) {
      calcF shouldBe expCalc
      costF shouldBe expCost
    }
    res
  }

  def checkSC[T](name: String, script: String,
      expectedCalc: Rep[(SigmaContract, Context)] => Rep[T],
      expectedCost: Rep[(SigmaContract, Context)] => Rep[Int]
  ): Rep[(((SigmaContract, Context)) => T, ((SigmaContract, Context)) => Int)] =
    checkInEnv(env, Map(), name, script, expectedCalc, expectedCost)

  def check[SC <: SigmaContract:Elem, T](name: String, script: String,
      expectedCalc: Rep[(SC, Context)] => Rep[T],
      expectedCost: Rep[(SC, Context)] => Rep[Int]
  ): Rep[(((SC, Context)) => T, ((SC, Context)) => Int)] =
    checkInEnv(env, Map(), name, script, expectedCalc, expectedCost)

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

  val crowdFundingScript =
    """{
     | let backerPubKey = getVar[Proof](backerPubKeyId)
     | let projectPubKey = getVar[Proof](projectPubKeyId)
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
    """.stripMargin

  val timeout = 100L
  val minToRaise = 1000L
  val backerPubKeyId = 1.toByte
  val projectPubKeyId = 2.toByte
  val envCF = Map(
    "timeout" -> timeout,
    "minToRaise" -> minToRaise,
    "backerPubKeyId" -> backerPubKeyId,
    "projectPubKeyId" -> projectPubKeyId
  )

  test("Crowd Funding") {
    val prover = new ErgoLikeProvingInterpreter()
    val backer = prover.dlogSecrets(0).publicImage
    val project = prover.dlogSecrets(1).publicImage
    val vars = Seq(backerPubKeyId -> ProofConstant(backer), projectPubKeyId -> ProofConstant(project)).toMap

    checkInEnv[CrowdFunding, Boolean](envCF, vars, "CrowdFunding", crowdFundingScript,
      { in: Rep[(CrowdFunding, Context)] =>
        val Pair(cntr, ctx) = in
        val backerPubKey = ctx.getVar[Sigma](backerPubKeyId)
        val projectPubKey = ctx.getVar[Sigma](projectPubKeyId)
        val c1 = cntr.allOf(colBuilder(ctx.HEIGHT >= toRep(timeout), backerPubKey.isValid))
        val c2 = cntr.allOf(colBuilder(
          ctx.HEIGHT < toRep(timeout),
          projectPubKey.isValid,
          ctx.OUTPUTS.exists(fun { out =>
            cntr.allOf(colBuilder(out.value >= toRep(minToRaise), out.propositionBytes === projectPubKey.propBytes))
          })
        ))
        cntr.anyOf(colBuilder(c1, c2))
      },
      { in: Rep[(CrowdFunding, Context)] => HeightAccess + ConstantNode + TripleDeclaration}
      , false)
  }

  test("Crowd Funding: measure") {
    def eval(i: Int) = {
      val cf = cost[CrowdFunding](envCF ++ Seq("timeout" -> (timeout + i)), Map(), crowdFundingScript)
      split(cf)
    }
    var res: Rep[Any] = eval(0)
//    measure(2) { j => // 10 warm up iterations when j == 0
//      measure(j*500 + 10, false) { i =>
//        res = eval(i)
//      }
//    }
    emit("Crowd_Funding_measure", res)
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
