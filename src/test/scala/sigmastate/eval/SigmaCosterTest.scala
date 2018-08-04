package sigmastate.eval

import com.google.common.base.Strings
import sigmastate.SType
import sigmastate.Values.{SigmaPropConstant, SValue}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.lang.{CosterCtx, LangTests, SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.utxo.CostTable.Cost

import scalan.BaseCtxTests

class SigmaCosterTest extends BaseCtxTests with LangTests {
  lazy val ctx = new TestContext with CosterCtx {
    import TestSigmaDslBuilder._
    val sigmaDslBuilder = RTestSigmaDslBuilder()
    val builder = TransformingSigmaBuilder
  }
  import ctx._
  import Context._; import SigmaContract._
  import Cost._; import ColBuilder._; import Col._; import Box._; import Sigma._; import CrowdFunding._
  import SigmaDslBuilder._; import WOption._

  lazy val compiler = new SigmaCompiler(ctx.builder)
  lazy val dsl = sigmaDslBuilder

  def cost(env: Map[String, Any], x: String) = {
    val compiled = compiler.typecheck(env, x)
    val cg = ctx.buildCostedGraph[SType](env.mapValues(builder.liftAny(_).get), compiled)
    cg
  }

  def checkInEnv[T](env: Map[String, Any], name: String, script: String,
      expectedCalc: Rep[Context] => Rep[T],
      expectedCost: Rep[Context] => Rep[Int],
      doChecks: Boolean = true
  ): Rep[(Context => T, Context => Int)] = {
    val cf = cost(env, script)
    val p @ Pair(calcF, costF) = cf match { case cf: RFunc[Context, Costed[_]]@unchecked =>
      split(cf)
    }
    val expCalc = fun(expectedCalc)
    val expCost = fun(expectedCost)

    if (!Strings.isNullOrEmpty(name)) {
      emit(name, p, expCalc, expCost)
    }

    val res = Pair(calcF.asRep[Context => T], costF.asRep[Context => Int])
    if (doChecks) {
      calcF shouldBe expCalc
      costF shouldBe expCost
    }
    res
  }

  def check[T](name: String, script: String,
      expectedCalc: Rep[Context] => Rep[T],
      expectedCost: Rep[Context] => Rep[Int]
  ): Rep[(Context => T, Context => Int)] =
    checkInEnv(Map(), name, script, expectedCalc, expectedCost)

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
      _ => ConstantNode * 6 + TripleDeclaration * 3 + 2 * OrDeclaration)
    check("or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
      { ctx => sigmaDslBuilder.anyOf(colBuilder.apply(ctx.OUTPUTS.length > 1, ctx.OUTPUTS.length < 1 )) },
      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + OrDeclaration)

    check("and", "1 > 1 && 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + AndDeclaration)
    check("and2", "1 > 1 && 2 < 1 && 2 > 1", _ => false,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + 2 * AndDeclaration)
    check("and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1",
      { ctx => sigmaDslBuilder.allOf(colBuilder.apply(ctx.OUTPUTS.length > 1, ctx.OUTPUTS.length < 1)) },
      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + AndDeclaration)
  }

  test("costed context data") {
    check("height1", "HEIGHT + 1L", ctx => ctx.HEIGHT + 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    check("height2", "HEIGHT > 1L", ctx => ctx.HEIGHT > 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    check("size", "INPUTS.size + OUTPUTS.size",
      ctx => { ctx.INPUTS.length + ctx.OUTPUTS.length },
      _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + TripleDeclaration)
    check("value", "SELF.value + 1L",
      ctx => ctx.SELF.value + 1L,
      _ => SelfAccess + ExtractAmount + ConstantNode + TripleDeclaration)
  }

  val crowdFundingScript =
    """{
     | let backerPubKey = getVar[SigmaProp](backerPubKeyId)
     | let projectPubKey = getVar[SigmaProp](projectPubKeyId)
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

    checkInEnv[Boolean](envCF, "CrowdFunding", crowdFundingScript,
      { ctx: Rep[Context] =>
        val backerPubKey = ctx.getVar[Sigma](backerPubKeyId)
        val projectPubKey = ctx.getVar[Sigma](projectPubKeyId)
        val c1 = dsl.allOf(colBuilder(ctx.HEIGHT >= toRep(timeout), backerPubKey.isValid))
        val c2 = dsl.allOf(colBuilder(
          ctx.HEIGHT < toRep(timeout),
          projectPubKey.isValid,
          ctx.OUTPUTS.exists(fun { out =>
            dsl.allOf(colBuilder(out.value >= toRep(minToRaise), out.propositionBytes === projectPubKey.propBytes))
          })
        ))
        dsl.anyOf(colBuilder(c1, c2))
      },
      { in: Rep[Context] => HeightAccess + ConstantNode + TripleDeclaration}
      , false)
  }

  test("Crowd Funding: measure") {
    def eval(i: Int) = {
      val cf = cost(envCF ++ Seq("timeout" -> (timeout + i)), crowdFundingScript)
//      split(cf)
      cf
    }
    var res: Rep[Any] = eval(0)
    measure(2) { j => // 10 warm up iterations when j == 0
      measure(j*500 + 10, false) { i =>
        res = eval(i)
      }
    }
    println(s"Defs: $defCounter, Time: ${defTime / 1000000}")
    emit("Crowd_Funding_measure", res)
  }

  val demurrageScript =
    """{
     | let c2 = allOf(Array(
     |   HEIGHT >= SELF.R4[Long].value + demurragePeriod,
     |   OUTPUTS.exists(fun (out: Box) = {
     |     out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
     |   })
     | ))
     | getVar[SigmaProp](regScriptId) || c2
     | }
    """.stripMargin

  val demurragePeriod = 100L
  val demurrageCost = 2L
  val regScriptId = 1.toByte
  val envDem = Map(
    "demurragePeriod" -> demurragePeriod,
    "demurrageCost" -> demurrageCost,
    "regScriptId" -> regScriptId,
  )

  test("Demurrage") {
    val prover = new ErgoLikeProvingInterpreter()
    val regScript = prover.dlogSecrets(0).publicImage

    checkInEnv[Boolean](envDem, "Demurrage", demurrageScript,
    { ctx: Rep[Context] =>
      val regScript = ctx.getVar[Sigma](regScriptId)
      val c2 = dsl.allOf(colBuilder(
        ctx.HEIGHT >= ctx.SELF.R4[Long].get + demurragePeriod,
        ctx.OUTPUTS.exists(fun { out =>
          out.value >= ctx.SELF.value - demurrageCost && out.propositionBytes === ctx.SELF.propositionBytes
        })
      ))
      regScript.isValid || c2
    },
    { in: Rep[Context] => 0 } // ignored
    , false)
  }

  test("costed collection ops") {
    val cost = (ctx: Rep[Context]) => {
      toRep(OutputsAccess) +
          (toRep(ExtractAmount) + ConstantNode + TripleDeclaration) *
              ctx.OUTPUTS.length
    }
    check("exists", "OUTPUTS.exists(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.exists(fun(out => { out.value >= 0L })), cost)
    check("forall", "OUTPUTS.forall(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.forall(fun(out => { out.value >= 0L })), cost)
    check("map", "OUTPUTS.map(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.map(fun(out => { out.value >= 0L })), cost)
    check("where", "OUTPUTS.where(fun (out: Box) = { out.value >= 0L })",
      ctx => ctx.OUTPUTS.filter(fun(out => { out.value >= 0L })), cost)
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
          ctx => ctx.INPUTS.length + ctx.OUTPUTS.length + i,
          _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + 2 * TripleDeclaration + ConstantNode)
      }
    }
    res.show
  }


}
