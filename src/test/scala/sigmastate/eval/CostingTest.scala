package sigmastate.eval

import java.math.BigInteger

import com.google.common.base.Strings
import org.bouncycastle.math.ec.ECPoint
import sigmastate.SType
import sigmastate.Values._
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.lang.{LangTests, Costing, TransformingSigmaBuilder, SigmaCompiler}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.SizeOf

import scalan.BaseCtxTests

class CostingTest extends BaseCtxTests with LangTests with ExampleContracts with ErgoScriptTestkit {
  import IR._
  import WArray._
  import WECPoint._
  import ProveDlogEvidence._
  import Context._; import SigmaContract._
  import Cost._; import ColBuilder._; import Col._; import Box._; import Sigma._; import CrowdFunding._
  import SigmaDslBuilder._; import WOption._

  lazy val dsl = sigmaDslBuilder

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

  test("constants") {
    check("int", "1", _ => 1, _ => costOf(IntConstant(1)))
    check("long", "1L", _ => 1L, _ => costOf(LongConstant(1)))
    check("boolean", "true", _ => true, _ => costOf(TrueLeaf))
    checkInEnv(env, "byte", "b1", _ => 1.toByte, _ => costOf(ByteConstant(1)))
    checkInEnv(env, "arr", "arr1.size",
      _ => colBuilder.fromArray(mkWArrayConst(Array[Byte](1, 2))).length,
      { _ =>
        val c = ByteArrayConstant(env("arr1").asInstanceOf[Array[Byte]])
        costOf(c) + costOf(SizeOf(c))
      })
    checkInEnv(env, "bigint", "n1", {_ => toRep(n1) }, { _ => costOf(BigIntConstant(n1))})
    checkInEnv(env, "bigint2", "big", {_ => toRep(big) }, { _ => costOf(BigIntConstant(big))})
    checkInEnv(env, "group", "g1", {_ => mkWECPointConst(g1) }, { _ => costOf(GroupElementConstant(g1))})
    checkInEnv(env, "sigmaprop", "p1",
      {_ => RProveDlogEvidence(mkWECPointConst(g1)) },
      { _ => costOf(GroupElementConstant(g1)) + costOf(p1)})
  }
  
  test("operations") {
    check("one+one", "1 + 1", _ => 1 + 1, _ => ConstantNode * 2 + TripleDeclaration)
    check("oneL+oneL", "1L - 1L", _ => 1L - 1L, _ => ConstantNode * 2 + TripleDeclaration)
    check("one_gt_one", "1 > 1", _ => false, _ => ConstantNode * 2 + TripleDeclaration)
    check("or", "1 > 1 || 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + BinOrDeclaration)
    check("or2", "1 > 1 || 2 < 1 || 2 > 1", _ => true,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + 2 * BinOrDeclaration)
    check("or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
      { ctx => (ctx.OUTPUTS.length > 1) lazy_|| Thunk(ctx.OUTPUTS.length < 1)  },
      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + BinOrDeclaration)

    check("and", "1 > 1 && 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + BinAndDeclaration)
    check("and2", "1 > 1 && 2 < 1 && 2 > 1", _ => false,
      _ => ConstantNode * 6 + TripleDeclaration * 3 + 2 * BinAndDeclaration)
    check("and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1",
      { ctx => (ctx.OUTPUTS.length > 1) lazy_&& Thunk(ctx.OUTPUTS.length < 1) },
      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + BinAndDeclaration)
  }

  test("context data") {
    check("height1", "HEIGHT + 1L", ctx => ctx.HEIGHT + 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    check("height2", "HEIGHT > 1L", ctx => ctx.HEIGHT > 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
    check("size", "INPUTS.size + OUTPUTS.size",
      ctx => { ctx.INPUTS.length + ctx.OUTPUTS.length },
      _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + TripleDeclaration)
    check("value", "SELF.value + 1L",
      ctx => ctx.SELF.value + 1L,
      _ => SelfAccess + ExtractAmount + ConstantNode + TripleDeclaration)
  }

  test("collection ops") {
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

  test("lambdas") {
    val lamCost = (ctx: Rep[Context]) => {
      toRep(LambdaDeclaration)
    }
    check("lam1", "fun (out: Box) = { out.value >= 0L }",
      ctx => fun { out: Rep[Box] => out.value >= 0L }, lamCost)
    check("lam2", "{let f = fun (out: Box) = { out.value >= 0L }; f}",
      ctx => fun { out: Rep[Box] => out.value >= 0L }, lamCost)
    check("lam3", "{let f = fun (out: Box) = { out.value >= 0L }; f(SELF) }",
      ctx => { val f = fun { out: Rep[Box] => out.value >= 0L }; Apply(f, ctx.SELF, false) },
      ctx => { toRep(LambdaDeclaration) + SelfAccess + (TripleDeclaration + ExtractAmount + LongConstantDeclaration) })
  }

  test("if then else") {
    check("lam1", "{ let x = if (OUTPUTS.size > 0) OUTPUTS(0).value else SELF.value; x }",
      { ctx => val x = IF (ctx.OUTPUTS.length > 0) THEN ctx.OUTPUTS(0).value ELSE ctx.SELF.value; x },
      { ctx =>
        val condCost = toRep(SizeOfDeclaration) + IntConstantDeclaration + TripleDeclaration
        val thenCost = toRep(ByIndexDeclaration) + ExtractAmount + IntConstantDeclaration
        val elseCost = toRep(SelfAccess) + ExtractAmount
        toRep(OutputsAccess) + condCost + IfDeclaration + (thenCost max elseCost)
      })
  }

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
