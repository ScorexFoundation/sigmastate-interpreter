package sigmastate.eval

import java.math.BigInteger

import com.google.common.base.Strings
import org.bouncycastle.math.ec.ECPoint
import scapi.sigma.DLogProtocol
import sigmastate._
import sigmastate.Values._
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.{LangTests, Costing, TransformingSigmaBuilder, SigmaCompiler}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.{SigmaPropBytes, SizeOf}

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

  test("SType.dataSize") {
    def check(tpe: SType, v: Any, exp: Long) =
      tpe.dataSize(v.asInstanceOf[SType#WrappedType]) shouldBe exp

    check(SBoolean, true, 1)
    check(SByte, 1.toByte, 1)
    check(SShort, 1.toShort, 2)
    check(SInt, 1, 4)
    check(SLong, 1, 8)
    check(SString, "abc", 3)
    check(SBigInt, BigInteger.ZERO, 0)
    check(SBigInt, BigInteger.ONE, 1)
    check(SBigInt, BigInteger.valueOf(Long.MaxValue), 8)
    check(SBigInt, { val i = BigInteger.valueOf(Long.MaxValue); i.multiply(i) }, 16)
    val g = CryptoConstants.dlogGroup.generator
    check(SGroupElement, g, CryptoConstants.groupSize)
    check(SSigmaProp, DLogProtocol.ProveDlog(g), CryptoConstants.groupSize)
    check(sigmastate.SOption(SInt), Some(10), 1 + 4)
    def checkCol(elemTpe: SType, arr: Array[Any], exp: Long) =
      check(sigmastate.SCollection(SInt), arr, exp)
    checkCol(SInt, Array(10,20), 2 + 2L * 4)
    checkCol(SInt, Array(), 2)
    checkCol(SBigInt, Array(BigInteger.ZERO, BigInteger.valueOf(Long.MaxValue)), 2 + 0 + 8)
    check(STuple(SInt, STuple(SInt, SInt)), Array(10, Array[Any](20, 30)), 2 + 4 + (2 + 4 + 4))
  }

  test("Sized.dataSize") {
    import CostedPrim._
    import NumericOps._
    import WBigInteger._
    val V1 = mkWBigIntegerConst(BigInteger.TEN)
    val Def(IR.SizeOf(v)) = sizeOf(V1)
    v  shouldBe V1
    val V2 = mkWBigIntegerConst(big)
    val res = V1.multiply(V2)
    val s = sizeOf(res)
    emit("size", res, s)
    s should matchPattern { case Def(ApplyBinOp(_: NumericPlus[_], Def(IR.SizeOf(V1)), Def(IR.SizeOf(V2)))) => }
  }

  test("constants") {
    check("int", "1", _ => 1, _ => constCost[Int], _ => sizeOf(1))
    check("long", "1L", _ => 1L, _ => constCost[Long], _ => sizeOf(1L))
    check("boolean", "true", _ => true, _ => constCost[Boolean], _ => sizeOf(true))
    checkInEnv(env, "byte", "b1", _ => 1.toByte, _ => constCost[Byte], _ => sizeOf(1.toByte))

    val arr1 = env("arr1").asInstanceOf[Array[Byte]]
    val symArr1 = colBuilder.fromArray(mkWArrayConst(arr1))
    checkInEnv(env, "arr", "arr1",
      {_ => symArr1}, {_ => constCost[Col[Byte]]}, { _ => typeSize[Byte] * symArr1.length.toLong } )
    checkInEnv(env, "arr2", "arr1.size",
      {_ => colBuilder.fromArray(mkWArrayConst(arr1)).length },
      { _ =>
        val c = ByteArrayConstant(arr1)
        costOf(c) + costOf(utxo.SizeOf(c))
      })
//    checkInEnv(env, "bigint", "n1", {_ => toRep(n1) }, { _ => costOf(BigIntConstant(n1))})
//    checkInEnv(env, "bigint2", "big", {_ => toRep(big) }, { _ => costOf(BigIntConstant(big))})
//    checkInEnv(env, "group", "g1", {_ => mkWECPointConst(g1) }, {_ => costOf(GroupElementConstant(g1))})
//    checkInEnv(env, "sigmaprop", "p1.propBytes",
//      { _ => RProveDlogEvidence(mkWECPointConst(g1)).asRep[Sigma].propBytes },
//      { _ => costOf(GroupElementConstant(g1)) + costOf(p1) + costOf(SigmaPropBytes(SigmaPropConstant(p1)))})
  }
  
  test("operations") {
    import NumericOps._
    import builder._
    check("one+one", "1 + 1", _ => toRep(1) + 1,
      {_ => val c1 = IntConstant(1); costOf(c1) + costOf(c1) + costOf(Plus(c1, c1)) })
//    checkInEnv(env, "one+one2", "big - n1", {_ => toRep(big) - n1},
//      {_ =>
//        val c1 = BigIntConstant(big);
//        val c2 = BigIntConstant(n1);
//        costOf(c1) + costOf(c2) + costOf(Minus(c1, c2)) })
    check("one_gt_one", "1 > 1", {_ => toRep(1) > 1},
      { _ => val c1 = IntConstant(1); costOf(c1) + costOf(c1) + costOf(GT(c1, c1)) })
//    checkInEnv(env, "or", "1 > 1 || n1 < big", {_ => (toRep(1) > 1) lazy_|| Thunk(toRep(n1) < big)},
//      { _ =>
//        val (lv, lc) = {
//          val c1 = IntConstant(1);
//          val res = mkGT(c1, c1); (res, costOf(c1) + costOf(c1) + costOf(res)) }
//        val (rv, rc) = {
//          val c1 = BigIntConstant(n1);
//          val c2 = BigIntConstant(big);
//          val res = mkLT(c1, c1); (res, costOf(c1) + costOf(c2) + costOf(res)) }
//        lc + rc + costOf(mkBinOr(lv, rv))
//      })
//    check("or2", "1 > 1 || 2 < 1 || 2 > 1", _ => true,
//      _ => ConstantNode * 6 + TripleDeclaration * 3 + 2 * BinOrDeclaration)
//    check("or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
//      { ctx => (ctx.OUTPUTS.length > 1) lazy_|| Thunk(ctx.OUTPUTS.length < 1)  },
//      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + BinOrDeclaration)
//
//    check("and", "1 > 1 && 2 < 1", _ => false, _ => ConstantNode * 4 + TripleDeclaration * 2 + BinAndDeclaration)
//    check("and2", "1 > 1 && 2 < 1 && 2 > 1", _ => false,
//      _ => ConstantNode * 6 + TripleDeclaration * 3 + 2 * BinAndDeclaration)
//    check("and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1",
//      { ctx => (ctx.OUTPUTS.length > 1) lazy_&& Thunk(ctx.OUTPUTS.length < 1) },
//      _ => (OutputsAccess + SizeOfDeclaration) * 2 + TripleDeclaration * 2 + ConstantNode * 2 + BinAndDeclaration)
  }

  test("context data") {
//    check("var1", "getVar[BigInt](1)", ctx => ctx.getVar[BigInteger](1.toByte), _ => 1)
//    check("height1", "HEIGHT + 1L", ctx => ctx.HEIGHT + 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
//    check("height2", "HEIGHT > 1L", ctx => ctx.HEIGHT > 1L, _ => HeightAccess + ConstantNode + TripleDeclaration)
//    check("size", "INPUTS.size + OUTPUTS.size",
//      ctx => { ctx.INPUTS.length + ctx.OUTPUTS.length },
//      _ => InputsAccess + SizeOfDeclaration + OutputsAccess + SizeOfDeclaration + TripleDeclaration)
//    check("value", "SELF.value + 1L",
//      ctx => ctx.SELF.value + 1L,
//      _ => SelfAccess + ExtractAmount + ConstantNode + TripleDeclaration)
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
        val backerPubKey = ctx.getVar[Sigma](backerPubKeyId).get
        val projectPubKey = ctx.getVar[Sigma](projectPubKeyId).get
        val c1 = dsl.allOf(colBuilder(ctx.HEIGHT >= toRep(timeout), backerPubKey.isValid))
        val c2 = dsl.allOf(colBuilder(
          ctx.HEIGHT < toRep(timeout),
          projectPubKey.isValid,
          ctx.OUTPUTS.exists(fun { out =>
            dsl.allOf(colBuilder(out.value >= toRep(minToRaise), out.propositionBytes === projectPubKey.propBytes))
          })
        ))
        dsl.anyOf(colBuilder(c1, c2))
      }
      )
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
      val regScript = ctx.getVar[Sigma](regScriptId).get
      val c2 = dsl.allOf(colBuilder(
        ctx.HEIGHT >= ctx.SELF.R4[Long].get + demurragePeriod,
        ctx.OUTPUTS.exists(fun { out =>
          out.value >= ctx.SELF.value - demurrageCost && out.propositionBytes === ctx.SELF.propositionBytes
        })
      ))
      regScript.isValid || c2
    }
    )
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
