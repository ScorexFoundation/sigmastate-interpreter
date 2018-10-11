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
import SType._

import scalan.BaseCtxTests

class CostingTest extends BaseCtxTests with LangTests with ExampleContracts with ErgoScriptTestkit { cake =>
  import IR._
  import WArray._
  import WECPoint._
  import WBigInteger._
  import ProveDlogEvidence._
  import Context._; import SigmaContract._
  import Cost._; import ColBuilder._; import Col._; import Box._; import SigmaProp._; import CrowdFunding._
  import SigmaDslBuilder._; import WOption._
  import TrivialSigma._
  import Liftables._
  
  test("SType.dataSize") {
    def check(tpe: SType, v: Any, exp: Long) =
      tpe.dataSize(v.asWrappedType) shouldBe exp

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
    check(SSigmaProp, DLogProtocol.ProveDlog(g), CryptoConstants.groupSize + 1)
    check(sigmastate.SOption(SInt), Some(10), 1 + 4)
    def checkCol(elemTpe: SType, arr: Array[Any], exp: Long) =
      check(sigmastate.SCollection(SInt), arr, exp)
    checkCol(SInt, Array(10,20), 2 + 2L * 4)
    checkCol(SInt, Array(), 2)
    checkCol(SBigInt, Array(BigInteger.ZERO, BigInteger.valueOf(Long.MaxValue)), 2 + 0 + 8)
    check(STuple(SInt, STuple(SInt, SInt)), Array(10, Array[Any](20, 30)), 2 + 4 + (2 + 4 + 4))
  }

  test("constants") {
    check("int", "1", _ => 1, _ => constCost[Int], _ => sizeOf(1))
    check("long", "1L", _ => 1L, _ => constCost[Long], _ => sizeOf(1L))
    check("boolean", "true", _ => true, _ => constCost[Boolean], _ => sizeOf(true))
    checkInEnv(env, "byte", "b1", _ => 1.toByte, _ => constCost[Byte], _ => sizeOf(1.toByte))

    val arr1 = env("arr1").asInstanceOf[Array[Byte]]
    val symArr1 = colBuilder.fromArray(liftConst(arr1))
    checkInEnv(env, "arr", "arr1",
      {_ => symArr1}, {_ => constCost[Col[Byte]]}, { _ => typeSize[Byte] * symArr1.length.toLong } )
    checkInEnv(env, "arr2", "arr1.size",
      {_ => colBuilder.fromArray(liftConst(arr1)).length },
      { _ =>
        val c = ByteArrayConstant(arr1)
        costOf(c) + costOf(utxo.SizeOf(c))
      })

    val n1Sym = liftConst(n1)
    checkInEnv(env, "bigint", "n1", {_ => n1Sym }, { _ => constCost[WBigInteger] }, { _ => sizeOf(n1Sym) })

    val g1Sym = liftConst(g1.asInstanceOf[ECPoint])
    checkInEnv(env, "group", "g1", {_ => g1Sym }, {_ => constCost[WECPoint]}, { _ => typeSize[WECPoint] })

    checkInEnv(env, "sigmaprop", "p1.propBytes",
      { _ => RProveDlogEvidence(g1Sym).asRep[SigmaProp].propBytes },
      { _ => constCost[WECPoint] + constCost[SigmaProp] + costOf("SigmaPropBytes", SFunc(SSigmaProp, SCollection.SByteArray))})
  }
  
  test("operations") {
    import NumericOps._
    import builder._
    check("one+one", "1 + 1", _ => toRep(1) + 1,
      {_ => val c1 = IntConstant(1); costOf(c1) + costOf(c1) + costOf(Plus(c1, c1)) })
    checkInEnv(env, "one+one2", "big - n1", {_ => liftConst(big).subtract(liftConst(n1))})
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
    check("height1", "HEIGHT + 1L", ctx => ctx.HEIGHT + 1L)
    check("height2", "HEIGHT > 1L", ctx => ctx.HEIGHT > 1L)
    check("size", "INPUTS.size + OUTPUTS.size", ctx => { ctx.INPUTS.length + ctx.OUTPUTS.length })
    check("value", "SELF.value + 1L", ctx => ctx.SELF.value + 1L)
  }

  test("collection ops") {
    check("exists", "OUTPUTS.exists { (out: Box) => out.value >= 0L }",
      ctx => ctx.OUTPUTS.exists(fun(out => { out.value >= 0L })))
    check("forall", "OUTPUTS.forall { (out: Box) => out.value >= 0L }",
      ctx => ctx.OUTPUTS.forall(fun(out => { out.value >= 0L })))
    check("map", "OUTPUTS.map { (out: Box) => out.value >= 0L }",
      ctx => ctx.OUTPUTS.map(fun(out => { out.value >= 0L })))
//    check("where", "OUTPUTS.where(fun (out: Box) = { out.value >= 0L })",
//      ctx => ctx.OUTPUTS.filter(fun(out => { out.value >= 0L })))
  }

  test("lambdas") {
    check("lam1", "{ (out: Box) => out.value >= 0L }",
      ctx => fun { out: Rep[Box] => out.value >= 0L }, {_ => constCost[Box => Boolean]}, {_ => 8L})
    check("lam2", "{ val f = { (out: Box) => out.value >= 0L }; f }",
      ctx => fun { out: Rep[Box] => out.value >= 0L }, {_ => constCost[Box => Boolean]}, {_ => 8L})
    check("lam3", "{ val f = { (out: Box) => out.value >= 0L }; f(SELF) }",
      ctx => { val f = fun { out: Rep[Box] => out.value >= 0L }; Apply(f, ctx.SELF, false) })
  }

  test("if then else") {
    check("lam1", "{ val x = if (OUTPUTS.size > 0) OUTPUTS(0).value else SELF.value; x }",
      { ctx => val x = IF (ctx.OUTPUTS.length > 0) THEN ctx.OUTPUTS(0).value ELSE ctx.SELF.value; x })
  }

  test("Crowd Funding") {
    val prover = new ErgoLikeProvingInterpreter()
    val backerPK  @ DLogProtocol.ProveDlog(GroupElementConstant(backer: ECPoint)) = prover.dlogSecrets(0).publicImage
    val projectPK @ DLogProtocol.ProveDlog(GroupElementConstant(project: ECPoint)) = prover.dlogSecrets(1).publicImage

    val env = envCF ++ Seq("projectPubKey" -> projectPK, "backerPubKey" -> backerPK)
    checkInEnv[SigmaProp](env, "CrowdFunding", crowdFundingScript,
      { ctx: Rep[Context] =>
        val backerPubKey = RProveDlogEvidence(liftConst(backer)).asRep[SigmaProp] //ctx.getVar[SigmaProp](backerPubKeyId).get
        val projectPubKey = RProveDlogEvidence(liftConst(project)).asRep[SigmaProp] //ctx.getVar[SigmaProp](projectPubKeyId).get
        val c1 = RTrivialSigma(ctx.HEIGHT >= toRep(timeout)).asRep[SigmaProp] && backerPubKey
        val c2 = RTrivialSigma(dsl.allOf(colBuilder(
          ctx.HEIGHT < toRep(timeout),
          ctx.OUTPUTS.exists(fun { out =>
            out.value >= toRep(minToRaise) lazy_&& Thunk(out.propositionBytes === projectPubKey.propBytes)
          }))
        )).asRep[SigmaProp] && projectPubKey
        (c1 || c2)
      }
      )
  }

  test("Crowd Funding: measure") {
    val prover = new ErgoLikeProvingInterpreter()
    val backerPK  @ DLogProtocol.ProveDlog(GroupElementConstant(backer: ECPoint)) = prover.dlogSecrets(0).publicImage
    val projectPK @ DLogProtocol.ProveDlog(GroupElementConstant(project: ECPoint)) = prover.dlogSecrets(1).publicImage
    val env = envCF ++ Seq("projectPubKey" -> projectPK, "backerPubKey" -> backerPK)
    def eval(i: Int) = {
      val cf = cost(env ++ Seq("timeout" -> (timeout + i)), crowdFundingScript)
//      split(cf)
      cf
    }
    var res: Rep[Any] = eval(0)
    measure(2) { j => // 10 warm up iterations when j == 0
      measure(j*10 + 10, false) { i =>
        res = eval(i)
      }
    }
    println(s"Defs: $defCounter, Time: ${defTime / 1000000}")
    emit("Crowd_Funding_measure", res)
  }

  test("Demurrage") {
    val prover = new ErgoLikeProvingInterpreter()
    val regScriptPK  @ DLogProtocol.ProveDlog(GroupElementConstant(script: ECPoint)) = prover.dlogSecrets(0).publicImage
    val env = envDem ++ Seq("regScript" -> regScriptPK)
    checkInEnv[Boolean](env, "Demurrage", demurrageScript,
    { ctx: Rep[Context] =>
      val regScript = RProveDlogEvidence(liftConst(script)).asRep[SigmaProp] //val regScript = ctx.getVar[SigmaProp](regScriptId).get
      val c2 = dsl.allOf(colBuilder(
        ctx.HEIGHT >= ctx.SELF.getReg[Long](4).get + demurragePeriod,
        ctx.OUTPUTS.exists(fun { out =>
          val selfBytes = ctx.SELF.propositionBytes
          (out.value >= ctx.SELF.value - demurrageCost) lazy_&& Thunk{out.propositionBytes === selfBytes}
        })
      ))
      regScript.isValid lazy_|| Thunk{c2}
    }
    )
  }

  test("measure: costed context data") {
    var res: Rep[Any] = null
    measure(2) { j => // 10 warm up iterations when j == 0
      measure(j*500 + 10, false) { i =>
        res = check("", s"INPUTS.size + OUTPUTS.size + $i",
          ctx => ctx.INPUTS.length + ctx.OUTPUTS.length + i)
      }
    }
    println(s"Defs: $defCounter, Time: ${defTime / 1000000}")
  }


}
