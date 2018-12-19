package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform._
import scapi.sigma.DLogProtocol
import sigmastate.SCollection.SByteArray
import sigmastate._
import sigmastate.Values.{LongConstant, FalseLeaf, TrueLeaf, BigIntConstant, SigmaPropConstant, ByteArrayConstant, IntConstant, BigIntArrayConstant, SigmaBoolean, GroupElementConstant, ValUse}
import sigmastate.helpers.ErgoLikeTestProvingInterpreter
import sigmastate.interpreter.{ContextExtension, CryptoConstants}
import sigmastate.lang.DefaultSigmaBuilder.mkTaggedVariable
import sigmastate.lang.LangTests
import sigmastate.utxo._
import special.collection.{Col => VCol}
import special.sigma.{TestValue => VTestValue}
import scalan.BaseCtxTests
import scalan.util.BenchmarkUtil._

class CompilerItTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {
  import IR._
  import builder._
  import WArray._
  import WOption._
  import ColBuilder._
  import Context._
  import Col._
  import SigmaProp._
  import CostedCol._
  import CCostedCol._
  import WBigInteger._
  import WECPoint._
  import ProveDlogEvidence._
  import ProveDHTEvidence._
  import sigmastate.serialization.OpCodes._
  import Liftables._
  import SType.AnyOps


  def intConstCase = {
    Case(env, "intConst", "1", ergoCtx,
      calc = {_ => 1 },
      cost = {_ => constCost[Int]},
      size = {_ => sizeOf(1)},
      tree = IntConstant(1), Result(1, 1, 4))
  }
  test("intConstCase") {
    intConstCase.doReduce
  }

  def bigIntegerConstCase = {
    Case(env, "bigIntegerConst", "big", ergoCtx,
      calc = {_ => bigSym },
      cost = {_ => constCost[WBigInteger]},
      size = {_ => SBigInt.MaxSizeInBytes },
      tree = BigIntConstant(big), Result(big, 1, 32))
  }
  test("bigIntegerConstCase") {
    bigIntegerConstCase.doReduce
  }

  def addBigIntegerConstsCase = {
//    val size = (sizeOf(bigSym) max sizeOf(n1Sym)) + 1L
    val res = big.add(n1)
    Case(env, "addBigIntegerConsts", "big + n1", ergoCtx,
      calc = {_ => bigSym.add(n1Sym) },
      cost = {_ => constCost[WBigInteger] + constCost[WBigInteger] +
          costOf("+", SFunc(Vector(SBigInt, SBigInt), SBigInt)) },
      size = {_ => SBigInt.MaxSizeInBytes },
      tree = mkPlus(BigIntConstant(big), BigIntConstant(n1)),
      Result(res, 12, 32))
  }
  test("addBigIntegerConstsCase") {
    addBigIntegerConstsCase.doReduce()
  }

  def arrayConstCase = {
    val arr1 = env("arr1").asInstanceOf[Array[Byte]]
    val arr1Sym = liftConst(arr1)
    val col1Sym = colBuilder.fromArray[Byte](arr1Sym)
    val res = Cols.fromArray(arr1).arr
    Case(env, "arrayConst", "arr1", ergoCtx,
      calc = {_ => col1Sym },
      cost = {_ => constCost[Col[Byte]] },
      size = {_ => sizeOf(col1Sym) },
      tree = ByteArrayConstant(arr1), Result(res, 1, 2))
  }
  test("arrayConstCase") {
    arrayConstCase.doReduce()
  }

  def sigmaPropConstCase = {
    val resSym = RProveDlogEvidence(liftConst(g1.asInstanceOf[ECPoint]))
    val res = DLogProtocol.ProveDlog(g1) // NOTE! this value cannot be produced by test script
    Case(env, "sigmaPropConst", "p1", ergoCtx,
      calc = {_ => resSym },
      cost = null,
      size = {_ => CryptoConstants.groupSize.toLong },
      tree = SigmaPropConstant(p1), Result(res, 10053, 32))
  }
  test("sigmaPropConstCase") {
    sigmaPropConstCase.doReduce()
  }

  def andSigmaPropConstsCase = {
    import SigmaDslBuilder._
    val p1Sym: Rep[SigmaProp] = RProveDlogEvidence(liftConst(g1.asInstanceOf[ECPoint]))
    val p2Sym: Rep[SigmaProp] = RProveDlogEvidence(liftConst(g2.asInstanceOf[ECPoint]))
    Case(env, "andSigmaPropConsts", "p1 && p2", ergoCtx,
      calc = {_ => dsl.allZK(colBuilder.fromItems(p1Sym, p2Sym)) },
      cost = null,
      size = null,
      tree = SigmaAnd(Seq(SigmaPropConstant(p1), SigmaPropConstant(p2))),
      Result(CAND(Seq(p1, p2)), 20126, 66))
  }

  test("andSigmaPropConstsCase") {
    andSigmaPropConstsCase.doReduce()
  }

  def bigIntArray_Map_Case = {
    import SCollection._
    val res = Cols.fromArray(bigIntArr1).map(n => n.add(n1)).arr
    val arrSym = colBuilder.fromArray(liftConst(bigIntArr1))
    Case(env, "bigIntArray_Map",
      "bigIntArr1.map { (i: BigInt) => i + n1 }", ergoCtx,
      calc = { ctx =>
        val arr = liftConst(bigIntArr1)
        val vals = colBuilder.fromArray(arr)
        val costs = colBuilder.replicate(arr.length, constCost[WBigInteger])
        val sizes = colBuilder.fromArray(liftConst(bigIntArr1.map(x => SBigInt.dataSize(x.asWrappedType))))
        val arrC = RCCostedCol(vals, costs, sizes, constCost[Col[WBigInteger]])
        vals.map(fun(n => n.add(liftConst(n1))))
      },
      cost = null,
//      {_ =>
//        val arr = liftConst(bigIntArr1)
//        val opType = SFunc(Vector(SBigInt,SBigInt), SBigInt)
//        val f = fun { in: Rep[(Int, Long)] =>
//          val Pair(c, s) = in
//          val c1 = c + constCost[WBigInteger] + costOf("+", opType)
//          val c2 = costOf("+_per_item", opType) * ((s max sizeOf(liftConst(n1))) + 1L).toInt
//          c1 + c2
//        }
//        val arrSizes = colBuilder.fromArray(liftConst(Array(1L, 1L)))
//        val costs = colBuilder.replicate(arr.length, 0).zip(arrSizes).map(f)
//        constCost[Coll[WBigInteger]] + costs.sum(intPlusMonoid)
//      },
      size = {_ =>
        typeSize[WBigInteger] * liftConst(bigIntArr1).length.toLong
      },
      tree = mkMapCollection(BigIntArrayConstant(bigIntArr1), mkFuncValue(Vector((1,SBigInt)), ArithOp(ValUse(1,SBigInt), BigIntConstant(10L), -102))),
      Result(res, 23, 64))
  }
  test("bigIntArray_Map_Case") {
    bigIntArray_Map_Case.doReduce()
  }

  def bigIntArray_Slice_Case = {
    import SCollection._
    Case(env, "bigIntArray_Slice_Case",
      "bigIntArr1.slice(0,1)", ergoCtx,
      calc = null,
      cost = null,
      size = null,
      tree = null,
      Result(bigIntArr1.slice(0, 1), 21, 32))
  }
  test("bigIntArray_Slice_Case") {
    bigIntArray_Slice_Case.doReduce()
  }

//  def bigIntArray_Where_Case = {
//    import SCollection._
//    Case(env, "bigIntArray_Where_Case",
//      "bigIntArr1.where(fun (i: BigInt) = i > 0)", ergoCtx,
//      calc = null,
//      cost = null,
//      size = null,
//      tree = null,
//      Result.Ignore)
//  }
//  test("bigIntArray_Where_Case") {
//    bigIntArray_Where_Case.doReduce()
//  }

  def register_BigIntArr_Case = {
    import SCollection._
    Case(env, "register_BigIntArr_Case",
      "SELF.R4[Coll[BigInt]].get", ergoCtx,
      calc = null,
      cost = null,
      size = null,
      tree = null,
      Result(bigIntArr1, 2, 64L))
  }
  test("register_BigIntArr_Case") {
    measure(5) { i =>
      register_BigIntArr_Case.doReduce()
    }
  }

  def register_BigIntArr_Map_Case = {
    import SCollection._
    Case(env, "register_BigIntArr_Map_Case",
      "SELF.R4[Coll[BigInt]].get.map { (i: BigInt) => i + n1 }", ergoCtx,
      calc = null,
      cost = null,
      size = null,
      tree = null,
      Result(bigIntArr1.map(i => i.add(n1)), 24, 64L))
  }
  test("register_BigIntArr_Map_Case") {
    register_BigIntArr_Map_Case.doReduce()
  }

  def register_BigIntArr_Slice_Case = {
    import SCollection._
    Case(env, "register_BinIntArr_Slice_Case",
      "SELF.R4[Coll[BigInt]].get.slice(0,1)", ergoCtx,
      calc = null,
      cost = null,
      size = null,
      tree = null,
      Result(bigIntArr1.slice(0,1)/*,207, 1L*/))
  }
  test("register_BigIntArr_Slice_Case") {
    register_BigIntArr_Slice_Case.doReduce()
  }

  def crowdFunding_Case = {
    import SCollection._
    import TrivialSigma._
    import SigmaDslBuilder._
    import Box._
    import Values._
    val prover = new ErgoLikeTestProvingInterpreter()
    val backerPK  @ DLogProtocol.ProveDlog(GroupElementConstant(backer: ECPoint)) = prover.dlogSecrets(0).publicImage
    val projectPK @ DLogProtocol.ProveDlog(GroupElementConstant(project: ECPoint)) = prover.dlogSecrets(1).publicImage

    val env = envCF ++ Seq("projectPubKey" -> projectPK, "backerPubKey" -> backerPK)
    Case(env, "crowdFunding_Case", crowdFundingScript, ergoCtx,
      { ctx: Rep[Context] =>
        val backerPubKey = RProveDlogEvidence(liftConst(backer)).asRep[SigmaProp] //ctx.getVar[SigmaProp](backerPubKeyId).get
        val projectPubKey = RProveDlogEvidence(liftConst(project)).asRep[SigmaProp] //ctx.getVar[SigmaProp](projectPubKeyId).get
        val c1 = RTrivialSigma(ctx.HEIGHT >= toRep(timeout)).asRep[SigmaProp] && backerPubKey
        val c2 = RTrivialSigma(dsl.allOf(colBuilder.fromItems(
          ctx.HEIGHT < toRep(timeout),
          ctx.OUTPUTS.exists(fun { out =>
            out.value >= toRep(minToRaise) lazy_&& Thunk(out.propositionBytes === projectPubKey.propBytes)
          }))
        )).asRep[SigmaProp] && projectPubKey
        (c1 || c2)
      },
      cost = null,
      size = null,
      tree = BlockValue(Vector(
        ValDef(1,List(),SigmaPropConstant(projectPK))),
        SigmaOr(Seq(
          SigmaAnd(Seq(BoolToSigmaProp(GE(Height,LongConstant(100))),SigmaPropConstant(backerPK))),
          SigmaAnd(Seq(
            BoolToSigmaProp(AND(Vector(
              LT(Height,LongConstant(100)),
              Exists(Outputs,
                FuncValue(Vector((2,SBox)),
                  BinAnd(
                    GE(ExtractAmount(ValUse(2,SBox)),LongConstant(1000)),
                    EQ(ExtractScriptBytes(ValUse(2,SBox)), SigmaPropBytes(ValUse(1,SSigmaProp))))
                )
              )))),
            ValUse(1,SSigmaProp)
          ))))),
      Result({ TrivialProp.FalseProp }, 40674, 1L)
    )
  }
  test("crowdFunding_Case") {
    crowdFunding_Case.doReduce()
  }

  //  def register_BinIntArr_Where_Case = {
  //    import SCollection._
  //    Case(env, "contextVar_BinIntArr_Map_Case",
  //      "SELF.R4[Array[BigInt]].value.where(fun (i: BigInt) = i > 0)", ergoCtx,
  //      calc = null,
  //      cost = null,
  //      size = null,
  //      tree = null,
  //      Result.Ignore)
  //  }

}
