package sigmastate.eval

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox}
import scapi.sigma.DLogProtocol
import sigmastate._
import sigmastate.Values.{LongConstant, FalseLeaf, TrueLeaf, BigIntConstant, SigmaPropConstant, ByteArrayConstant, IntConstant, BigIntArrayConstant, SigmaBoolean}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.LangTests
import special.sigma.{TestValue => VTestValue}

import scalan.BaseCtxTests

class CompilerItTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {
  import IR._
  import builder._
  import WArray._
  import ColBuilder._
  import Col._
  import Sigma._
  import WBigInteger._
  import WECPoint._
  import ProveDlogEvidence._
  import ProveDHTEvidence._
  import sigmastate.serialization.OpCodes._

  lazy val dsl = sigmaDslBuilder
  lazy val bigSym = mkWBigIntegerConst(big)
  lazy val n1Sym = mkWBigIntegerConst(n1)

  val backerProver = new ErgoLikeProvingInterpreter
  val projectProver = new ErgoLikeProvingInterpreter
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage
  val ctxVars = contextVars(Map(
    backerPubKeyId -> backerPubKey,
    projectPubKeyId -> projectPubKey,
    3.toByte -> bigIntArr1
  )).arr

  val boxToSpend = ErgoBox(10, TrueLeaf)
  val tx1Output1 = ErgoBox(minToRaise, projectPubKey)
  val tx1Output2 = ErgoBox(1, projectPubKey)
  val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
  val ergoCtx = ErgoLikeContext(
    currentHeight = timeout - 1,
    lastBlockUtxoRoot = AvlTreeData.dummy,
    boxesToSpend = IndexedSeq(),
    spendingTransaction = tx1,
    self = boxToSpend,
    extension = ContextExtension(Map(
      backerPubKeyId -> SigmaPropConstant(backerPubKey),
      projectPubKeyId -> SigmaPropConstant(projectPubKey),
      3.toByte -> BigIntArrayConstant(bigIntArr1)
    )))

  def intConstCase = {
    Case(env, "intConst", "1", ergoCtx, contract = {_ => 1},
      calc = {_ => 1},
      cost = {_ => constCost[Int]},
      size = {_ => sizeOf(1)},
      tree = IntConstant(1), Result(1, 1, 4))
  }

  def bigIntegerConstCase = {
    Case(env, "bigIntegerConst", "big", ergoCtx, contract = {_ => big},
      calc = {_ => bigSym },
      cost = {_ => constCost[WBigInteger]},
      size = {_ => sizeOf(bigSym)},
      tree = BigIntConstant(big), Result(big, 1, 16))
  }

  def addBigIntegerConstsCase = {
    val size = (sizeOf(bigSym) max sizeOf(n1Sym)) + 1L
    val res = big.add(n1)
    Case(env, "addBigIntegerConsts", "big + n1", ergoCtx, contract = {_ => big.add(n1)},
      calc = {_ => bigSym.add(n1Sym) },
      cost = {_ => constCost[WBigInteger] + constCost[WBigInteger] +
          costOf("+", PlusCode, SFunc(Vector(SBigInt, SBigInt), SBigInt)) +
          costOf("+_per_item", PlusCode, SFunc(Vector(SBigInt, SBigInt), SBigInt)) * size.toInt },
      size = {_ => size },
      tree = mkPlus(BigIntConstant(big), BigIntConstant(n1)),
      Result(res, 119, 17))
  }

  def arrayConstCase = {
        val arr1 = env("arr1").asInstanceOf[Array[Byte]]
        val arr1Sym = colBuilder.fromArray(mkWArrayConst(arr1))
        val res = Cols.fromArray(arr1).arr
        Case(env, "arrayConst", "arr1", ergoCtx, contract = {_ => res },
          calc = {_ => arr1Sym },
          cost = {_ => constCost[Col[Byte]] },
          size = {_ => sizeOf(arr1Sym) },
          tree = ByteArrayConstant(arr1), Result(res, 1, 2))
  }

//  def bigIntArrayVar_Map_Case = {
//        val arr1Sym = colBuilder.fromArray(mkWArrayConst(arr1))
//        val res = Cols.fromArray(arr1).arr
//        Case(env, "bigIntArrayVar_Map",
//          "getVar[Array[BigInt]](1).get.map(fun (n: BigInt) = n + 1)", ctx,
//          contract = { ctx => ctx.getVar[Col(1) },
//          calc = {_ => arr1Sym },
//          cost = {_ => constCost[Col[Byte]] },
//          size = {_ => sizeOf(arr1Sym) },
//          tree = ByteArrayConstant(arr1), Result(res, 1, 2))
//  }

  def sigmaPropConstCase = {
        val resSym = RProveDlogEvidence(mkWECPointConst(g1))
        val res = DLogProtocol.ProveDlog(g1) // NOTE! this value cannot be produced by test script
        Case(env, "sigmaPropConst", "p1", ergoCtx, contract = {_ => res },
          calc = {_ => resSym },
          cost = {_ => constCost[WECPoint] + constCost[Sigma] },
          size = {_ => sizeOf(resSym) },
          tree = SigmaPropConstant(p1), Result(res, 1 + 1, 32 + 1))
  }

  def andSigmaPropConstsCase = {
        val p1Sym: Rep[Sigma] = RProveDlogEvidence(mkWECPointConst(g1))
        val p2Sym: Rep[Sigma] = RProveDlogEvidence(mkWECPointConst(g2))
        val res = AND(p1, p2)
        val resSym = (p1Sym && p2Sym).isValid
        Case(env, "andSigmaPropConsts", "p1 && p2", ergoCtx, contract = {_ => res },
          calc = {_ => resSym },
          cost = {_ =>
            val c1 = constCost[WECPoint] + constCost[Sigma] +
                      costOf("SigmaPropIsValid", SigmaPropIsValidCode, SFunc(SSigmaProp, SBoolean))
            c1 + c1 + costOf("BinAnd", BinAndCode, SFunc(Vector(SBoolean, SBoolean), SBoolean))
          },
          size = {_ => sizeOf(resSym) },
          tree = SigmaAnd(Seq(SigmaPropConstant(p1), SigmaPropConstant(p2))).isValid,
          Result(res, (1 + 1 + 1) * 2 + 1, 1))
  }

  lazy val testCases = Seq[EsTestCase[_]](
    intConstCase , bigIntegerConstCase, addBigIntegerConstsCase, arrayConstCase, sigmaPropConstCase
  )

  test("run all") {
    for (c <- testCases)
      c.doReduce()
  }

  test("constants") {
    intConstCase.doReduce
    bigIntegerConstCase.doReduce
    addBigIntegerConstsCase.doReduce()
    arrayConstCase.doReduce()
    sigmaPropConstCase.doReduce()
    andSigmaPropConstsCase.doReduce()
  }

}
