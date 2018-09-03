package sigmastate.eval

import scapi.sigma.DLogProtocol
import sigmastate._
import sigmastate.Values.{LongConstant, BigIntConstant, SigmaPropConstant, ByteArrayConstant, IntConstant, SigmaBoolean}
import sigmastate.lang.LangTests

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
  val ctx = newContext(height = 1, boxA1)

  def intConstCase = {
    Case(env, "intConst", "1", ctx, contract = {_ => 1},
      calc = {_ => 1},
      cost = {_ => constCost[Int]},
      size = {_ => sizeOf(1)},
      tree = IntConstant(1), Result(1, 1, 4))
  }

  def bigIntegerConstCase = {
    Case(env, "bigIntegerConst", "big", ctx, contract = {_ => big},
      calc = {_ => bigSym },
      cost = {_ => constCost[WBigInteger]},
      size = {_ => sizeOf(bigSym)},
      tree = BigIntConstant(big), Result(big, 1, 16))
  }

  def addBigIntegerConstsCase = {
    val size = (sizeOf(bigSym) max sizeOf(n1Sym)) + 1L
    val res = big.add(n1)
    Case(env, "addBigIntegerConsts", "big + n1", ctx, contract = {_ => big.add(n1)},
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
        Case(env, "arrayConst", "arr1", ctx, contract = {_ => res },
          calc = {_ => arr1Sym },
          cost = {_ => constCost[Col[Byte]] },
          size = {_ => sizeOf(arr1Sym) },
          tree = ByteArrayConstant(arr1), Result(res, 1, 2))
  }

  def sigmaPropConstCase = {
        val resSym = RProveDlogEvidence(mkWECPointConst(g1))
        val res = DLogProtocol.ProveDlog(g1) // NOTE! this value cannot be produced by test script
        Case(env, "sigmaPropConst", "p1", ctx, contract = {_ => res },
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
        Case(env, "andSigmaPropConsts", "p1 && p2", ctx, contract = {_ => res },
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
    intConstCase, bigIntegerConstCase, addBigIntegerConstsCase, arrayConstCase, sigmaPropConstCase
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
