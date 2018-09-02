package sigmastate.eval

import sigmastate.{SBigInt, SFunc}
import sigmastate.Values.{LongConstant, IntConstant, BigIntConstant, ByteArrayConstant}
import sigmastate.lang.LangTests

import scalan.BaseCtxTests

class CompilerItTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {
  import IR._
  import builder._
  import WArray._
  import ColBuilder._
  import Col._
  import WBigInteger._
  import sigmastate.serialization.OpCodes._

  lazy val dsl = sigmaDslBuilder
  lazy val bigSym = mkWBigIntegerConst(big)
  lazy val n1Sym = mkWBigIntegerConst(n1)
  val ctx = newContext(height = 1, boxA1)

  def intCase = {
    Case(env, "int", "1", ctx, contract = {_ => 1},
      calc = {_ => 1},
      cost = {_ => constCost[Int]},
      size = {_ => sizeOf(1)},
      tree = IntConstant(1), Result(1, 1, 4))
  }

  def bigIntegerCase = {
    Case(env, "BigInteger", "big", ctx, contract = {_ => big},
      calc = {_ => bigSym },
      cost = {_ => constCost[WBigInteger]},
      size = {_ => sizeOf(bigSym)},
      tree = BigIntConstant(big), Result(big, 1, 16))
  }

  def addBigIntegersCase = {
    val size = (sizeOf(bigSym) max sizeOf(n1Sym)) + 1L
    val res = big.add(n1)
    Case(env, "BigInteger2", "big + n1", ctx, contract = {_ => big.add(n1)},
      calc = {_ => bigSym.add(n1Sym) },
      cost = {_ => constCost[WBigInteger] + constCost[WBigInteger] +
          costOf("+", PlusCode, SFunc(Vector(SBigInt, SBigInt), SBigInt)) +
          costOf("+_per_item", PlusCode, SFunc(Vector(SBigInt, SBigInt), SBigInt)) * size.toInt },
      size = {_ => size },
      tree = mkPlus(BigIntConstant(big), BigIntConstant(n1)),
      Result(res, 119, 17))
  }

  def arrayCase = {
        val arr1 = env("arr1").asInstanceOf[Array[Byte]]
        val arr1Sym = colBuilder.fromArray(mkWArrayConst(arr1))
        val res = Cols.fromArray(arr1).arr
        Case(env, "arr", "arr1", ctx, contract = {_ => res },
          calc = {_ => arr1Sym },
          cost = {_ => constCost[Col[Byte]] },
          size = {_ => sizeOf(arr1Sym) },
          tree = ByteArrayConstant(arr1), Result(res))
  }

  lazy val testCases = Seq[EsTestCase[_]](
    intCase, bigIntegerCase, addBigIntegersCase
  )

  test("run all") {
    for (c <- testCases)
      c.doReduce()
  }

  test("constants") {
//    intCase.doReduce
//    bigIntegerCase.doReduce
//    addBigIntegersCase.doReduce()
      arrayCase.doReduce()

  }

}
