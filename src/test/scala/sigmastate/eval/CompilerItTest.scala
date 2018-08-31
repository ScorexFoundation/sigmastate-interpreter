package sigmastate.eval

import sigmastate.Values.IntConstant
import sigmastate.lang.LangTests

import scalan.BaseCtxTests

class CompilerItTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {
  import IR._
  import WArray._
  import ColBuilder._
  import Col._

  lazy val dsl = sigmaDslBuilder

  test("constants") {
    val ctx = newContext(height = 1, boxA1)
    checkAll(env, "int", "1", ctx,
      contract = {_ => 1},
      calc = {_ => 1},
      cost = {_ => constCost[Int]},
      size = {_ => sizeOf(1)},
      tree = IntConstant(1), 1)
//    check("long", "1L", _ => 1L, _ => constCost[Long], _ => sizeOf(1L))
//    check("boolean", "true", _ => true, _ => constCost[Boolean], _ => sizeOf(true))
//    checkInEnv(env, "byte", "b1", _ => 1.toByte, _ => constCost[Byte], _ => sizeOf(1.toByte))
//
//    val arr1 = env("arr1").asInstanceOf[Array[Byte]]
//    val symArr1 = colBuilder.fromArray(mkWArrayConst(arr1))
//    checkInEnv(env, "arr", "arr1",
//    {_ => symArr1}, {_ => constCost[Col[Byte]]}, { _ => typeSize[Byte] * symArr1.length.toLong } )
  }

}
