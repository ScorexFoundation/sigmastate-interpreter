package sigmastate.eval

import sigmastate._
import sigmastate.Values.{ShortConstant, LongConstant, FalseLeaf, TrueLeaf, BlockValue, IntConstant, ByteConstant, ValDef, ValUse}
import sigmastate.serialization.OpCodes._

import scalan.BaseCtxTests
import sigmastate.lang.LangTests

class ErgoTreeBuildingTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    build(noEnv, "oneInt", "1", IntConstant(1))
    build(noEnv, "oneLong", "1L", LongConstant(1L))
  }

  test("binary operations") {
    build(noEnv, "one+one", "1 + 1", BlockValue(Vector(ValDef(1, IntConstant(1))), ArithOp(ValUse(1, SInt), ValUse(1, SInt), PlusCode)))
    build(noEnv, "oneL-oneL", "1L - 1L", BlockValue(Vector(ValDef(1, LongConstant(1))), ArithOp(ValUse(1, SLong), ValUse(1, SLong), MinusCode)))
    Seq((">", GT[SType] _), ("<", LT[SType] _), (">=", GE[SType] _),("<=", LE[SType] _), ("==", EQ[SType] _),("!=", NEQ[SType] _))
          .foreach { case (op, mk) =>
            build(noEnv, s"one_${op}_one", s"1 $op 2", mk(IntConstant(1), IntConstant(2)))
          }
//    build(noEnv, "or", "1 > 1 || 2 < 1", FalseLeaf)
//    build(noEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", TrueLeaf)
//    build(noEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1", TrueLeaf)
//    build(noEnv, "and", "1 > 1 && 2 < 1", FalseLeaf)
//    build(noEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", FalseLeaf)
//    build(noEnv, "and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1", FalseLeaf)
  }

//  test("context data") {
//    build(noEnv, "height1", "HEIGHT + 1L", LongConstant(101))
//    build(noEnv, "height2", "HEIGHT > 1L", TrueLeaf)
//    build(noEnv, "size", "INPUTS.size + OUTPUTS.size", IntConstant(2))
//    build(noEnv, "value", "SELF.value + 1L", LongConstant(101))
//  }

//  test("Crowd Funding") {
//    build(envCF, "CrowdFunding", crowdFundingScript, FalseLeaf)
//  }
}
