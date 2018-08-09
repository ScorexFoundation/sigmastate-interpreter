package sigmastate.eval

import sigmastate.Values.{TrueLeaf, LongConstant, FalseLeaf, IntConstant}
import scalan.BaseCtxTests
import sigmastate.lang.LangTests

class ErgoTreeBuildingTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("costed constants") {
    build(noEnv, "one", "1", IntConstant(1))
//    build(noEnv, "oneL", "1L", LongConstant(1L))
  }

//  test("costed operations") {
//    build(noEnv, "one+one", "1 + 1", IntConstant(2))
//    build(noEnv, "oneL+oneL", "1L - 1L", LongConstant(0))
//    build(noEnv, "one_gt_one", "1 > 1", FalseLeaf)
//    build(noEnv, "or", "1 > 1 || 2 < 1", FalseLeaf)
//    build(noEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", TrueLeaf)
//    build(noEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size < 1", TrueLeaf)
//    build(noEnv, "and", "1 > 1 && 2 < 1", FalseLeaf)
//    build(noEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", FalseLeaf)
//    build(noEnv, "and3", "OUTPUTS.size > 1 && OUTPUTS.size < 1", FalseLeaf)
//  }
//
//  test("costed context data") {
//    build(noEnv, "height1", "HEIGHT + 1L", LongConstant(101))
//    build(noEnv, "height2", "HEIGHT > 1L", TrueLeaf)
//    build(noEnv, "size", "INPUTS.size + OUTPUTS.size", IntConstant(2))
//    build(noEnv, "value", "SELF.value + 1L", LongConstant(101))
//  }
//
//  test("Crowd Funding") {
//    build(envCF, "CrowdFunding", crowdFundingScript, FalseLeaf)
//  }
}
