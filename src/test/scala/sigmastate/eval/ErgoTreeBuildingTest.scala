package sigmastate.eval

import org.ergoplatform.{Height, Outputs, Self, Inputs}
import sigmastate._
import sigmastate.Values.{LongConstant, FuncValue, BlockValue, IntConstant, ValDef, ValUse, FalseLeaf, TrueLeaf}
import sigmastate.serialization.OpCodes._

import scalan.BaseCtxTests
import sigmastate.lang.LangTests
import sigmastate.utxo.{ExtractAmount, SizeOf}

class ErgoTreeBuildingTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    build(noEnv, "oneInt", "1", IntConstant(1))
    build(noEnv, "oneLong", "1L", LongConstant(1L))
  }

  test("binary operations") {
    build(noEnv, "one+one", "1 + 1", BlockValue(Vector(ValDef(1, IntConstant(1))), ArithOp(ValUse(1, SInt), ValUse(1, SInt), PlusCode)))
    build(noEnv, "oneL-oneL", "1L - 1L", BlockValue(Vector(ValDef(1, LongConstant(1))), ArithOp(ValUse(1, SLong), ValUse(1, SLong), MinusCode)))
    Seq((">", GT[SType] _), ("<", LT[SType] _), (">=", GE[SType] _), ("<=", LE[SType] _), ("==", EQ[SType] _), ("!=", NEQ[SType] _))
          .foreach { case (op, mk) =>
            build(noEnv, s"one_${op}_one", s"1 $op 2", mk(IntConstant(1), IntConstant(2)))
          }
    build(noEnv, "logical", "1 > 1 || 2 < 1",
      BlockValue(
        Vector(ValDef(1, IntConstant(1))),
        BinOr(GT(ValUse(1,SInt),ValUse(1,SInt)),LT(IntConstant(2),ValUse(1,SInt)))))
    build(noEnv, "logical2", "1 > 1 && 2 < 1 || 2 > 1",
      BlockValue(Vector(
        ValDef(1,List(),IntConstant(1)),
        ValDef(2,List(),IntConstant(2))),
        BinOr(BinAnd(GT(ValUse(1,SInt),ValUse(1,SInt)),LT(ValUse(2,SInt),ValUse(1,SInt))),GT(ValUse(2,SInt),ValUse(1,SInt)))))
    build(noEnv, "logical3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
      BlockValue(Vector(
        ValDef(1,List(),SizeOf(Outputs)),
        ValDef(2,List(),IntConstant(1))),
        BinOr(GT(ValUse(1,SInt),ValUse(2,SInt)),LT(ValUse(1,SInt),ValUse(2,SInt)))))
  }

  test("context data") {
    import IR.builder._
    build(noEnv, "height1", "HEIGHT + 1L", mkPlus(Height, LongConstant(1)))
    build(noEnv, "size", "INPUTS.size + OUTPUTS.size", mkPlus(SizeOf(Inputs), SizeOf(Outputs)))
    build(noEnv, "value", "SELF.value + 1L", mkPlus(ExtractAmount(Self), LongConstant(1)))
  }

  test("simple lambdas") {
    import IR.builder._
    build(noEnv, "lam1", "fun (x: Long) = HEIGHT + x", FuncValue(Vector((2,SLong)), mkPlus(Height, ValUse(2,SLong))))
    build(noEnv, "lam2", "{ let f = fun (x: Long) = HEIGHT + x; f }", FuncValue(Vector((2,SLong)), mkPlus(Height, ValUse(2,SLong))))
  }

//  test("Crowd Funding") {
//    build(envCF, "CrowdFunding", crowdFundingScript, FalseLeaf)
//  }
}
