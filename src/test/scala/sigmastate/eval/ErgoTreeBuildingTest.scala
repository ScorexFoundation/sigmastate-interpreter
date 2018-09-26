package sigmastate.eval

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.{Height, Outputs, Self, Inputs}
import scapi.sigma.DLogProtocol
import sigmastate._
import sigmastate.lang.Terms.ValueOps
import sigmastate.Values.{LongConstant, FuncValue, FalseLeaf, TrueLeaf, BlockValue, SigmaPropConstant, IntConstant, ValDef, GroupElementConstant, ValUse, TaggedVariable}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.serialization.OpCodes._

import scalan.BaseCtxTests
import sigmastate.lang.LangTests
import sigmastate.lang.Terms.Apply
import sigmastate.utxo._

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
    build(noEnv, "lam1", "fun (x: Long) = HEIGHT + x", FuncValue(Vector((1,SLong)), mkPlus(Height, ValUse(1,SLong))))
    build(noEnv, "lam2", "{ let f = fun (x: Long) = HEIGHT + x; f }", FuncValue(Vector((1,SLong)), mkPlus(Height, ValUse(1,SLong))))
    build(noEnv, "lam3", "{ OUTPUTS.exists(fun (x: Box) = HEIGHT == x.value) }",
      Exists1(Outputs,FuncValue(Vector((1,SBox)),EQ(Height,ExtractAmount(ValUse(1,SBox))))))
    build(noEnv, "lam4", "{ OUTPUTS.forall(fun (x: Box) = HEIGHT == x.value) }",
      ForAll1(Outputs,FuncValue(Vector((1,SBox)),EQ(Height,ExtractAmount(ValUse(1,SBox))))))
    build(noEnv, "lam5", "{ let f = fun (x: Long) = HEIGHT + x; f(10L) }",
      Apply(FuncValue(Vector((1,SLong)), mkPlus(Height, ValUse(1,SLong))), Vector(LongConstant(10))))
    build(noEnv, "lam6", "{ let f = fun (x: Long) = HEIGHT + x; f(10L) + f(20L) }",
      BlockValue(Vector(
        ValDef(1,List(),FuncValue(Vector((1,SLong)), Plus(Height, ValUse(1,SLong))))),
        Plus(Apply(ValUse(1,SFunc(SLong, SLong)),Vector(LongConstant(10))).asNumValue,
             Apply(ValUse(1,SFunc(SLong, SLong)),Vector(LongConstant(20))).asNumValue)))
  }

  test("Crowd Funding") {
    val prover = new ErgoLikeProvingInterpreter()
    val backerPK  @ DLogProtocol.ProveDlog(GroupElementConstant(backer: ECPoint)) = prover.dlogSecrets(0).publicImage
    val projectPK @ DLogProtocol.ProveDlog(GroupElementConstant(project: ECPoint)) = prover.dlogSecrets(1).publicImage
    val env = envCF ++ Seq("projectPubKey" -> projectPK, "backerPubKey" -> backerPK)
    build(env, "CrowdFunding", crowdFundingScript,
      BlockValue(Vector(
        ValDef(1,List(),LongConstant(100)),
        ValDef(2,List(),SigmaPropConstant(projectPK))),
        SigmaOr(Seq(
          SigmaAnd(Seq(BoolToSigmaProp(GE(Height,ValUse(1,SLong))),SigmaPropConstant(backerPK))),
          SigmaAnd(Seq(
            BoolToSigmaProp(AND(Vector(
              LT(Height,ValUse(1,SLong)),
              Exists1(Outputs, FuncValue(Vector((3,SBox)),
                  BinAnd(
                    GE(ExtractAmount(ValUse(3,SBox)),LongConstant(1000)),
                    EQ(ExtractScriptBytes(ValUse(3,SBox)), SigmaPropBytes(ValUse(2,SSigmaProp)))))
              )))),
            ValUse(2,SSigmaProp)
          ))))))
  }
}
