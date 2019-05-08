package sigmastate.eval

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.{Height, Inputs, Outputs, Self}
import sigmastate._
import sigmastate.lang.Terms.ValueOps
import sigmastate.Values.{BlockValue, FalseLeaf, FuncValue, GroupElementConstant, IntConstant, LongConstant, SigmaPropConstant, TaggedVariable, TrueLeaf, ValDef, ValUse}
import sigmastate.helpers.ContextEnrichingTestProvingInterpreter
import sigmastate.serialization.OpCodes._
import sigmastate.interpreter.Interpreter._
import scalan.BaseCtxTests
import sigmastate.basics.DLogProtocol
import sigmastate.lang.LangTests
import sigmastate.lang.Terms.Apply
import sigmastate.utxo._

class ErgoTreeBuildingTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  implicit override lazy val IR = new TestContext with IRContext with CompiletimeCosting {
    beginPass(noConstPropagationPass)
  }

  test("constants") {
    build(emptyEnv, "oneInt", "1", IntConstant(1))
    build(emptyEnv, "oneLong", "1L", LongConstant(1L))
  }

  test("binary operations") {
    build(emptyEnv, "one+one",
      "1 + 1",
      Plus(IntConstant(1), IntConstant(1))
    )
    build(emptyEnv, "oneL-oneL",
      "1L - 1L",
      Minus(LongConstant(1), LongConstant(1))
    )
    Seq((">", GT[SType] _), ("<", LT[SType] _), (">=", GE[SType] _), ("<=", LE[SType] _), ("==", EQ[SType] _), ("!=", NEQ[SType] _))
          .foreach { case (op, mk) =>
            build(emptyEnv, s"one_${op}_one", s"1 $op 2", mk(IntConstant(1), IntConstant(2)))
          }
    build(emptyEnv, "logical", "1 > 1 || 2 < 1",
      BinOr(GT(IntConstant(1),IntConstant(1)),LT(IntConstant(2),IntConstant(1)))
    )
    build(emptyEnv, "logical2", "1 > 1 && 2 < 1 || 2 > 1",
      BinOr(
        BinAnd(
          GT(IntConstant(1), IntConstant(1)),
          LT(IntConstant(2), IntConstant(1))
        ),
        GT(IntConstant(2), IntConstant(1))
      )
    )
    build(emptyEnv, "logical3", "OUTPUTS.size > 1 || OUTPUTS.size < 1",
      BlockValue(
        Vector(ValDef(1, List(), SizeOf(Outputs))),
        BinOr(GT(ValUse(1, SInt), IntConstant(1)), LT(ValUse(1, SInt), IntConstant(1))))
    )
    build(emptyEnv, "logical4", "OUTPUTS.size > 1 ^ OUTPUTS.size < 1",
      BlockValue(
        Vector(ValDef(1, List(), SizeOf(Outputs))),
        BinXor(GT(ValUse(1, SInt), IntConstant(1)), LT(ValUse(1, SInt), IntConstant(1))))
    )
  }

  test("context data") {
    import IR.builder._
    build(emptyEnv, "height1", "HEIGHT + 1L", mkPlus(Height, LongConstant(1)))
    build(emptyEnv, "size", "INPUTS.size + OUTPUTS.size", mkPlus(SizeOf(Inputs), SizeOf(Outputs)))
    build(emptyEnv, "value", "SELF.value + 1L", mkPlus(ExtractAmount(Self), LongConstant(1)))
  }

  test("simple lambdas") {
    import IR.builder._
    build(emptyEnv, "lam1", "{ (x: Long) => HEIGHT + x }", FuncValue(Vector((1,SLong)), mkPlus(Height, ValUse(1,SLong))))
    build(emptyEnv, "lam2", "{ val f = { (x: Long) => HEIGHT + x }; f }", FuncValue(Vector((1,SLong)), mkPlus(Height, ValUse(1,SLong))))
    build(emptyEnv, "lam3", "{ OUTPUTS.exists { (x: Box) => HEIGHT == x.value } }",
      Exists(Outputs, FuncValue(Vector((1,SBox)),EQ(Upcast(Height, SLong),ExtractAmount(ValUse(1,SBox))))))
    build(emptyEnv, "lam4", "{ OUTPUTS.forall { (x: Box) => HEIGHT == x.value } }",
      ForAll(Outputs,FuncValue(Vector((1,SBox)),EQ(Upcast(Height, SLong),ExtractAmount(ValUse(1,SBox))))))
    build(emptyEnv, "lam5", "{ val f = { (x: Long) => HEIGHT + x }; f(10L) }",
      Apply(FuncValue(Vector((1,SLong)), mkPlus(Upcast(Height, SLong), ValUse(1,SLong))), Vector(LongConstant(10))))
    build(emptyEnv, "lam6", "{ val f = { (x: Long) => HEIGHT + x }; f(10L) + f(20L) }",
      BlockValue(Vector(
        ValDef(1,List(),FuncValue(Vector((1,SLong)), Plus(Upcast(Height, SLong), ValUse(1,SLong))))),
        Plus(Apply(ValUse(1,SFunc(SLong, SLong)),Vector(LongConstant(10))).asNumValue,
             Apply(ValUse(1,SFunc(SLong, SLong)),Vector(LongConstant(20))).asNumValue)))
    build(emptyEnv, "lam7", "{ def f(x: Long) = HEIGHT + x; f }", FuncValue(Vector((1,SLong)), mkPlus(Height, ValUse(1,SLong))))
  }

  test("Crowd Funding") {
    val prover = new ContextEnrichingTestProvingInterpreter()
    val backerPK  = prover.dlogSecrets(0).publicImage
    val projectPK = prover.dlogSecrets(1).publicImage
    val env = envCF ++ Seq("projectPubKey" -> projectPK, "backerPubKey" -> backerPK)
    build(env, "CrowdFunding", crowdFundingScript,
      BlockValue(Vector(
        ValDef(1,List(),SigmaPropConstant(projectPK))),
        SigmaOr(Seq(
          SigmaAnd(Seq(BoolToSigmaProp(GE(Height,IntConstant(100))),SigmaPropConstant(backerPK))),
          SigmaAnd(Seq(
            BoolToSigmaProp(AND(Vector(
              LT(Height,IntConstant(100)),
              Exists(Outputs, FuncValue(Vector((2,SBox)),
                  BinAnd(
                    GE(ExtractAmount(ValUse(2,SBox)),LongConstant(1000)),
                    EQ(ExtractScriptBytes(ValUse(2,SBox)), SigmaPropBytes(ValUse(1,SSigmaProp)))))
              )))),
            ValUse(1,SSigmaProp)
          ))))))
  }
}
