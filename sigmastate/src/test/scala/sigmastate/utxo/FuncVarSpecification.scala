package sigmastate.utxo

import org.ergoplatform.ErgoLikeContext
import sigmastate.Values.Constant
import sigmastate.eval.CFunc
import sigmastate.SType.AnyOps
import sigmastate.{SInt, SFunc}
import sigmastate.helpers.{SigmaTestingCommons, ContextEnrichingTestProvingInterpreter}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.lang.Terms.ValueOps

class FuncVarSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
  }

  // TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/417
  ignore("Func context variable") {
//    val scriptId = 21.toByte
//    val code = compileWithCosting(emptyEnv, s"{ (x: Int) => x + 1 }")
//    val ctx = ErgoLikeContext.dummy(fakeSelf)
//
//    val prover = new ContextEnrichingTestProvingInterpreter()
//        .withContextExtender(scriptId, Constant(CFunc[Int, Int](ctx, code).asWrappedType, SFunc(SInt, SInt)))
//    val prop = compileWithCosting(emptyEnv, s"{ val f = getVar[Int => Int](1).get; f(10) > 0 }").asBoolValue.asSigmaProp
//    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).getOrThrow
//
  }
}
