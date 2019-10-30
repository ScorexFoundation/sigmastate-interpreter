package sigmastate.verification.contract

import sigmastate.compiler.macros.impl.ErgoContractCompiler
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext}
import stainless.annotation.ignore

@ignore
object DummyContractCompilation {

  def build(): Unit = {
    val limit = 1
    val res = ErgoContractCompiler.compile { ctx: VerifiedContext =>
      DummyContractVerification.contract(ctx, limit)
    }
    println(res)
  }
}



