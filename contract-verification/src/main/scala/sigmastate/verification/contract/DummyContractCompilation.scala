package sigmastate.verification.contract

import sigmastate.compiler.macros.impl.{ErgoContract, ErgoContractCompiler}
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext}
import stainless.annotation.ignore

@ignore
object DummyContractCompilation {

  def dummyContractInstance(limit: Int): ErgoContract =
    ErgoContractCompiler.compile { ctx: VerifiedContext =>
      DummyContractVerification.contract(ctx, limit)
    }

  val c: ErgoContract = dummyContractInstance(10)
}



