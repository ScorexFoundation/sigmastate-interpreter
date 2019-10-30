package sigmastate.compiler.macros.test

import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext}
import sigmastate.verification.contract.DummyContractVerification

object SigmaDslCompilerImpTest {

  def main(args: Array[String]): Unit = {
    val limit = 1
    val ergoContract = ErgoContract{ ctx: VerifiedContext =>
      DummyContractVerification.contract(ctx, limit) }
    println(ergoContract)
  }
}
