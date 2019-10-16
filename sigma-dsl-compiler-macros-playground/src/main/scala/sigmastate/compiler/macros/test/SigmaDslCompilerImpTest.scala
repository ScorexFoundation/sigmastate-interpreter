package sigmastate.compiler.macros.test

import org.ergoplatform.dsl.PropositionSpec
import sigmastate.compiler.macros.SigmaDslCompiler
import sigmastate.verification.SigmaDsl.api.sigma.Context
import sigmastate.verification.contract.DummyContractVerification

object SigmaDslCompilerImpTest {

  def propositionVerified(name: String, dslSpec: Context => Boolean): PropositionSpec =
    SigmaDslCompiler.compile(dslSpec)

  def main(args: Array[String]): Unit = {
    val propSpec = propositionVerified("dummy", DummyContractVerification.contract)
    println(propSpec)
  }
}
