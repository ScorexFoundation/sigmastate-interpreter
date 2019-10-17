package sigmastate.compiler.macros.test

import org.ergoplatform.dsl.ContractSyntax.ErgoScript
import org.ergoplatform.dsl.PropositionSpec
import sigmastate.Values.ErgoTree
import sigmastate.compiler.macros.SigmaDslCompiler
import sigmastate.compiler.macros.impl.PlainPropositionSpec
import sigmastate.verification.SigmaDsl.api.sigma.Context
import sigmastate.verification.contract.DummyContractVerification

object SigmaDslCompilerImpTest {

  def propositionVerified(name: String, dslSpec: sigmastate.verification.SigmaDsl.api.sigma.Context => Boolean): PlainPropositionSpec = {
    val (prop, sigmaProp) = SigmaDslCompiler.compile(dslSpec)
    val ergoTree = ErgoTree.fromProposition(sigmaProp)
    val script = ErgoScript(Map(), "}") // fails fast if compiled
    PlainPropositionSpec(name, prop, ergoTree, script)
  }

  def main(args: Array[String]): Unit = {
    val propSpec = propositionVerified("dummy", DummyContractVerification.contract)
    println(propSpec)
  }
}
