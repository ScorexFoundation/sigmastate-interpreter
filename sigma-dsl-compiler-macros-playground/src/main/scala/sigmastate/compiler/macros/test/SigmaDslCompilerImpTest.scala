package sigmastate.compiler.macros.test

import org.ergoplatform.dsl.ContractSyntax.ErgoScript
import sigmastate.Values.ErgoTree
import sigmastate.compiler.macros.impl.PlainPropositionSpec
import sigmastate.verification.contract.DummyContractVerification
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext, SigmaProp => VerifiedSigmaProp}

object SigmaDslCompilerImpTest {

//  def propositionVerified(name: String, dslSpec: VerifiedContext => VerifiedSigmaProp): PlainPropositionSpec = {
//    val (prop, sigmaProp) = ErgoContract(dslSpec)
//    val ergoTree = ErgoTree.fromProposition(sigmaProp)
//    val script = ErgoScript(Map(), "}") // fails fast if compiled
//    PlainPropositionSpec(name, prop, ergoTree, script)
//  }

  def main(args: Array[String]): Unit = {
    val ergoContract = ErgoContract(DummyContractVerification.contract)
    println(ergoContract)
  }
}
