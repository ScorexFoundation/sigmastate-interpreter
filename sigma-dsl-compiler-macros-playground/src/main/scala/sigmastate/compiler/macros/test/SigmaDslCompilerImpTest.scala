package sigmastate.compiler.macros.test

import sigmastate.compiler.macros.SigmaDslCompiler
import sigmastate.verification.SigmaDsl.impl.sigma.PK
import sigmastate.verification.contract.CrowdFundingContractVerification

object SigmaDslCompilerImpTest {

  def main(args: Array[String]): Unit = {
    val pkBacker = PK("9h7DHKSDgE4uvP8313GVGdsEg3AvdAWSSTG7XZsLwBfeth4aePG")
    val pkProject = PK("9gBSqNT9LH9WjvWbyqEvFirMbYp4nfGHnoWdceKGu45AKiya3Fq")
    val contract = (CrowdFundingContractVerification.crowdFundingContract(5000, 20000, pkBacker, pkProject)(_))
      .asInstanceOf[Any => Boolean]
    SigmaDslCompiler.compile(contract)
  }
}
