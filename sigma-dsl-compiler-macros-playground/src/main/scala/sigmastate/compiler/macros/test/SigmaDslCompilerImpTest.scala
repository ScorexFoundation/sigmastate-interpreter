package sigmastate.compiler.macros.test

import sigmastate.verification.contract.DummyContractVerification

object SigmaDslCompilerImpTest {

  def main(args: Array[String]): Unit = {
    val ergoContract = ErgoContract(DummyContractVerification.contract)
    println(ergoContract)
  }
}
