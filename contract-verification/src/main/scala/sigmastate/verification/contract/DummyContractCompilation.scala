package sigmastate.verification.contract

import sigmastate.compiler.macros.impl.ErgoContractCompiler
import stainless.annotation.ignore

@ignore
object DummyContractCompilation {

  def build(): Unit = {
    val res = ErgoContractCompiler.compile(DummyContractVerification.contract)
    println(res)
  }
}
