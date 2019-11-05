package sigmastate.verification.contract

import stainless.annotation.ignore

@ignore
object DummyContractCompilationApp {

  def main(args: Array[String]): Unit = {
    val c = DummyContractCompilation.contractInstance(10)
    println(c)
  }
}



