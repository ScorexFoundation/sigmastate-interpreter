package sigmastate.verification.contract

import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators
import stainless.annotation.ignore

@ignore
class DummyContractCompilationTest extends SigmaTestingCommons with ObjectGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("dummy contract scalaFunc call") {
//    val c = DummyContractVerification.contractInstance(10000000)
    val c = DummyContractVerification.contractInstance(1)
    val ctx = ergoLikeContextGen.sample.get.toSigmaContext(IR, false, Map())
    val v = c.scalaFunc(ctx)
    println(v)
  }
}
