package sigmastate.verification.contract

import org.ergoplatform.Height
import sigmastate.{BoolToSigmaProp, LT}
import sigmastate.Values.IntConstant
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators
import stainless.annotation.ignore

@ignore
class DummyContractCompilationTest extends SigmaTestingCommons with ObjectGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("dummy contract ergo tree") {
    val c = DummyContractCompilation.contractInstance(10)
    assert(c.prop == BoolToSigmaProp(LT(Height, IntConstant(10))))
  }

  property("dummy contract scalaFunc success") {
    val c = DummyContractCompilation.contractInstance(10000000)
    val ctx = ergoLikeContextGen.sample.get.toSigmaContext(IR, false, Map())
    val v = c.scalaFunc(ctx)
    // "should be" matcher brakes the scalac compiler (probably due to stainless)
    assert(v.isValid)
  }

  property("dummy contract scalaFunc fail") {
    val c = DummyContractCompilation.contractInstance(0)
    val ctx = ergoLikeContextGen.sample.get.toSigmaContext(IR, false, Map())
    val v = c.scalaFunc(ctx)
    assert(!v.isValid)
  }
}
