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
    forAll(unsignedIntGen) { l =>
      val c = DummyContractCompilation.contractInstance(l)
      val expectedProp = BoolToSigmaProp(LT(Height, IntConstant(l)))
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract scalaFunc") {
    val contractTrue = DummyContractCompilation.contractInstance(10000000)
    val contractFalse = DummyContractCompilation.contractInstance(0)
    forAll(ergoLikeContextGen.map(_.toSigmaContext(IR, isCost = false, Map()))) { ctx =>
      // "should be" matcher brakes the scalac compiler (probably due to stainless)
      assert(contractTrue.scalaFunc(ctx).isValid)
      assert(!contractFalse.scalaFunc(ctx).isValid)
    }
  }
}
