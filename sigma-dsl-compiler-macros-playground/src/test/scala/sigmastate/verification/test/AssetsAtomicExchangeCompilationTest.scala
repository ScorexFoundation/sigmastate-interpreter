package sigmastate.verification.test

import org.ergoplatform.Height
import org.scalacheck.Arbitrary.arbLong
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongArrayConstant, LongConstant, SigmaPropConstant, Value}
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utxo.{ByIndex, SelectField, SizeOf}
import sigmastate.verification.contract.{AssetsAtomicExchangeCompilation, DummyContractCompilation}
import sigmastate.verified.VerifiedTypeConverters._
import sigmastate._
import special.collection.{Coll, CollOverArray}
import special.sigma.SigmaProp

class AssetsAtomicExchangeCompilationTest extends SigmaTestingCommons with MiscGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("buyer contract ergo tree") {
    forAll(unsignedIntGen, byteCollGen(0, 40), arbLong.arbitrary, proveDlogGen) {
      case (deadline, tokenId, tokenAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c = AssetsAtomicExchangeCompilation.buyerContractInstance(deadline, tokenId, tokenAmount, pk)
        val expectedProp = BoolToSigmaProp(LT(Height, Height))
        assert(c.prop == expectedProp)
    }
  }

}
