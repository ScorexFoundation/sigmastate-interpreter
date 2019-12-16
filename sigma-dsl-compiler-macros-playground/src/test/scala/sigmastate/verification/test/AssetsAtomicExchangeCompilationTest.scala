package sigmastate.verification.test

import org.ergoplatform.{ErgoBox, Height}
import org.scalacheck.Arbitrary.arbLong
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongArrayConstant, LongConstant, SigmaPropConstant, Value}
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter, SigmaTestingCommons}
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

  property("buyer contract ergo tree test") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val pk: SigmaProp = CSigmaProp(pubkey)
    val c = AssetsAtomicExchangeCompilation.buyerContractInstance(50, byteCollGen(20).sample.get, 1, pk)
    val tree = c.ergoTree

    val spendingTransaction = createTransaction(IndexedSeq(ErgoBox(1, tree, 0)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(tree, ctx, fakeMessage).get
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

}
