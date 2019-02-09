package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.R4
import sigmastate.helpers.SigmaTestingCommons
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.TestContractSpec
import scorex.crypto.hash.Blake2b256
import sigmastate.TrivialProp
import sigmastate.eval.CostingSigmaProp
import special.sigma.Extensions._


class AssetsAtomicExchangeSpec2 extends SigmaTestingCommons { suite =>
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  property("atomic exchange spec") {
    val contract = new AssetsAtomicExchange(70, spec.Coll(Blake2b256("token1")))(spec) {
      import spec._
      val tokenBuyer = ProvingParty("Alice")
      val tokenSeller = ProvingParty("Bob")
      val verifier = VerifyingParty("Miner")
    }

    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = block(0).newTransaction()
    // setup buyer's box from which we will transfer Ergs to holder box
    val mockBuyerBox = mockTx
        .outBox(100, contract.buyerSignature)
    // setup seller's box from which we will transfer tokens to holder boxes
    val mockSellerBox = mockTx
        .outBox(MinErgValue * 2, contract.sellerSignature)
        .withTokens(Token(contract.tokenId, 60))

    // ACT
    val startBlock = block(50)
    // start exchange protocol
    val (ergHolder, tokenHolder) = contract.startExchange(startBlock, mockBuyerBox, mockSellerBox, 100, Token(contract.tokenId, 60))
    // setup spending transaction
    val (buyerTokens, sellerErgs) = contract.finishExchange(startBlock, ergHolder, tokenHolder)

    // ASSERT
    val input0 = buyerTokens.tx.inputs(0)
    val res = input0.runDsl()
    res shouldBe CostingSigmaProp(TrivialProp.TrueProp)

    val buyerProof = contract.tokenBuyer.prove(input0).get
    contract.verifier.verify(input0, buyerProof) shouldBe true

    val input1 = buyerTokens.tx.inputs(1)
    val res1 = input1.runDsl()
    res1 shouldBe CostingSigmaProp(TrivialProp.TrueProp)
    val sellerProof = contract.tokenSeller.prove(input1).get
    contract.verifier.verify(input1, sellerProof) shouldBe true
  }

  property("partial filling") {
    val contract = new AssetsPartialFilling(70, spec.Coll(Blake2b256("token1")))(spec) {
      import spec._
      val tokenBuyer = ProvingParty("Alice")
      val tokenSeller = ProvingParty("Bob")
      val verifier = VerifyingParty("Miner")
    }
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which will be spent
    val mockTx = block(0).newTransaction()
    // setup buyer's box from which we will transfer Ergs to holder box
    val mockBuyerBox = mockTx
        .outBox(10000, contract.buyerSignature)
    // setup seller's box from which we will transfer tokens to holder boxes
    val mockSellerBox = mockTx
        .outBox(MinErgValue * 2, contract.sellerSignature)
        .withTokens(Token(contract.token1, 60))

    // ACT
    val startBlock = block(0)
    // start exchange protocol
    val (buyerHolder, sellerHolder) = contract.startExchange(startBlock, mockBuyerBox, mockSellerBox, 10000, Token(contract.token1, 60))

    // setup spending transaction
    val spendingTx = block(0).newTransaction().spending(buyerHolder, sellerHolder)
    spendingTx.outBox(5050, contract.buyerProp)
        .withTokens(Token(contract.token1, 10))
        .withRegs(R4 -> buyerHolder.id)
    spendingTx.outBox(4950, contract.sellerProp)
        .withTokens(Token(contract.token1, 50))
        .withRegs(R4 -> sellerHolder.id)

    // ASSERT
    val input0 = spendingTx.inputs(0)
    val buyerExt = Map(Byte.MaxValue -> toAnyValue(0.toShort))
    val res = input0.runDsl(buyerExt)
    res shouldBe CostingSigmaProp(TrivialProp.TrueProp)

    val buyerProof = contract.tokenBuyer.prove(input0, buyerExt).get
    contract.verifier.verify(input0, buyerProof) shouldBe true

    val input1 = spendingTx.inputs(1)
    val sellerExt = Map(Byte.MaxValue -> toAnyValue(1.toShort))
    val res1 = input1.runDsl(sellerExt)
    res1 shouldBe CostingSigmaProp(TrivialProp.TrueProp)
    val sellerProof = contract.tokenSeller.prove(input1, sellerExt).get
    contract.verifier.verify(input1, sellerProof) shouldBe true
  }
}
