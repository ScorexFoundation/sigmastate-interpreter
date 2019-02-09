package sigmastate.utxo.examples

import sigmastate.helpers.SigmaTestingCommons
import special.collection.Coll
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, TestContractSpec, ContractSyntax}
import special.sigma.{Context, SigmaProp}
import scorex.crypto.hash.Blake2b256
import sigmastate.TrivialProp
import sigmastate.eval.CostingSigmaProp

trait StdContracts { self: ContractSyntax =>
  import spec._
  def transferErgWithChange(tx: Transaction, from: OutBox, to: PropositionSpec, ergAmt: Long): (OutBox, Option[OutBox]) = {
    val ergChange = from.value - ergAmt
    if (ergChange < 0)
      error(s"Cannot transfer $ergAmt Ergs from $from to $to: not enough Ergs")
    val destBox = tx.outBox(ergAmt, to)
    val changeBox = if (ergChange > 0) Some(tx.outBox(ergChange, from.propSpec))
    else None
    (destBox, changeBox)
  }

  def transferTokenWithChange(tx: Transaction, from: OutBox, to: PropositionSpec, tokenAmt: Token): (OutBox, Option[OutBox]) = {
    val tokenChange = from.token(tokenAmt.id).value - tokenAmt.value
    if (tokenChange < 0)
      error(s"Cannot transfer $tokenAmt from $from to $to: not enough amount of token")

    val ergChange = from.value - MinErgValue
    if (ergChange < MinErgValue)
      error(s"Cannot transfer $tokenAmt from $from to $to: not enough amount of Erg for two boxes")

    val destBox = tx.outBox(MinErgValue, to)
        .withTokens(tokenAmt)
    val changeBox =
      if (ergChange > 0) {
        val box = tx.outBox(ergChange, from.propSpec)
        Some(box)
      }
      else None
    (destBox, changeBox)
  }

}

abstract class AssetsAtomicExchange[Spec <: ContractSpec]
    (val deadline: Int, val tokenId: Coll[Byte])
    (implicit val spec: Spec)
    extends SigmaContractSyntax with StdContracts
{
  /** The party, who wants to buy some amount of token with id == `tokenId`. */
  val tokenBuyer: spec.ProvingParty
  /** The party, who wants to sell some amount of token with id == `tokenId`. */
  val tokenSeller: spec.ProvingParty
  val verifier: spec.VerifyingParty
  def pkA = tokenBuyer.pubKey
  def pkB = tokenSeller.pubKey
  import syntax._
  lazy val env = Env("pkA" -> pkA, "pkB" -> pkB, "deadline" -> deadline, "tokenId" -> tokenId)

  lazy val buyerProp = proposition("buyer", { ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkA) || {
      val tokenData = OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)
      val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
      val c = allOf(Coll(
        tokenData._1 == tokenId,
        tokenData._2 >= 60L,
        OUTPUTS(0).propositionBytes == pkA.propBytes,
        knownId
      ))
      c
    }
  },
  env,
  """{
   |  (HEIGHT > deadline && pkA) || {
   |    val tokenData = OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)
   |    val c = allOf(Coll(
   |      tokenData._1 == tokenId,
   |      tokenData._2 >= 60L,
   |      OUTPUTS(0).propositionBytes == pkA.propBytes,
   |      OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
   |    ))
   |    c
   |  }
   |}
  """.stripMargin)

  lazy val sellerProp = proposition("seller", {ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkB) ||
        allOf(Coll(
          OUTPUTS(1).value >= 100,
          OUTPUTS(1).R4[Coll[Byte]].get == SELF.id,
          OUTPUTS(1).propositionBytes == pkB.propBytes
        ))
  },
  env,
  """{
   |  (HEIGHT > deadline && pkB) ||
   |    allOf(Coll(
   |      OUTPUTS(1).value >= 100,
   |      OUTPUTS(1).R4[Coll[Byte]].get == SELF.id,
   |      OUTPUTS(1).propositionBytes == pkB.propBytes
   |    ))
   |}
  """.stripMargin)

  lazy val buyerSignature  = proposition("buyerSignature", _ => pkA, env, "pkA")
  lazy val sellerSignature = proposition("sellerSignature", _ => pkB, env, "pkB")

  import spec._

  /** This methods starts exchange protocol using two boxes.
    * It requires that the boxes are protected by `buyerSignature` and `sellerSignature` correspondingly.
    * It creates a transaction in the target block with two holder boxes and two change boxes.
    * @return a pair of holder boxes
    */
  def startExchange(targetBlock: Block, buyerErgBox: OutBox, sellerTokenBox: OutBox, ergAmt: Long, tokenAmt: Token): (OutBox, OutBox) = {
    require(buyerErgBox.propSpec == buyerSignature && sellerTokenBox.propSpec == sellerSignature)

    val tx = targetBlock.newTransaction().spending(buyerErgBox, sellerTokenBox)
    val (buyerHolder, _) = transferErgWithChange(tx, buyerErgBox, buyerProp, ergAmt)
    val (sellerHolder, _) = transferTokenWithChange(tx, sellerTokenBox, sellerProp, tokenAmt)
    (buyerHolder, sellerHolder)
  }

  /** Having two boxes prepared for token exchange, this method creates spending transaction which
    * completes the exchange protocol.
    * @param  targetBlock   block in which the spending transaction will be created
    * @param  buyerHolder   holder box with buyer's Ergs
    * @param  sellerHolder  holder box with seller's tokens
    * @return               a pair of boxes with buyers's tokens and seller's Ergs
    * */
  def finishExchange(targetBlock: Block, buyerHolder: OutBox, sellerHolder: OutBox): (OutBox, OutBox) = {
    require(buyerHolder.propSpec == buyerProp && sellerHolder.propSpec == sellerProp)
    val spendingTx = targetBlock.newTransaction().spending(buyerHolder, sellerHolder)
    val buyerTokens = spendingTx
        .outBox(sellerHolder.value, buyerSignature)
        .withTokens(Token(tokenId, sellerHolder.token(tokenId).value))
        .withRegs(R4 -> buyerHolder.id)
    val sellerErgs = spendingTx
        .outBox(buyerHolder.value, sellerSignature)
        .withRegs(R4 -> sellerHolder.id)
    (buyerTokens, sellerErgs)
  }
}

class AssetsAtomicExchangeSpec2 extends SigmaTestingCommons { suite =>
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  lazy val contract = new AssetsAtomicExchange(70, spec.Coll(Blake2b256("token1")))(spec) {
    import spec._
    val tokenBuyer = ProvingParty("Alice")
    val tokenSeller = ProvingParty("Bob")
    val verifier = VerifyingParty("Miner")
  }

  property("atomic exchange spec") {
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
  }
}
