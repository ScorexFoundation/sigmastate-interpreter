package sigmastate.utxo.examples

import sigmastate.helpers.SigmaTestingCommons
import special.collection.Coll
import org.ergoplatform.ErgoBox.R4
import special.sigma.{Context, SpecContext, SigmaProp}
import scorex.crypto.hash.Blake2b256
import sigmastate.TrivialProp
import sigmastate.eval.CostingSigmaProp

class AssetsAtomicExchangeSpec2 extends SigmaTestingCommons { suite =>
  implicit lazy val spec = SpecContext(suite)(new TestingIRContext)
  import spec._

  case class AssetsAtomicExchange(
      pkA: SigmaProp, pkB: SigmaProp,
      deadline: Int, tokenId: Coll[Byte]
  )(implicit val specContext: SpecContext) extends ContractSpec {
    import syntax._
    val env = Env("pkA" -> pkA, "pkB" -> pkB, "deadline" -> deadline, "tokenId" -> tokenId)

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
  }

  property("atomic exchange spec") {

    val tokenBuyer = ProvingParty("Alice")
    val tokenSeller = ProvingParty("Bob")
    val verifier = VerifyingParty("Miner")

    val contract = AssetsAtomicExchange(tokenBuyer.pubKey, tokenSeller.pubKey, 70, Blake2b256("token1"))


    // setup block, tx, and output boxes which we will spend
    val creatingTx = block(0).transaction()
    val out0 = creatingTx
        .outBox(100, contract.buyerProp)
    val out1 = creatingTx
        .outBox(1, contract.sellerProp)
        .withTokens(contract.tokenId -> 60)

    // setup spending transaction
    val spendingTx = block(50).transaction().spending(out0, out1)
    spendingTx.outBox(1, contract.buyerSignature)
        .withTokens(contract.tokenId -> 60)
        .withRegs(R4 -> out0.id)
    spendingTx.outBox(100, contract.sellerSignature)
        .withRegs(R4 -> out1.id)

    val input0 = spendingTx.inputs(0)
    val res = input0.runDsl()
    res shouldBe CostingSigmaProp(TrivialProp.TrueProp)

    val buyerProof = tokenBuyer.prove(input0).get
    verifier.verify(input0, buyerProof) shouldBe true
  }
}
