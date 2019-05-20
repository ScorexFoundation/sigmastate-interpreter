package sigmastate.utxo.examples

import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import sigmastate.eval.Extensions
import sigmastate.helpers.SigmaTestingCommons
import special.collection.Coll
import special.sigma.{Box, Context}

class RevenueSharingExamplesSpecification extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  case class RevenueContract[Spec <: ContractSpec]
  (alice: Spec#ProvingParty, bob: Spec#ProvingParty, carol:Spec#ProvingParty)
  (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    val spenders = Coll(
      (blake2b256(alice.pubKey.propBytes), 50),
      (blake2b256(bob.pubKey.propBytes), 30),
      (blake2b256(carol.pubKey.propBytes), 20)
    )

    val miner = alice  // put some other entity here
    val feeProp = miner.pubKey
    val fee = 10
    val feePropBytesHash = blake2b256(feeProp.propBytes)

    lazy val contractEnv = Env(
      "spenders" -> spenders,
      "feePropBytesHash" -> feePropBytesHash,
      "fee" -> fee,
      "feeProp" -> feeProp,
      "requireAliceSignature" -> alice.pubKey,
      "requireBobSignature" -> bob.pubKey,
      "requireCarolSignature" -> carol.pubKey,
    )
    lazy val prop = proposition("revenueContract", { CONTEXT: Context =>
      import CONTEXT._

      val feeBox = OUTPUTS(0)
      val validFeeBox = blake2b256(feeBox.propositionBytes) == feePropBytesHash
      //validFeeBox
      miner.pubKey // dummy line because above doesn't work
    },
    """{
      |      val feeBox = OUTPUTS(0)
      |      val validFeeBox = blake2b256(feeBox.propositionBytes) == feePropBytesHash
      |      val amt = SELF.value - fee
      |      val ratios = spenders.map({(e:(Coll[Byte], Int)) => e._2})
      |      val total = ratios.fold(0, {(l:Int, r:Int) => l + r})
      |      val validOuts = spenders.zip(OUTPUTS).forall({
      |        (e:((Coll[Byte], Int), Box)) =>
      |           val ratio = e._1._2
      |           val pubKeyHash = e._1._1
      |           val box = e._2
      |           val share = amt / total * ratio
      |           box.value >= share && blake2b256(box.propositionBytes) == pubKeyHash
      |      })
      |      validOuts && validFeeBox
      |}
    """.stripMargin)

    lazy val requireAliceSignature =  proposition(
      "requireAliceSignature",
      _ => alice.pubKey,
      "requireAliceSignature"
    )
    lazy val requireBobSignature =  proposition(
      "requireBobSignature",
      _ => bob.pubKey,
      "requireBobSignature"
    )
    lazy val requireCarolSignature =  proposition(
      "requireCarolSignature",
      _ => carol.pubKey,
      "requireCarolSignature"
    )
    lazy val requireMinerSignature =  proposition(
      "feeProp", _ => miner.pubKey, "feeProp"
    )

  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  lazy val alice = spec.ProvingParty("Alice")
  lazy val bob = spec.ProvingParty("Bob")
  lazy val carol = spec.ProvingParty("Carol")

  property("Revenue sharing contract") {
    val contract = RevenueContract[spec.type](alice, bob, carol)(spec)

    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()

    val deposit = mockTx.outBox(110, contract.prop)

    val tx = candidateBlock(10).newTransaction().spending(deposit)

    tx.outBox(50, contract.requireAliceSignature)
    tx.outBox(30, contract.requireBobSignature)
    tx.outBox(20, contract.requireCarolSignature)
    tx.outBox(10, contract.requireMinerSignature)

    val in = tx.inputs(0)

    val res = in.runDsl(Map(1.toByte -> Extensions.toAnyValue(1)))

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
