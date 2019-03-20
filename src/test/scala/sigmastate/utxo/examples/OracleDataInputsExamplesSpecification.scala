package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import scorex.crypto.hash.Blake2b256
import sigmastate.helpers.SigmaTestingCommons
import special.collection.Coll
import special.sigma.Context

class OracleDataInputsExamplesSpecification extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)

  private lazy val tokenId: Coll[Byte] = spec.Coll(Blake2b256("token1"))

  case class OracleContract[Spec <: ContractSpec]
      ( temperature: Long,
        tokenID: Coll[Byte],
        alice: Spec#ProvingParty, bob: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    import syntax._
    def pkA = alice.pubKey
    def pkB = bob.pubKey
    def inRegId = reg1.asIndex

    lazy val contractEnv = Env("pkA" -> pkA, "pkB" -> pkB, "tokenId" -> tokenId)

    lazy val prop = proposition("dataInputContract", { CONTEXT: Context =>
      import CONTEXT._
      val dataInput = CONTEXT.dataInputs(0)
      val inReg = dataInput.R4[Long].get
      val inToken = dataInput.R2[Coll[(Coll[Byte], Long)]].get(0)._1 == tokenId
      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
      inToken && okContractLogic
    },
      """{
        |      val dataInput = CONTEXT.dataInputs(0)
        |      val inReg = dataInput.R4[Long].get
        |      val inToken = dataInput.R2[Coll[(Coll[Byte], Long)]].get(0)._1 == tokenId
        |      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
        |      inToken && okContractLogic
        |}
      """.stripMargin)

    lazy val aliceSignature  = proposition("aliceSignature", _ => pkA, "pkA")

    lazy val dummySignature  = proposition("dummySignature", _ => pkA, "pkA")
  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  lazy val alice = spec.ProvingParty("Alice")
  lazy val bob = spec.ProvingParty("Bob")

  property("lightweight oracle token example (ErgoDsl)") {
    val temperature: Long = 18
    val contract = OracleContract[spec.type](temperature, tokenId, alice, bob)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = candidateBlock(0).newTransaction()

    val dBox = mockTx.outBox(1, contract.prop).withRegs(reg1 -> temperature)

    val sOracle = mockTx // in real world, this must be protected by pkOracle
        .outBox(value = 1L, contract.dummySignature)
        .withRegs(reg1 -> temperature)
        .withTokens(Token(tokenId, 1))

    val sAlice = mockTx.outBox(10, contract.prop)
    val sBob   = mockTx.outBox(10, contract.prop)

    val tx = candidateBlock(50).newTransaction().withDataInputs(sOracle).spending(sAlice, sBob)
    tx.outBox(20, contract.aliceSignature)
    val in = tx.inputs(1)
    val res = in.runDsl()
    res shouldBe alice.pubKey

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
