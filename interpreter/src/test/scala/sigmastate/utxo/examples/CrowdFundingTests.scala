package sigmastate.utxo.examples

import sigmastate.helpers.SigmaTestingCommons
import org.ergoplatform.dsl.TestContractSpec

class CrowdFundingTests extends SigmaTestingCommons { suite =>
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  lazy val backer = spec.ProvingParty("Alice")
  lazy val project = spec.ProvingParty("Bob")

  property("Evaluation - Crowdfunding Example") {
    val contract = CrowdFunding[spec.type](100, 1000, backer, project)(spec)
    import contract.spec._

    val holderBox = candidateBlock(0).newTransaction()
        .outBox(10, contract.holderProp)

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold
    {
      // normally this transaction would be invalid (ill-balanced), but we're not checking it in this test
      val spendingTx = candidateBlock(contract.deadline - 1).newTransaction().spending(holderBox)
      spendingTx.outBox(contract.minToRaise, contract.projectSignature)

      // ASSERT
      val input0 = spendingTx.inputs(0)
      val res = input0.runDsl()
      res shouldBe contract.pkProject

      //project is generating a proof and it is passing verification
      val buyerProof = contract.project.prove(input0).get
      contract.verifier.verify(input0, buyerProof) shouldBe true

      //backer can't generate a proof
      contract.backer.prove(input0).isFailure shouldBe true
    }

    //Second case: height < timeout, project is NOT able to claim amount of tokens less than required threshold
    {
      val spendingTx = candidateBlock(contract.deadline - 1).newTransaction().spending(holderBox)
      spendingTx.outBox(contract.minToRaise - 1, contract.projectSignature)
      val input0 = spendingTx.inputs(0)

      //project cant' generate a proof
      val proofP2Try = contract.project.prove(input0)
      proofP2Try.isSuccess shouldBe false

      //backer can't generate a proof
      val proofB2Try = contract.backer.prove(input0)
      proofB2Try.isSuccess shouldBe false
    }

    //Third case: height >= timeout
    {
      val spendingTx = candidateBlock(contract.deadline).newTransaction().spending(holderBox)
      spendingTx.outBox(contract.minToRaise + 1, contract.projectSignature)
      val input0 = spendingTx.inputs(0)

      //project cant' generate a proof
      contract.project.prove(input0).isFailure shouldBe true

      //backer is generating a proof and it is passing verification
      val proofB = contract.backer.prove(input0).get
      contract.verifier.verify(input0, proofB) shouldBe true
    }
  }

}
