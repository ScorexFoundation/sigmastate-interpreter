package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.rework.DLogProtocol.{DLogNode, DLogProverInput}
import scorex.crypto.hash.Blake2b256
import sigmastate._

class UtxoProvingInterpreter extends UtxoInterpreter with DLogProverInterpreter {
  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._
    Seq(DLogProverInput.random()._1)
  }
}

class UtxoInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  /**
    * Crowdfunding example:
    * a project declares a need to raise "minToRaise" amount of tokens until some "timeout" height
    * a backer then creates an output which is spendable by with project's public key until timeout and only if a spending
    * transaction creates an output to project's public key with amount >= minToRaise
    * after the timeout output could be spent by backer only
    */
  property("Evaluation - Crowdfunding Example") {

    //a blockchain node veryfing a block containing a spending transaction
    val verifier = new UtxoInterpreter

    //backer's prover with his private key
    val backerProver = new UtxoProvingInterpreter

    //project's prover with his private key
    val projectProver = new UtxoProvingInterpreter

    val backerPubKey = backerProver.secrets.head.publicImage.h
    val projectPubKey = projectProver.secrets.head.publicImage.h

    val timeout = IntLeaf(100)
    val minToRaise = IntLeaf(1000)

    // (height >= timeout /\ dlog_g backerKey) \/ (height < timeout /\ dlog_g projKey /\ has_output(amount >= minToRaise, proposition = dlog_g projKey)
    val crowdFundingScript = OR(
      AND(GE(Height, timeout), DLogNode(backerPubKey)),
      AND(
        Seq(
          LT(Height, timeout),
          DLogNode(projectPubKey),
          TxHasOutput(GE(OutputAmount, minToRaise), EQ(OutputScript, PropLeaf(DLogNode(projectPubKey))))
        )
      )
    )

    val outputToSpend = SigmaStateBox(10, crowdFundingScript)
    val challenge = Blake2b256("Hello World") //normally challenge to be defined by spending transaction bytes

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold

    val tx1Output1 = SigmaStateBox(minToRaise.value, DLogNode(projectPubKey))
    val tx1Output2 = SigmaStateBox(1, DLogNode(projectPubKey))

    //normally this transaction would invalid, but we're not checking it in this test
    val tx1 = SigmaStateTransaction(Seq(), Seq(tx1Output1, tx1Output2))

    val ctx1 = UtxoContext(currentHeight = timeout.value - 1, spendingTransaction = tx1, self = outputToSpend -> 0)

    //project is generating a proof and it is passing verification
    val proofP = projectProver.prove(crowdFundingScript, ctx1, challenge).get.asInstanceOf[verifier.ProofT]
    verifier.evaluate(crowdFundingScript, ctx1, proofP, challenge).get shouldBe true

    //backer can't generate a proof
    val proofBTry = backerProver.prove(crowdFundingScript, ctx1, challenge)
    proofBTry.isSuccess shouldBe false


    //Second case: height < timeout, project is NOT able to claim amount of tokens not less than required threshold

    val tx2Output1 = SigmaStateBox(minToRaise.value - 1, DLogNode(projectPubKey))
    val tx2Output2 = SigmaStateBox(1, DLogNode(projectPubKey))
    val tx2 = SigmaStateTransaction(Seq(), Seq(tx2Output1, tx2Output2))

    val ctx2 = UtxoContext(currentHeight = timeout.value - 1, spendingTransaction = tx2, self = outputToSpend -> 0)

    //project cant' generate a proof
    val proofP2Try = projectProver.prove(crowdFundingScript, ctx2, challenge)
    proofP2Try.isSuccess shouldBe false

    //backer can't generate a proof
    val proofB2Try = backerProver.prove(crowdFundingScript, ctx2, challenge)
    proofB2Try.isSuccess shouldBe false

    //Third case: height >= timeout

    //project raised enough money but too late...
    val tx3Output1 = SigmaStateBox(minToRaise.value + 1, DLogNode(projectPubKey))
    val tx3Output2 = SigmaStateBox(1, DLogNode(projectPubKey))
    val tx3 = SigmaStateTransaction(Seq(), Seq(tx3Output1, tx3Output2))

    val ctx3 = UtxoContext(currentHeight = timeout.value, spendingTransaction = tx3, self = outputToSpend -> 0)

    //project cant' generate a proof
    val proofP3Try = projectProver.prove(crowdFundingScript, ctx3, challenge)
    proofP3Try.isSuccess shouldBe false

    //backer is generating a proof and it is passing verification
    val proofB = backerProver.prove(crowdFundingScript, ctx3, challenge).get.asInstanceOf[verifier.ProofT]
    verifier.evaluate(crowdFundingScript, ctx3, proofB, challenge).get shouldBe true
  }

  property("Evaluation - Demurrage Example") {
    val DemurragePeriod = 100
    val demurrageCost = 2

    val prover = new UtxoProvingInterpreter
    val regScript = DLogNode(prover.secrets.head.publicImage.h)

    //val script = OR(regScript, AND()
  }
}
