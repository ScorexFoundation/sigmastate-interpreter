package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.rework.DLogProtocol.DLogProverInput
import scorex.crypto.hash.Blake2b256
import sigmastate._

class UtxoBackerProvingInterpreter extends UtxoInterpreter with DLogProverInterpreter{
  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._
    Seq(DLogProverInput.random()._1)
  }
}

class UtxoProjectProvingInterpreter extends UtxoInterpreter with DLogProverInterpreter{
  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._
    Seq(DLogProverInput.random()._1)
  }
}


class UtxoInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("Evaluation - Crowdfunding Example") {
    // (height >= 100 /\ dlog_g backerKey) \/ (height < 100 /\ dlog_g projKey /\ has_output(amount >= 100000, proposition = dlog_g projKey)

    val verifier = new UtxoInterpreter

    val backerProver = new UtxoBackerProvingInterpreter
    val projectProver = new UtxoProjectProvingInterpreter

    val backerPubKey = backerProver.secrets.head.publicImage.h
    val projectPubKey = projectProver.secrets.head.publicImage.h

    val timeout = IntLeaf(100)
    val minToRaise = IntLeaf(1000)

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
    val newOutput1 = SigmaStateBox(minToRaise.value, DLogNode(projectPubKey))
    val newOutput2 = SigmaStateBox(1, DLogNode(projectPubKey))
    val tx = SigmaStateTransaction(Seq(), Seq(newOutput1, newOutput2))

    val ctx1 = UtxoContext(currentHeight = timeout.value - 1, spendingTransaction = tx, self = outputToSpend)

    //project is generating a proof and it passing verification
    val challenge = Blake2b256("Hello World")
    val proofP = projectProver.prove(crowdFundingScript, ctx1, challenge).get.asInstanceOf[verifier.ProofT]
    verifier.evaluate(crowdFundingScript, ctx1, proofP, challenge).get shouldBe true

    //backer can't generate the proof
    val proofBTry = backerProver.prove(crowdFundingScript, ctx1, challenge)
    proofBTry.isSuccess  shouldBe false
  }

  ignore("Evaluation - Demurrage Example") {

  }
}
