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

  property("PropLeaf EQ/NEQ") {
    val prover1 = new UtxoProvingInterpreter
    val prover2 = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val h1 = prover1.secrets.head.publicImage.h
    val h2 = prover2.secrets.head.publicImage.h

    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = SigmaStateBox(0, TrueConstantTree) -> 0)


    verifier.reduceToCrypto(EQ(PropLeaf(DLogNode(h1)), PropLeaf(DLogNode(h1))), ctx)
      .get.isInstanceOf[TrueConstantTree.type] shouldBe true

    verifier.reduceToCrypto(EQ(PropLeaf(DLogNode(h1)), PropLeaf(DLogNode(h2))), ctx)
      .get.isInstanceOf[FalseConstantTree.type] shouldBe true

  }

  ignore("TxHasOutput reductions") {}

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
    val proofP = projectProver.prove(crowdFundingScript, ctx1, challenge).get
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
    val proofB = backerProver.prove(crowdFundingScript, ctx3, challenge).get
    verifier.evaluate(crowdFundingScript, ctx3, proofB, challenge).get shouldBe true
  }


  /**
    * Demurrage currency example.
    *
    * The idea is that miners enforce users to combine a guarding script of a user ("regular_script") with a condition
    * that anyone (presumably, a miner) can spend no more "demurrage_cost" amount of tokens from an output of the user
    * after "demurrage_period" blocks since output creation. If the user is relocating the money from the output before
    * that height, the miner can charge according to output lifetime.
    *
    * (regular_script) ∨
    *   (height > (out.height + demurrage_period ) ∧ has_output(value >= out.value − demurrage_cost, script = out.script))
    */
  property("Evaluation - Demurrage Example") {
    val demurragePeriod = 100
    val demurrageCost = 2

    //a blockchain node veryfing a block containing a spending transaction
    val verifier = new UtxoInterpreter

    //backer's prover with his private key
    val userProver = new UtxoProvingInterpreter

    val regScript = DLogNode(userProver.secrets.head.publicImage.h)

    val script = OR(
      regScript,
      AND(
        GE(Height, Plus(SelfHeight, IntLeaf(demurragePeriod))),
        TxHasOutput(GE(OutputAmount, Minus(SelfAmount, IntLeaf(demurrageCost))), EQ(OutputScript, SelfScript))
      )
    )

    val outHeight = 100
    val outValue = 10

    val outputToSpend = SigmaStateBox(outValue, script)


    val challenge = Blake2b256("Hello World") //normally challenge to be defined by spending transaction bytes

    //case 1: demurrage time hasn't come yet
    val tx1 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue, script)))

    val ctx1 = UtxoContext(
      currentHeight = outHeight + demurragePeriod - 1,
      spendingTransaction = tx1,
      self = outputToSpend -> outHeight)

    //user can spend all the money
    val uProof1 = userProver.prove(script, ctx1, challenge).get
    verifier.evaluate(script, ctx1, uProof1, challenge).get shouldBe true

    //miner can't spend any money
    verifier.evaluate(script, ctx1, NoProof, challenge).get shouldBe false

    //case 2: demurrage time has come
    val ctx2 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx1,
      self = outputToSpend -> outHeight)

    //user can spend all the money
    val uProof2 = userProver.prove(script, ctx1, challenge).get
    verifier.evaluate(script, ctx2, uProof2, challenge).get shouldBe true

    //miner can spend "demurrageCost" tokens
    val tx3 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue - demurrageCost, script)))
    val ctx3 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx3,
      self = outputToSpend -> outHeight)

    verifier.evaluate(script, ctx3, NoProof, challenge).get shouldBe true

    //miner can't spend more
    val tx4 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue - demurrageCost - 1, script)))
    val ctx4 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx4,
      self = outputToSpend -> outHeight)

    verifier.evaluate(script, ctx4, NoProof, challenge).get shouldBe false


    //miner can spend less
    val tx5 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue - demurrageCost + 1, script)))
    val ctx5 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx5,
      self = outputToSpend -> outHeight)

    verifier.evaluate(script, ctx5, NoProof, challenge).get shouldBe true
  }
}
