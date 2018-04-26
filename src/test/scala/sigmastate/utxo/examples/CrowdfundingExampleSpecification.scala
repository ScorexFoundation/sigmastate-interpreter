package sigmastate.utxo.examples

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import sigmastate.Values.{ByteArrayConstant, IntConstant, TaggedBox}
import sigmastate._
import sigmastate.utxo._

class CrowdfundingExampleSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  import BoxHelpers.fakeMessage

  /**
    * Crowdfunding example:
    * a project declares a need to raise "minToRaise" amount of tokens until some "timeout" height
    * a backer then creates an output which is spendable by with project's public key until timeout and only if a spending
    * transaction creates an output to project's public key with amount >= minToRaise
    * after the timeout output could be spent by backer only
    */
  property("Evaluation - Crowdfunding Example") {

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoInterpreter

    //backer's prover with his private key
    val backerProver = new ErgoProvingInterpreter

    //project's prover with his private key
    val projectProver = new ErgoProvingInterpreter

    val backerPubKey = backerProver.dlogSecrets.head.publicImage
    val projectPubKey = projectProver.dlogSecrets.head.publicImage

    val timeout = IntConstant(100)
    val minToRaise = IntConstant(1000)

    val env = Map(
      "timeout" -> 100,
      "minToRaise" -> 1000,
      "backerPubKey" -> backerPubKey,
      "projectPubKey" -> projectPubKey,
    )
    val compiledScript = compile(env,
      """{
        | let c1 = HEIGHT >= timeout && backerPubKey
        | let c2 = allOf(Array(
        |   HEIGHT < timeout,
        |   projectPubKey,
        |   OUTPUTS.exists(fun (out: Box) = {
        |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
        |   })
        | ))
        | c1 || c2
        | }
      """.stripMargin)

    //    // (height >= timeout /\ dlog_g backerKey) \/ (height < timeout /\ dlog_g projKey /\ has_output(amount >= minToRaise, proposition = dlog_g projKey)
    val crowdFundingScript = OR(
      AND(GE(Height, timeout), backerPubKey),
      AND(
        Seq(
          LT(Height, timeout),
          projectPubKey,
          Exists(Outputs, 21,
            AND(
              GE(ExtractAmount(TaggedBox(21)), minToRaise),
              EQ(ExtractScriptBytes(TaggedBox(21)), ByteArrayConstant(projectPubKey.propBytes))
            )
          )
        )
      )
    )
    compiledScript shouldBe crowdFundingScript

    val outputToSpend = ErgoBox(10, crowdFundingScript)

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold

    val tx1Output1 = ErgoBox(minToRaise.value, projectPubKey)
    val tx1Output2 = ErgoBox(1, projectPubKey)

    //normally this transaction would invalid, but we're not checking it in this test
    val tx1 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))

    val ctx1 = ErgoContext(
      currentHeight = timeout.value - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = outputToSpend)

    //project is generating a proof and it is passing verification
    val proofP = projectProver.prove(crowdFundingScript, ctx1, fakeMessage).get.proof
    verifier.verify(crowdFundingScript, ctx1, proofP, fakeMessage).get shouldBe true

    //backer can't generate a proof
    backerProver.prove(crowdFundingScript, ctx1, fakeMessage).isFailure shouldBe true


    //Second case: height < timeout, project is NOT able to claim amount of tokens not less than required threshold

    val tx2Output1 = ErgoBox(minToRaise.value - 1, projectPubKey)
    val tx2Output2 = ErgoBox(1, projectPubKey)
    val tx2 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx2Output1, tx2Output2))

    val ctx2 = ErgoContext(
      currentHeight = timeout.value - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx2,
      self = outputToSpend)

    //project cant' generate a proof
    val proofP2Try = projectProver.prove(crowdFundingScript, ctx2, fakeMessage)
    proofP2Try.isSuccess shouldBe false

    //backer can't generate a proof
    val proofB2Try = backerProver.prove(crowdFundingScript, ctx2, fakeMessage)
    proofB2Try.isSuccess shouldBe false

    //Third case: height >= timeout

    //project raised enough money but too late...
    val tx3Output1 = ErgoBox(minToRaise.value + 1, projectPubKey)
    val tx3Output2 = ErgoBox(1, projectPubKey)
    val tx3 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx3Output1, tx3Output2))

    val ctx3 = ErgoContext(
      currentHeight = timeout.value,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx3,
      self = outputToSpend)

    //project cant' generate a proof
    projectProver.prove(crowdFundingScript, ctx3, fakeMessage).isFailure shouldBe true

    //backer is generating a proof and it is passing verification
    val proofB = backerProver.prove(crowdFundingScript, ctx3, fakeMessage).get.proof
    verifier.verify(crowdFundingScript, ctx3, proofB, fakeMessage).get shouldBe true
  }


}
