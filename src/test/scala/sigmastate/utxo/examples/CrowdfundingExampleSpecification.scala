package sigmastate.utxo.examples

import org.ergoplatform._
import sigmastate.Values.{ByteArrayConstant, LongConstant, TaggedBox, SigmaPropConstant}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.interpreter.Interpreter._
import sigmastate.utxo._

class CrowdfundingExampleSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  /**
    * Crowdfunding example:
    * a project declares a need to raise "minToRaise" amount of tokens until some "timeout" height
    * a backer then creates an output which is spendable by with project's public key until timeout and only if a spending
    * transaction creates an output to project's public key with amount >= minToRaise
    * after the timeout output could be spent by backer only
    */
  property("Evaluation - Crowdfunding Example") {

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter

    //backer's prover with his private key
    val backerProver = new ErgoLikeTestProvingInterpreter

    //project's prover with his private key
    val projectProver = new ErgoLikeTestProvingInterpreter

    val backerPubKey = backerProver.dlogSecrets.head.publicImage
    val projectPubKey = projectProver.dlogSecrets.head.publicImage

    val timeout = LongConstant(100)
    val minToRaise = LongConstant(1000)

    val env = Map(
      ScriptNameProp -> "CrowdFunding",
      "timeout" -> 100,
      "minToRaise" -> 1000,
      "backerPubKey" -> backerPubKey,
      "projectPubKey" -> projectPubKey
    )
    val compiledScript = compile(env,
      """{
        | val c1 = HEIGHT >= timeout && backerPubKey
        | val c2 = allOf(Array(
        |   HEIGHT < timeout,
        |   projectPubKey,
        |   OUTPUTS.exists({ (out: Box) =>
        |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
        |   })
        | ))
        | c1 || c2
        | }
      """.stripMargin).asBoolValue

    val crowdFundingScript = BinOr(
      BinAnd(GE(Height, timeout), SigmaPropConstant(backerPubKey).isValid),
      AND(
        Seq(
          LT(Height, timeout),
          SigmaPropConstant(projectPubKey).isValid,
          Exists(Outputs, 21,
            BinAnd(
              GE(ExtractAmount(TaggedBox(21)), minToRaise),
              EQ(ExtractScriptBytes(TaggedBox(21)), SigmaPropConstant(projectPubKey).propBytes)
            )
          )
        )
      )
    )
    compiledScript shouldBe crowdFundingScript

    // Try a version of the script that matches the white paper -- should give the same result
    val altEnv = Map(
      "deadline" -> 100,
      "minToRaise" -> 1000,
      "backerPubKey" -> backerPubKey,
      "projectPubKey" -> projectPubKey
    )

    val altScript = compile(altEnv,
      """
        |       {
        |                val fundraisingFailure = HEIGHT >= deadline && backerPubKey
        |                val enoughRaised = {(outBox: Box) =>
        |                        outBox.value >= minToRaise &&
        |                        outBox.propositionBytes == projectPubKey.propBytes
        |                }
        |                val fundraisingSuccess = HEIGHT < deadline &&
        |                         projectPubKey &&
        |                         OUTPUTS.exists(enoughRaised)
        |
        |                fundraisingFailure || fundraisingSuccess
        |        }
      """.stripMargin).asBoolValue

//    altScript shouldBe compiledScript

    val outputToSpend = ErgoBox(10, compiledScript)

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold

    val tx1Output1 = ErgoBox(minToRaise.value, projectPubKey)
    val tx1Output2 = ErgoBox(1, projectPubKey)

    //normally this transaction would invalid, but we're not checking it in this test
    val tx1 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))

    val ctx1 = ErgoLikeContext(
      currentHeight = timeout.value - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = outputToSpend)

    //project is generating a proof and it is passing verification
    val proofP = projectProver.prove(env, compiledScript, ctx1, fakeMessage).get.proof
    verifier.verify(env, compiledScript, ctx1, proofP, fakeMessage).get._1 shouldBe true

    //backer can't generate a proof
    backerProver.prove(compiledScript, ctx1, fakeMessage).isFailure shouldBe true


    //Second case: height < timeout, project is NOT able to claim amount of tokens not less than required threshold

    val tx2Output1 = ErgoBox(minToRaise.value - 1, projectPubKey)
    val tx2Output2 = ErgoBox(1, projectPubKey)
    val tx2 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx2Output1, tx2Output2))

    val ctx2 = ErgoLikeContext(
      currentHeight = timeout.value - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx2,
      self = outputToSpend)

    //project cant' generate a proof
    val proofP2Try = projectProver.prove(compiledScript, ctx2, fakeMessage)
    proofP2Try.isSuccess shouldBe false

    //backer can't generate a proof
    val proofB2Try = backerProver.prove(compiledScript, ctx2, fakeMessage)
    proofB2Try.isSuccess shouldBe false

    //Third case: height >= timeout

    //project raised enough money but too late...
    val tx3Output1 = ErgoBox(minToRaise.value + 1, projectPubKey)
    val tx3Output2 = ErgoBox(1, projectPubKey)
    val tx3 = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx3Output1, tx3Output2))

    val ctx3 = ErgoLikeContext(
      currentHeight = timeout.value,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx3,
      self = outputToSpend)

    //project cant' generate a proof
    projectProver.prove(compiledScript, ctx3, fakeMessage).isFailure shouldBe true

    //backer is generating a proof and it is passing verification
    val proofB = backerProver.prove(compiledScript, ctx3, fakeMessage).get.proof
    verifier.verify(env, compiledScript, ctx3, proofB, fakeMessage).get._1 shouldBe true
  }


}
