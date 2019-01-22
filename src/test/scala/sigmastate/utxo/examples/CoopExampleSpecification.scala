package sigmastate.utxo.examples

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import org.scalatest.Assertion
import org.scalatest.TryValues._
import sigmastate.basics.DLogProtocol.ProveDlog
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, Value}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter
import sigmastate.{AvlTreeData, SBoolean}

class CoopExampleSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  
  def mkTxFromOutputs(ergoBox: ErgoBox*): ErgoLikeTransaction = {
    ErgoLikeTransaction(IndexedSeq(), ergoBox.toIndexedSeq)
  }

  def mkCtx(height: Int,
            tx: ErgoLikeTransaction,
            self: ErgoBox): ErgoLikeContext = {
    ErgoLikeContext(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(self),
      spendingTransaction = tx,
      self = self)
  }

  def successProofTest(exp: Value[SBoolean.type],
                       ctx: ErgoLikeContext,
                       prover: ErgoLikeTestProvingInterpreter,
                       verifier: ErgoLikeTestInterpreter): Assertion = {
    val proofResult = prover.prove(exp, ctx, fakeMessage)
    proofResult should be a 'success
    verifier.verify(exp, ctx, proofResult.success.value, fakeMessage) should be a 'success
  }

  def failingProofTest(exp: Value[SBoolean.type],
                       ctx: ErgoLikeContext,
                       prover: ErgoLikeTestProvingInterpreter): Assertion = {
    prover.prove(exp, ctx, fakeMessage) should be a 'failure
  }

  property("commit to the threshold sig") {

    val coopA = new ErgoLikeTestProvingInterpreter
    val coopB = new ErgoLikeTestProvingInterpreter
    val coopC = new ErgoLikeTestProvingInterpreter
    val coopD = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val totalValue = 10000L
    val toolValue = 5000L
    val constructionValue = 2000L

    val skA = coopA.dlogSecrets.head
    val skB = coopB.dlogSecrets.head
    val skC = coopC.dlogSecrets.head
    val skD = coopD.dlogSecrets.head

    val pubkeyA = skA.publicImage
    val pubkeyB = skB.publicImage
    val pubkeyC = skC.publicImage
    val pubkeyD = skD.publicImage

    val toolA = new ErgoLikeTestProvingInterpreter
    val toolB = new ErgoLikeTestProvingInterpreter
    val toolC = new ErgoLikeTestProvingInterpreter
    val toolD = new ErgoLikeTestProvingInterpreter


    val toolRing = Seq(
      toolA.dlogSecrets.head.publicImage,
      toolB.dlogSecrets.head.publicImage,
      toolC.dlogSecrets.head.publicImage,
      toolD.dlogSecrets.head.publicImage)

    val constrA = new ErgoLikeTestProvingInterpreter
    val constrB = new ErgoLikeTestProvingInterpreter
    val constrC = new ErgoLikeTestProvingInterpreter

    val constructionRing = Seq(
      constrA.dlogSecrets.head.publicImage,
      constrB.dlogSecrets.head.publicImage,
      constrC.dlogSecrets.head.publicImage)

    val business = new ErgoLikeTestProvingInterpreter
    val businessKey = business.dlogSecrets.head.publicImage

    def withdraw(minHeight: Long, totalValue: Long) = {
      s"""
         |val withdrawCondition = HEIGHT > ${minHeight}L &&
         |        OUTPUTS(0).value >= ${totalValue / 4}L && OUTPUTS(0).propositionBytes == pubkeyA.propBytes &&
         |        OUTPUTS(1).value >= ${totalValue / 4}L && OUTPUTS(1).propositionBytes == pubkeyB.propBytes &&
         |        OUTPUTS(2).value >= ${totalValue / 4}L && OUTPUTS(2).propositionBytes == pubkeyC.propBytes &&
         |        OUTPUTS(3).value >= ${totalValue / 4}L && OUTPUTS(3).propositionBytes == pubkeyD.propBytes
       """.stripMargin
    }

    val spendingEnv = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "businessKey" -> businessKey,
      "pubkeyTool1" -> (toolRing(0) : ProveDlog),
      "pubkeyTool2" -> (toolRing(1) : ProveDlog),
      "pubkeyTool3" -> (toolRing(2) : ProveDlog),
      "pubkeyTool4" -> (toolRing(3) : ProveDlog),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> constructionRing(2)
    )

     val spendingProp1 = compileWithCosting(spendingEnv,
      s"""
         |{
         | val spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && businessKey
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
         |}
       """.stripMargin).asBoolValue

    {
      val self = ErgoBox(totalValue, spendingProp1, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output4 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5001, tx, self)
      successProofTest(spendingProp1, ctx, coopA, verifier)
    }

    val spendingEnv2 = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "businessKey" -> businessKey,
      "pubkeyTool1" -> SBoolean.mkConstant(false),
      "pubkeyTool2" -> SBoolean.mkConstant(false),
      "pubkeyTool3" -> SBoolean.mkConstant(false),
      "pubkeyTool4" -> SBoolean.mkConstant(false),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> constructionRing(2)
    )

    val spendingProp2 = compileWithCosting(spendingEnv2,
      s"""
         |{
         | val spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && businessKey
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
         |}
       """.stripMargin).asBoolValue

    /**
      * Withdraw successfully
      */
    {
      val self = ErgoBox(totalValue, spendingProp2, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output4 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5001, tx, self)
      successProofTest(spendingProp2, ctx, coopA, verifier)
    }

    /**
      * Won't spend more then defined share
      */
    {
      val self = ErgoBox(totalValue, spendingProp2, 0)
      val output1 = ErgoBox(totalValue / 2, pubkeyB, 0)
      val output2 = ErgoBox(totalValue / 2, pubkeyC, 0)
      val tx = mkTxFromOutputs(output1, output2)
      val ctx = mkCtx(5001, tx, self = self)
      failingProofTest(spendingProp2, ctx, coopA)
    }

    /**
      * Won't spend before minimal height
      */
    {
      val self = ErgoBox(totalValue, spendingProp2, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output4 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5000, tx, self)
      failingProofTest(spendingProp2, ctx, coopA)
    }


    val spendingProp3 = compileWithCosting(spendingEnv,
      s"""
         | {
         |
         |  val spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && businessKey
         |
         |  ${withdraw(5000, constructionValue)}
         |
         |  spendingSuccess || withdrawCondition
         | }
       """.stripMargin).asBoolValue

    /**
      * Will spend correctly if all the conditions are satisfied
      */
    {
      val self = ErgoBox(totalValue, spendingProp3, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output4 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5001, tx, self)
      successProofTest(spendingProp2, ctx, coopA, verifier)
    }

    val spendingEnv3 = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "businessKey" -> businessKey,
      "pubkeyTool1" -> SBoolean.mkConstant(false),
      "pubkeyTool2" -> SBoolean.mkConstant(false),
      "pubkeyTool3" -> SBoolean.mkConstant(false),
      "pubkeyTool4" -> SBoolean.mkConstant(false),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> SBoolean.mkConstant(false)
    )

    val spendingProp4 = compileWithCosting(spendingEnv3,
      s"""
         | {
         |
         |  val spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && businessKey
         |
         |  ${withdraw(5000, constructionValue)}
         |
         |  spendingSuccess || withdrawCondition
         | }
       """.stripMargin).asBoolValue

    {
      val self = ErgoBox(totalValue, spendingProp4, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output4 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5001, tx, self)
      successProofTest(spendingProp4, ctx, coopA, verifier)
    }

    val spendingProp5 = compileWithCosting(spendingEnv, "businessKey").asBoolValue

    {
      val self = ErgoBox(totalValue, spendingProp5, 0)
      val output = ErgoBox(totalValue, businessKey, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1, tx, self)
      failingProofTest(spendingProp5, ctx, coopA)
      failingProofTest(spendingProp5, ctx, coopB)
      failingProofTest(spendingProp5, ctx, coopC)
      failingProofTest(spendingProp5, ctx, coopD)
      successProofTest(spendingProp5, ctx, business, verifier)
    }

    val thresholdEnv = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "spendingContract1Hash" -> ByteArrayConstant(Blake2b256(spendingProp1.bytes)),
      "spendingContract2Hash" -> ByteArrayConstant(Blake2b256(spendingProp3.bytes))
    )

    // Basic compilation
    val thresholdProp = compileWithCosting(thresholdEnv,
      s""" {
         | val votingSuccess = atLeast(3, Coll(pubkeyA, pubkeyB, pubkeyC, pubkeyD))
         | val properSpending = OUTPUTS(0).value >= ${toolValue}L &&
         |                      blake2b256(OUTPUTS(0).propositionBytes) == spendingContract1Hash &&
         |                      OUTPUTS(1).value >= ${constructionValue}L &&
         |                      blake2b256(OUTPUTS(1).propositionBytes) == spendingContract2Hash
         |
         | ${withdraw(1000, totalValue)}
         |
         | (votingSuccess && properSpending) || withdrawCondition
         | }
      """.stripMargin).asBoolValue


    /**
      * Check votingSuccess && properSpending case
      */
    {
      val self = ErgoBox(totalValue + 1L, thresholdProp, 0)
      val output1 = ErgoBox(toolValue, spendingProp1, 0)
      val output2 = ErgoBox(constructionValue, spendingProp3, 0)
      val output3 = ErgoBox(totalValue - toolValue - constructionValue, spendingProp5, 0)
      //hack for avoiding None.get exception.
      val dummy = ErgoBox(0L, SBoolean.mkConstant(true), 0)
      val tx = mkTxFromOutputs(output1, output2, output3, dummy)
      val ctx = mkCtx(2000, tx, self)

      failingProofTest(thresholdProp, ctx, business)
      failingProofTest(thresholdProp, ctx, business.withSecrets(Seq(skA)))
      failingProofTest(thresholdProp, ctx, business.withSecrets(Seq(skA, skB)))
      successProofTest(thresholdProp, ctx, business.withSecrets(Seq(skA, skB, skC)), verifier)
      successProofTest(thresholdProp, ctx, business.withSecrets(Seq(skA, skB, skC, skD)), verifier)
    }

    /**
      * Check withdraw success
      */
    {
      val self = ErgoBox(totalValue + 1L, thresholdProp, 0)
      val output0 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output0, output1, output2, output3)
      val ctx = mkCtx(2000, tx, self)
      successProofTest(thresholdProp, ctx, business, verifier)
    }

    /**
      * Check withdraw failure. Not enough height case.
      */
    {
      val self = ErgoBox(totalValue + 1L, thresholdProp, 0)
      val output0 = ErgoBox(totalValue / 4, pubkeyA, 0)
      val output1 = ErgoBox(totalValue / 4, pubkeyB, 0)
      val output2 = ErgoBox(totalValue / 4, pubkeyC, 0)
      val output3 = ErgoBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output0, output1, output2, output3)
      val ctx = mkCtx(1000, tx, self)
      failingProofTest(thresholdProp, ctx, business)
    }


    val inputEnv = Map(
      "thresholdProp" -> ByteArrayConstant(Blake2b256(thresholdProp.bytes)),
      "pubkeyA" -> pubkeyA
    )

    val inputProp = compileWithCosting(inputEnv,
      s"""(OUTPUTS(0).value == $totalValue && blake2b256(OUTPUTS(0).propositionBytes) == thresholdProp) ||
         | (HEIGHT > 1000 && pubkeyA)
       """.stripMargin).asBoolValue

    /**
      * height not higher, total value is equal
      */
    {
      val self = ErgoBox(totalValue, inputProp, 0)
      val output = ErgoBox(totalValue, thresholdProp, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1000, tx, self)
      successProofTest(inputProp, ctx, coopA, verifier)
    }

    /**
      * total value is lower, height is higher
      */
    {
      val self = ErgoBox(totalValue - 1L, inputProp, 0)
      val output = ErgoBox(totalValue - 1L, thresholdProp, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1001, tx, self)
      successProofTest(inputProp, ctx, coopA, verifier)
    }

    /**
      * negative condition
      */
    {
      val self = ErgoBox(totalValue - 1L, inputProp, 0)
      val output = ErgoBox(totalValue - 1L, thresholdProp, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1000, tx, self)
      failingProofTest(inputProp, ctx, coopA)
    }
  }
}
