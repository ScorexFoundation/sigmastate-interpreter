package sigmastate.utxo.examples

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox, ErgoScriptPredef}
import org.scalatest.Assertion
import org.scalatest.TryValues._
import sigmastate.basics.DLogProtocol.ProveDlog
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, BooleanConstant, SigmaPropValue}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, SigmaTestingCommons, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.lang.Terms._
import sigmastate.AvlTreeData
import sigmastate.eval.IRContextFactoryImpl

class CoopExampleSpecification extends SigmaTestingCommons {
  def createIR = new TestingIRContext
  implicit lazy val IR = createIR
  implicit lazy val irFactory = new IRContextFactoryImpl(createIR)

  def mkTxFromOutputs(ergoBox: ErgoBox*): ErgoLikeTransaction = {
    createTransaction(ergoBox.toIndexedSeq)
  }

  def mkCtx(height: Int,
            tx: ErgoLikeTransaction,
            self: ErgoBox): ErgoLikeContext = {
    ErgoLikeContextTesting(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(self),
      spendingTransaction = tx,
      self = self)
  }

  def successProofTest(exp: SigmaPropValue,
                       ctx: ErgoLikeContext,
                       prover: ContextEnrichingTestProvingInterpreter,
                       verifier: ErgoLikeTestInterpreter): Assertion = {
    val proofResult = prover.prove(exp, ctx, fakeMessage)
    proofResult should be a 'success
    verifier.verify(exp, ctx, proofResult.success.value, fakeMessage) should be a 'success
  }

  def failingProofTest(exp: SigmaPropValue,
                       ctx: ErgoLikeContext,
                       prover: ContextEnrichingTestProvingInterpreter): Assertion = {
    prover.prove(exp, ctx, fakeMessage) should be a 'failure
  }

  property("commit to the threshold sig") {

    val coopA = new ContextEnrichingTestProvingInterpreter
    val coopB = new ContextEnrichingTestProvingInterpreter
    val coopC = new ContextEnrichingTestProvingInterpreter
    val coopD = new ContextEnrichingTestProvingInterpreter
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

    val toolA = new ContextEnrichingTestProvingInterpreter
    val toolB = new ContextEnrichingTestProvingInterpreter
    val toolC = new ContextEnrichingTestProvingInterpreter
    val toolD = new ContextEnrichingTestProvingInterpreter


    val toolRing = Seq(
      toolA.dlogSecrets.head.publicImage,
      toolB.dlogSecrets.head.publicImage,
      toolC.dlogSecrets.head.publicImage,
      toolD.dlogSecrets.head.publicImage)

    val constrA = new ContextEnrichingTestProvingInterpreter
    val constrB = new ContextEnrichingTestProvingInterpreter
    val constrC = new ContextEnrichingTestProvingInterpreter

    val constructionRing = Seq(
      constrA.dlogSecrets.head.publicImage,
      constrB.dlogSecrets.head.publicImage,
      constrC.dlogSecrets.head.publicImage)

    val business = new ContextEnrichingTestProvingInterpreter
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

     val spendingProp1 = compile(spendingEnv,
      s"""
         |{
         | val spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && businessKey
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
         |}
       """.stripMargin).asSigmaProp

    {
      val self = testBox(totalValue, spendingProp1, 0)
      val output1 = testBox(totalValue / 4, pubkeyA, 0)
      val output2 = testBox(totalValue / 4, pubkeyB, 0)
      val output3 = testBox(totalValue / 4, pubkeyC, 0)
      val output4 = testBox(totalValue / 4, pubkeyD, 0)
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
      "pubkeyTool1" -> BooleanConstant(false),
      "pubkeyTool2" -> BooleanConstant(false),
      "pubkeyTool3" -> BooleanConstant(false),
      "pubkeyTool4" -> BooleanConstant(false),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> constructionRing(2)
    )

    val spendingProp2 = compile(spendingEnv2,
      s"""
         |{
         | val spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && businessKey
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
         |}
       """.stripMargin).asSigmaProp

    /**
      * Withdraw successfully
      */
    {
      val self = testBox(totalValue, spendingProp2, 0)
      val output1 = testBox(totalValue / 4, pubkeyA, 0)
      val output2 = testBox(totalValue / 4, pubkeyB, 0)
      val output3 = testBox(totalValue / 4, pubkeyC, 0)
      val output4 = testBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5001, tx, self)
      successProofTest(spendingProp2, ctx, coopA, verifier)
    }

    /**
      * Won't spend more then defined share
      */
    {
      val self = testBox(totalValue, spendingProp2, 0)
      val output1 = testBox(totalValue / 2, pubkeyB, 0)
      val output2 = testBox(totalValue / 2, pubkeyC, 0)
      val tx = mkTxFromOutputs(output1, output2)
      val ctx = mkCtx(5001, tx, self = self)
      failingProofTest(spendingProp2, ctx, coopA)
    }

    /**
      * Won't spend before minimal height
      */
    {
      val self = testBox(totalValue, spendingProp2, 0)
      val output1 = testBox(totalValue / 4, pubkeyA, 0)
      val output2 = testBox(totalValue / 4, pubkeyB, 0)
      val output3 = testBox(totalValue / 4, pubkeyC, 0)
      val output4 = testBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5000, tx, self)
      failingProofTest(spendingProp2, ctx, coopA)
    }


    val spendingProp3 = compile(spendingEnv,
      s"""
         | {
         |
         |  val spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && businessKey
         |
         |  ${withdraw(5000, constructionValue)}
         |
         |  spendingSuccess || withdrawCondition
         | }
       """.stripMargin).asSigmaProp

    /**
      * Will spend correctly if all the conditions are satisfied
      */
    {
      val self = testBox(totalValue, spendingProp3, 0)
      val output1 = testBox(totalValue / 4, pubkeyA, 0)
      val output2 = testBox(totalValue / 4, pubkeyB, 0)
      val output3 = testBox(totalValue / 4, pubkeyC, 0)
      val output4 = testBox(totalValue / 4, pubkeyD, 0)
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
      "pubkeyTool1" -> BooleanConstant(false),
      "pubkeyTool2" -> BooleanConstant(false),
      "pubkeyTool3" -> BooleanConstant(false),
      "pubkeyTool4" -> BooleanConstant(false),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> BooleanConstant(false)
    )

    val spendingProp4 = compile(spendingEnv3,
      s"""
         | {
         |
         |  val spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && businessKey
         |
         |  ${withdraw(5000, constructionValue)}
         |
         |  spendingSuccess || withdrawCondition
         | }
       """.stripMargin).asSigmaProp

    {
      val self = testBox(totalValue, spendingProp4, 0)
      val output1 = testBox(totalValue / 4, pubkeyA, 0)
      val output2 = testBox(totalValue / 4, pubkeyB, 0)
      val output3 = testBox(totalValue / 4, pubkeyC, 0)
      val output4 = testBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output1, output2, output3, output4)
      val ctx = mkCtx(5001, tx, self)
      successProofTest(spendingProp4, ctx, coopA, verifier)
    }

    val spendingProp5 = compile(spendingEnv, "businessKey").asSigmaProp

    {
      val self = testBox(totalValue, spendingProp5, 0)
      val output = testBox(totalValue, businessKey, 0)
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
      "spendingContract1Hash" -> ByteArrayConstant(Blake2b256(spendingProp1.treeWithSegregation.bytes)),
      "spendingContract2Hash" -> ByteArrayConstant(Blake2b256(spendingProp3.treeWithSegregation.bytes))
    )

    // Basic compilation
    val thresholdProp = compile(thresholdEnv,
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
      """.stripMargin).asSigmaProp


    /**
      * Check votingSuccess && properSpending case
      */
    {
      val self = testBox(totalValue + 1L, thresholdProp, 0)
      val output1 = testBox(toolValue, spendingProp1, 0)
      val output2 = testBox(constructionValue, spendingProp3, 0)
      val output3 = testBox(totalValue - toolValue - constructionValue, spendingProp5, 0)
      //hack for avoiding None.get exception.
      val dummy = testBox(0L, ErgoScriptPredef.TrueProp, 0)
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
      val self = testBox(totalValue + 1L, thresholdProp, 0)
      val output0 = testBox(totalValue / 4, pubkeyA, 0)
      val output1 = testBox(totalValue / 4, pubkeyB, 0)
      val output2 = testBox(totalValue / 4, pubkeyC, 0)
      val output3 = testBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output0, output1, output2, output3)
      val ctx = mkCtx(2000, tx, self)
      successProofTest(thresholdProp, ctx, business, verifier)
    }

    /**
      * Check withdraw failure. Not enough height case.
      */
    {
      val self = testBox(totalValue + 1L, thresholdProp, 0)
      val output0 = testBox(totalValue / 4, pubkeyA, 0)
      val output1 = testBox(totalValue / 4, pubkeyB, 0)
      val output2 = testBox(totalValue / 4, pubkeyC, 0)
      val output3 = testBox(totalValue / 4, pubkeyD, 0)
      val tx = mkTxFromOutputs(output0, output1, output2, output3)
      val ctx = mkCtx(1000, tx, self)
      failingProofTest(thresholdProp, ctx, business)
    }


    val inputEnv = Map(
      "thresholdProp" -> ByteArrayConstant(Blake2b256(thresholdProp.treeWithSegregation.bytes)),
      "pubkeyA" -> pubkeyA
    )

    val inputProp = compile(inputEnv,
      s"""(OUTPUTS(0).value == $totalValue && blake2b256(OUTPUTS(0).propositionBytes) == thresholdProp) ||
         | (HEIGHT > 1000 && pubkeyA)
       """.stripMargin).asSigmaProp

    /**
      * height not higher, total value is equal
      */
    {
      val self = testBox(totalValue, inputProp, 0)
      val output = testBox(totalValue, thresholdProp, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1000, tx, self)
      successProofTest(inputProp, ctx, coopA, verifier)
    }

    /**
      * total value is lower, height is higher
      */
    {
      val self = testBox(totalValue - 1L, inputProp, 0)
      val output = testBox(totalValue - 1L, thresholdProp, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1001, tx, self)
      successProofTest(inputProp, ctx, coopA, verifier)
    }

    /**
      * negative condition
      */
    {
      val self = testBox(totalValue - 1L, inputProp, 0)
      val output = testBox(totalValue - 1L, thresholdProp, 0)
      val tx = mkTxFromOutputs(output)
      val ctx = mkCtx(1000, tx, self)
      failingProofTest(inputProp, ctx, coopA)
    }
  }
}
