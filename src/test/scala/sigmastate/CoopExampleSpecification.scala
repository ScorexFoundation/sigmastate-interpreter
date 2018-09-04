package sigmastate

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, ErgoLikeTransaction}
import org.scalatest.TryValues._
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.ByteArrayConstant
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._

class CoopExampleSpecification extends SigmaTestingCommons {

  property("commit to the threshold sig") {

    val coopA = new ErgoLikeProvingInterpreter
    val coopB = new ErgoLikeProvingInterpreter
    val coopC = new ErgoLikeProvingInterpreter
    val coopD = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

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

    val thresholdEnv = Map("pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "spendingContract1Hash" -> ByteArrayConstant(Blake2b256(Array.emptyByteArray)),
      "spendingContract2Hash" -> ByteArrayConstant(Blake2b256(Array.emptyByteArray)),
      "spendingContract3Hash" -> ByteArrayConstant(Blake2b256(Array.emptyByteArray))
    )

    def withdraw(minHeight: Long, totalValue: Long) = {
      s"""
         |let withdrawCondition = HEIGHT > ${minHeight}L &&
         |        OUTPUTS(0).value >= ${totalValue / 4}L && OUTPUTS(0).propositionBytes == pubkeyA.propBytes &&
         |        OUTPUTS(1).value >= ${totalValue / 4}L && OUTPUTS(1).propositionBytes == pubkeyB.propBytes &&
         |        OUTPUTS(2).value >= ${totalValue / 4}L && OUTPUTS(2).propositionBytes == pubkeyC.propBytes &&
         |        OUTPUTS(3).value >= ${totalValue / 4}L && OUTPUTS(3).propositionBytes == pubkeyD.propBytes
       """.stripMargin
    }

    // Basic compilation
    val thresholdProp = compile(thresholdEnv,
      s""" {
         | let votingSuccess = atLeast(3, Array(pubkeyA, pubkeyB, pubkeyC, pubkeyD))
         | let properSpending = OUTPUTS(0).value == $toolValue &&
         |                      blake2b256(OUTPUTS(0).propositionBytes) == spendingContract1Hash &&
         |                      OUTPUTS(1).value == $constructionValue &&
         |                      blake2b256(OUTPUTS(1).propositionBytes) == spendingContract2Hash &&
         |                      OUTPUTS(2).value == ${totalValue - toolValue - constructionValue} &&
         |                      blake2b256(OUTPUTS(2).propositionBytes) == spendingContract3Hash
         |
         | ${withdraw(1000, totalValue)}
         |
         | (votingSuccess && properSpending) || withdrawCondition
         | }
      """.stripMargin).asBoolValue


    val inputEnv = Map(
      "thresholdProp" -> ByteArrayConstant(Blake2b256(thresholdProp.bytes)),
      "pubkeyA" -> pubkeyA
    )

    val inputProp = compile(inputEnv,
      s"""(OUTPUTS(0).value == $totalValue && blake2b256(OUTPUTS(0).propositionBytes) == thresholdProp) ||
         | (HEIGHT > 1000 && pubkeyA)
       """.stripMargin).asBoolValue

    /**
      * height not higher, total value is equal
      */
    val output1 = ErgoBox(totalValue, inputProp)
    val tx1Output1 = ErgoBox(totalValue, thresholdProp)
    val tx1 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1))
    val ctx1 = ErgoLikeContext(
      currentHeight = 1000L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = output1)
    val proof1 = coopA.prove(inputProp, ctx1, fakeMessage).success.value.proof
    verifier.verify(inputProp, ctx1, proof1, fakeMessage) should be a 'success

    /**
      * total value is lower, height is higher
      */
    val output2 = ErgoBox(totalValue - 1L, inputProp)
    val tx2Output1 = ErgoBox(totalValue - 1L, thresholdProp)
    val tx2 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx2Output1))
    val ctx2 = ErgoLikeContext(
      currentHeight = 1001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx2,
      self = output2)
    val proof2 = coopA.prove(inputProp, ctx2, fakeMessage).success.value.proof
    verifier.verify(inputProp, ctx2, proof2, fakeMessage) should be a 'success

    /**
      * negative condition
      */
    val output3 = ErgoBox(totalValue - 1L, inputProp)
    val tx3Output1 = ErgoBox(totalValue - 1L, thresholdProp)
    val tx3 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx3Output1))
    val ctx3 = ErgoLikeContext(
      currentHeight = 1000L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx3,
      self = output3)
    coopA.prove(inputProp, ctx3, fakeMessage) should be a 'failure


    val toolA = new ErgoLikeProvingInterpreter
    val toolB = new ErgoLikeProvingInterpreter
    val toolC = new ErgoLikeProvingInterpreter
    val toolD = new ErgoLikeProvingInterpreter


    val toolRing = Seq(
      toolA.dlogSecrets.head.publicImage,
      toolB.dlogSecrets.head.publicImage,
      toolC.dlogSecrets.head.publicImage,
      toolD.dlogSecrets.head.publicImage)

    val constrA = new ErgoLikeProvingInterpreter
    val constrB = new ErgoLikeProvingInterpreter
    val constrC = new ErgoLikeProvingInterpreter

    val constructionRing = Seq(
      constrA.dlogSecrets.head.publicImage,
      constrB.dlogSecrets.head.publicImage,
      constrC.dlogSecrets.head.publicImage)

    val business = new ErgoLikeProvingInterpreter
    val businessKey = business.dlogSecrets.head.publicImage

    val spendingEnv = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "business" -> businessKey,
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
         | let spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && business
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
         |}
       """.stripMargin).asBoolValue

    val output4 = ErgoBox(totalValue, spendingProp1)
    val tx4Output1 = ErgoBox(totalValue / 4, pubkeyA)
    val tx4Output2 = ErgoBox(totalValue / 4, pubkeyB)
    val tx4Output3 = ErgoBox(totalValue / 4, pubkeyC)
    val tx4Output4 = ErgoBox(totalValue / 4, pubkeyD)
    val tx4 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx4Output1, tx4Output2, tx4Output3, tx4Output4))
    val ctx4 = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx4,
      self = output4)
    val proof4 = coopA.prove(spendingProp1, ctx4, fakeMessage).success.value.proof
    verifier.verify(spendingProp1, ctx4, proof4, fakeMessage) should be a 'success

    val spendingEnv2 = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "business" -> businessKey,
      "pubkeyTool1" -> SBoolean.mkConstant(false),
      "pubkeyTool2" -> SBoolean.mkConstant(false),
      "pubkeyTool3" -> SBoolean.mkConstant(false),
      "pubkeyTool4" -> SBoolean.mkConstant(false),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> constructionRing(2)
    )

    val spendingProp2 = compile(spendingEnv2,
      s"""
         |{
         | let spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && business
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
         |}
       """.stripMargin).asBoolValue

    /**
      * Will spend correctly if all the conditions are satisfied
      */
    val output5 = ErgoBox(totalValue, spendingProp2)
    val tx5Output1 = ErgoBox(totalValue / 4, pubkeyA)
    val tx5Output2 = ErgoBox(totalValue / 4, pubkeyB)
    val tx5Output3 = ErgoBox(totalValue / 4, pubkeyC)
    val tx5Output4 = ErgoBox(totalValue / 4, pubkeyD)
    val tx5 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx5Output1, tx5Output2, tx5Output3, tx5Output4))
    val ctx5 = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx5,
      self = output5)
    val proof5 = coopA.prove(spendingProp2, ctx5, fakeMessage).success.value.proof
    verifier.verify(spendingProp2, ctx5, proof5, fakeMessage) should be a 'success

    /**
      * Won't spend more then defined share
      */
    val output6 = ErgoBox(totalValue, spendingProp2)
    val tx6Output1 = ErgoBox(totalValue / 2, pubkeyB)
    val tx6Output2 = ErgoBox(totalValue / 2, pubkeyC)
    val tx6 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx6Output1, tx6Output2))
    val ctx6 = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx6,
      self = output6)
    coopA.prove(spendingProp2, ctx6, fakeMessage) should be a 'failure

    /**
      * Won't spend before minimail height
      */
    val output7 = ErgoBox(totalValue, spendingProp2)
    val tx7Output1 = ErgoBox(totalValue / 4, pubkeyA)
    val tx7Output2 = ErgoBox(totalValue / 4, pubkeyB)
    val tx7Output3 = ErgoBox(totalValue / 4, pubkeyC)
    val tx7Output4 = ErgoBox(totalValue / 4, pubkeyD)
    val tx7 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx7Output1, tx7Output2, tx7Output3, tx7Output4))
    val ctx7 = ErgoLikeContext(
      currentHeight = 4500L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx7,
      self = output7)
    coopA.prove(spendingProp2, ctx7, fakeMessage) should be a 'failure


    val spendingProp3 = compile(spendingEnv,
      s"""
         | {
         |
         |  let spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && business
         |
         |  ${withdraw(5000, constructionValue)}
         |
         |  spendingSuccess || withdrawCondition
         | }
       """.stripMargin).asBoolValue

    /**
      * Will spend correctly if all the conditions are satisfied
      */
    val output8 = ErgoBox(totalValue, spendingProp2)
    val tx8Output1 = ErgoBox(totalValue / 4, pubkeyA)
    val tx8Output2 = ErgoBox(totalValue / 4, pubkeyB)
    val tx8Output3 = ErgoBox(totalValue / 4, pubkeyC)
    val tx8Output4 = ErgoBox(totalValue / 4, pubkeyD)
    val tx8 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx8Output1, tx8Output2, tx8Output3, tx8Output4))
    val ctx8 = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx8,
      self = output8)
    val proof8 = coopA.prove(spendingProp3, ctx8, fakeMessage).success.value.proof
    verifier.verify(spendingProp3, ctx8, proof8, fakeMessage) should be a 'success

    val spendingEnv3 = Map(
      "pubkeyA" -> pubkeyA,
      "pubkeyB" -> pubkeyB,
      "pubkeyC" -> pubkeyC,
      "pubkeyD" -> pubkeyD,
      "business" -> businessKey,
      "pubkeyTool1" -> SBoolean.mkConstant(false),
      "pubkeyTool2" -> SBoolean.mkConstant(false),
      "pubkeyTool3" -> SBoolean.mkConstant(false),
      "pubkeyTool4" -> SBoolean.mkConstant(false),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> SBoolean.mkConstant(false)
    )

    val spendingProp4 = compile(spendingEnv3,
      s"""
         | {
         |
         |  let spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && business
         |
         |  ${withdraw(5000, constructionValue)}
         |
         |  spendingSuccess || withdrawCondition
         | }
       """.stripMargin).asBoolValue

    val output9 = ErgoBox(totalValue, spendingProp4)
    val tx9Output1 = ErgoBox(totalValue / 4, pubkeyA)
    val tx9Output2 = ErgoBox(totalValue / 4, pubkeyB)
    val tx9Output3 = ErgoBox(totalValue / 4, pubkeyC)
    val tx9Output4 = ErgoBox(totalValue / 4, pubkeyD)
    val tx9 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx9Output1, tx9Output2, tx9Output3, tx9Output4))
    val ctx9 = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx9,
      self = output9)
    val proof9 = coopA.prove(spendingProp4, ctx9, fakeMessage).success.value.proof
    verifier.verify(spendingProp4, ctx9, proof9, fakeMessage) should be a 'success

    //todo: failback
    val spendingProp5 = compile(spendingEnv, "business").asBoolValue

    val output10 = ErgoBox(totalValue, spendingProp5)
    val tx10Output1 = ErgoBox(totalValue, businessKey)
    val tx10 =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx10Output1))
    val ctx10 = ErgoLikeContext(
      currentHeight = 1L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx10,
      self = output10)
    val proof10 = business.prove(spendingProp5, ctx10, fakeMessage).success.value.proof
    coopA.prove(spendingProp5, ctx10, fakeMessage)should be a 'failure
    coopB.prove(spendingProp5, ctx10, fakeMessage)should be a 'failure
    coopC.prove(spendingProp5, ctx10, fakeMessage)should be a 'failure
    coopD.prove(spendingProp5, ctx10, fakeMessage)should be a 'failure
    verifier.verify(spendingProp5, ctx10, proof10, fakeMessage) should be a 'success

  }
}
