package sigmastate

import org.ergoplatform.ErgoLikeInterpreter
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.ByteArrayConstant
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}

class CoopExampleSpecification extends SigmaTestingCommons {

  property("commit to the threshold sig") {

    val coopA = new ErgoLikeProvingInterpreter
    val coopB = new ErgoLikeProvingInterpreter
    val coopC = new ErgoLikeProvingInterpreter
    val coopD = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val totalValue = 10000
    val toolValue = 5000
    val constructionValue = 2000

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

    def withdraw(minHeight: Int, totalValue: Int) = {
      s"""
         |let withdrawCondition = HEIGHT > $minHeight &&
         |        OUTPUTS(0).value >= ${totalValue / 4} && OUTPUTS(0).propositionBytes == pubkeyA.propBytes &&
         |        OUTPUTS(1).value >= ${totalValue / 4} && OUTPUTS(1).propositionBytes == pubkeyB.propBytes &&
         |        OUTPUTS(2).value >= ${totalValue / 4} && OUTPUTS(2).propositionBytes == pubkeyC.propBytes &&
         |        OUTPUTS(3).value >= ${totalValue / 4} && OUTPUTS(3).propositionBytes == pubkeyD.propBytes
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
      """.stripMargin)


    val inputEnv = Map(
      "thresholdProp" -> ByteArrayConstant(Blake2b256(thresholdProp.bytes)),
      "pubkeyA" -> pubkeyA
    )

    val inputProp = compile(inputEnv,
      s"""(OUTPUTS(0).value == $totalValue && blake2b256(OUTPUTS(0).propositionBytes) == thresholdProp) ||
         | (HEIGHT > 1000 && pubkeyA)
       """.stripMargin)

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
      "business" -> businessKey,
      "pubkeyTool1" -> toolRing(0),
      "pubkeyTool2" -> toolRing(1),
      "pubkeyTool3" -> toolRing(2),
      "pubkeyTool4" -> toolRing(3),
      "pubkeyConstr1" -> constructionRing(0),
      "pubkeyConstr2" -> constructionRing(1),
      "pubkeyConstr3" -> constructionRing(2)
    )


    val spendingProp1 = compile(spendingEnv,
      s"""
         | let spendingSuccess = (pubkeyTool1 || pubkeyTool2 || pubkeyTool3 || pubkeyTool4) && business
         |
         | ${withdraw(5000, toolValue)}
         |
         | spendingSuccess || withdrawCondition
       """.stripMargin)

    val spendingProp2 = compile(spendingEnv,
      s"""
         | let spendingSuccess = (pubkeyConstr1 || pubkeyConstr2 || pubkeyConstr3) && business
         |
         | ${withdraw(5000, constructionValue)}
         |
         | spendingSuccess || withdrawCondition
       """.stripMargin)

    //todo: failback
    val spendingProp3 = compile(spendingEnv, "business")
  }
}
