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

    val skA = coopA.dlogSecrets.head
    val skB = coopB.dlogSecrets.head
    val skC = coopC.dlogSecrets.head
    val skD = coopD.dlogSecrets.head

    val pubkeyA = skA.publicImage
    val pubkeyB = skB.publicImage
    val pubkeyC = skC.publicImage
    val pubkeyD = skD.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    // Basic compilation
    val thresholdProp = compile(env,
      s"""atLeast(3, Array(pubkeyA, pubkeyB, pubkeyC, pubkeyD)) &&
        | OUTPUTS(0).value == $totalValue &&
        | blake2b256(OUTPUTS(0).propositionBytes) == ???
      """.stripMargin)


    val inputEnv = Map("thresholdProp" -> ByteArrayConstant(Blake2b256(thresholdProp.bytes)))
    val inputProp = compile(inputEnv,
      s"OUTPUTS(0).value == $totalValue && blake2b256(OUTPUTS(0).propositionBytes) == thresholdProp")



  }

}
