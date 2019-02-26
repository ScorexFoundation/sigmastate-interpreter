package sigmastate.utxo

import org.ergoplatform.ErgoLikeContext
import sigmastate.AvlTreeData
import sigmastate.lang.Terms._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}

class DistributedSigSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  
  property("distributed AND") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val dlA = proverA.dlogSecrets.head.publicImage
    val dlB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> dlA, "pubkeyB" -> dlB)
    val compiledProp = compileWithCosting(env, """pubkeyA && pubkeyB""").asSigmaProp

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
  }

  property("distributed THRESHOLD") {

  }

}
