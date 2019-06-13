package sigmastate.utxo

import org.ergoplatform.ErgoLikeContext
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.basics.DLogProtocol.DLogInteractiveProver
import sigmastate.lang.Terms._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter._

class DistributedSigSpecification extends SigmaTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  
  property("distributed AND") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val verifier = new ContextEnrichingTestProvingInterpreter

    val dlA = proverA.dlogSecrets.head.publicImage
    val dlB = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> dlA, "pubkeyB" -> dlB)
    val prop = compile(env, """pubkeyA && pubkeyB""").asSigmaProp

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val (bRandomness, bCommitment) = DLogInteractiveProver.firstMessage(dlB)

    val dlBKnown: Hint = OtherCommitment(dlB, bCommitment)
    val bag = HintsBag(Seq(dlBKnown))

    val prA = proverA.prove(prop, ctx, fakeMessage, bag).get

    val reducedTree = verifier.reduceToCrypto(ctx, prop).get._1

    val proofTree = verifier.computeCommitments(SigSerializer.parseAndComputeChallenges(reducedTree, prA.proof)).get.asInstanceOf[UncheckedSigmaTree]

    def traverseNode(tree: ProofTree, proposition: SigmaBoolean, hintsBag: HintsBag): HintsBag = {
      tree match {
        case leaf: UncheckedLeaf[_] =>
          if(proposition == leaf.proposition){
            val h = OtherSecretProven(leaf.proposition, leaf)
            hintsBag.addHint(h).addHint(OtherCommitment(leaf.proposition, leaf.commitmentOpt.get))
          } else hintsBag
        case inner: UncheckedConjecture =>
          inner.children.foldLeft(hintsBag){case (hb, c) => traverseNode(c, proposition, hb)}
      }
    }

    val bagB = traverseNode(proofTree, dlA, HintsBag.empty).addHint(OwnCommitment(dlB, bRandomness, bCommitment))

    val prB = proverB.prove(prop, ctx, fakeMessage, bagB).get

    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe false

    verifier.verify(prop, ctx, prB, fakeMessage).get._1 shouldBe true
  }

  property("distributed THRESHOLD") {

  }
}
