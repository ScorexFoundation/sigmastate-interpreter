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

  /**
    * An example test where Alice (A) and Bob (B) are signing an input in a distributed way. A statement which
    * protects the box to spend is "pubkey_Alice && pubkey_Bob". Note that a signature is about a transcript of a
    * Sigma-protocol (a, e, z), which is done in non-interactive way (thus "e" is got via Fiat-Shamir transformation).
    *
    * For that, they are going through following steps:
    *
    * - Bob is generating (a,e) and sends "a" them to Alice
    * - Alice forms a hint which contains Bob's commitment "a" and puts into a hints bag for her prover
    *
    *
    */
  property("distributed AND") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val verifier = new ContextEnrichingTestProvingInterpreter

    val pubkeyAlice = proverA.dlogSecrets.head.publicImage
    val pubkeyBob = proverB.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyAlice, "pubkeyB" -> pubkeyBob)
    val prop = compile(env, """pubkeyA && pubkeyB""").asSigmaProp

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = null,
      self = fakeSelf)

    val (bRandomness, bCommitment) = DLogInteractiveProver.firstMessage(pubkeyBob)

    val dlBKnown: Hint = OtherCommitment(pubkeyBob, bCommitment)
    val bag = HintsBag(Seq(dlBKnown))

    val proofAlice = proverA.prove(prop, ctx, fakeMessage, bag).get

    val reducedTree = verifier.reduceToCrypto(ctx, prop).get._1

    val proofTree = verifier.computeCommitments(SigSerializer.parseAndComputeChallenges(reducedTree, proofAlice.proof)).get.asInstanceOf[UncheckedSigmaTree]

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

    val bagB = traverseNode(proofTree, pubkeyAlice, HintsBag.empty).addHint(OwnCommitment(pubkeyBob, bRandomness, bCommitment))

    val prB = proverB.prove(prop, ctx, fakeMessage, bagB).get

    verifier.verify(prop, ctx, proofAlice, fakeMessage).get._1 shouldBe false

    verifier.verify(prop, ctx, prB, fakeMessage).get._1 shouldBe true
  }

  property("distributed THRESHOLD") {

  }
}
