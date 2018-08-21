package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter}
import scapi.sigma.DLogProtocol.DLogProverInput
import sigmastate.Values.Value
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}


class ThresholdSpecification extends SigmaTestingCommons {

  property("threshold compilation") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter
    val proverC = new ErgoLikeProvingInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """atLeast(2, Array(pubkeyA, pubkeyB, pubkeyC))""")
    val prop = AtLeast(2, pubkeyA, pubkeyB, pubkeyC)
    compiledProp shouldBe prop
  }

  property("threshold reduce to crypto") {
    val prover = new ErgoLikeProvingInterpreter
    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    case class TestCase(numTrue: Int, vector: Seq[Value[SBoolean.type]], dlogOnlyVector: DlogOnlyVector)
    case class DlogOnlyVector(v: Seq[Value[SBoolean.type]]) {
      lazy val orVersion = prover.reduceToCrypto(ctx, OR(v)).get._1
      lazy val andVersion = prover.reduceToCrypto(ctx, AND(v)).get._1
    }

    val secrets = Seq[DLogProverInput](DLogProverInput.random())

    val fls = SBoolean.mkConstant(false)
    val tr = SBoolean.mkConstant(true)
    val emptyDlogOnlyVector = DlogOnlyVector(Seq())

    // build test cases of length 0 to 3 ProveDlogs with all possible inbetweens: nothing, false, true
    var testCaseSeq = Seq[TestCase](
      TestCase(0, Seq(), emptyDlogOnlyVector),
      TestCase(0, Seq(fls), emptyDlogOnlyVector),
      TestCase(1, Seq(tr), emptyDlogOnlyVector))
    for (sk <- secrets) {
      val pk = sk.publicImage
      var newTestCaseSeq = Seq[TestCase]()
      for (t <- testCaseSeq) {
        val dlogOnly = DlogOnlyVector(t.dlogOnlyVector.v :+ pk)
        newTestCaseSeq ++=
          Seq(t,
            TestCase(t.numTrue, t.vector :+ pk, dlogOnly),
            TestCase(t.numTrue, t.vector :+ pk :+ fls, dlogOnly),
            TestCase(t.numTrue + 1, t.vector :+ pk :+ tr, dlogOnly))
      }
      testCaseSeq = newTestCaseSeq
    }

    // for each test case, make into atleast and reduce it to crypto with different thresholds
    for (t <- testCaseSeq) {
      for (bound <- 0 to testCaseSeq.length + 1) {
        val pReduced = prover.reduceToCrypto(ctx, AtLeast(bound, t.vector))
        pReduced.isSuccess shouldBe true
        if (t.dlogOnlyVector.v.isEmpty) {
          // Should be just true or false depending on numTrue vs bound
          pReduced.get._1 shouldBe (if (t.numTrue >= bound) tr else fls)
        }
        else if (t.dlogOnlyVector.v.length == 1) {
          // Should be just true if numTrue>=bound
          if (t.numTrue >= bound) pReduced.get._1 shouldBe tr
          // Should be false if bound>numTrue + 1
          else if (bound > t.numTrue + 1) pReduced.get._1 shouldBe fls
          // if bound is exactly numTrue+1, should be just dlog
          else if (bound == t.numTrue + 1) pReduced.get._1 shouldBe t.dlogOnlyVector.v.head
        }
        else {
          // Should be just true if numTrue>=bound
          if (t.numTrue >= bound) pReduced.get._1 shouldBe tr
          // Should be false if bound>numTrue + dlogOnlyVector.length
          else if (bound > t.numTrue + t.dlogOnlyVector.v.length) pReduced.get._1 shouldBe fls
          // if bound is exactly numTrue+dlogOnlyVector, should be just AND of all dlogs
          else if (bound == t.numTrue + t.dlogOnlyVector.v.length) pReduced.get._1 shouldBe t.dlogOnlyVector.andVersion
          // if bound is exactly numTrue+1, should be just OR of all dlogs
          else if (bound == t.numTrue + 1) pReduced.get._1 shouldBe t.dlogOnlyVector.orVersion
          // else should be AtLeast
          else {
            val atLeastReduced = prover.reduceToCrypto(ctx, AtLeast(bound - t.numTrue, t.dlogOnlyVector.v))
            pReduced.get._1 shouldBe atLeastReduced.get._1
          }
        }
      }
    }
  }

  property("threshold proving of different trees") {
    val secret1 = DLogProverInput.random()
    val subProp1 = secret1.publicImage
    val secret2 = DLogProverInput.random()
    val subProp2 = secret2.publicImage
    val secret31 = DLogProverInput.random()
    val secret32 = DLogProverInput.random()
    val subProp3 = OR(secret31.publicImage, secret32.publicImage)
    val secret41 = DLogProverInput.random()
    val secret42 = DLogProverInput.random()
    val subProp4 = AND(secret41.publicImage, secret42.publicImage)
    val secret51 = DLogProverInput.random()
    val secret52 = DLogProverInput.random()
    val secret53 = DLogProverInput.random()
    val subProp5 = AtLeast(2, secret51.publicImage, secret52.publicImage, secret53.publicImage)
    val secret6 = DLogProverInput.random()

    val propComponents = Seq(subProp1, subProp2, subProp3, subProp4, subProp5)
    val secrets = Seq(Seq(secret1), Seq(secret2), Seq(secret31), Seq(secret41, secret42), Seq(secret51, secret53))


    // the integer indicates how many subpropositions the prover can prove
    var provers = Seq[(Int, ErgoLikeProvingInterpreter)]((0, new ErgoLikeProvingInterpreter))
    // create 32 different provers
    for (i <- secrets.indices) {
      provers = provers ++ provers.map(p => (p._1 + 1, p._2.withSecrets(secrets(i))))
    }
    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val verifier = new ErgoLikeInterpreter

    def canProve(prover: ErgoLikeProvingInterpreter, proposition: Value[SBoolean.type]): Unit = {
      val proof = prover.prove(proposition, ctx, fakeMessage).get
      verifier.verify(proposition, ctx, proof, fakeMessage).get._1 shouldBe true
    }

    def cannotProve(prover: ErgoLikeProvingInterpreter, proposition: Value[SBoolean.type]): Unit = {
      prover.prove(proposition, ctx, fakeMessage).isFailure shouldBe true
    }


    var twoToi = 1
    for (i <- 0 to secrets.length) {
      for (bound <- 1 to i) {
        // don't go beyond i -- "threshold reduce to crypto" tests that atLeast then becomes false
        // don't test bound 0 -- "threshold reduce to crypto" tests that atLeast then becomes true
        val pureAtLeastProp = AtLeast(bound, propComponents.slice(0, i))
        val OrPlusAtLeastOnRightProp = OR(secret6.publicImage, pureAtLeastProp)
        val OrPlusAtLeastOnLeftProp = OR(pureAtLeastProp, secret6.publicImage)
        val AndPlusAtLeastOnLeftProp = AND(pureAtLeastProp, secret6.publicImage)
        val AndPlusAtLeastOnRightProp = AND(secret6.publicImage, pureAtLeastProp)

        for (p <- provers.slice(0, twoToi)) {
          val pWithSecret6 = p._2.withSecrets(Seq(secret6))
          // only consider first 2^i provers, because later provers have secrets that are not relevant to this proposition
          if (p._1 >= bound) { // enough secrets for at least
            // prover should be able to prove pure and both ors
            canProve(p._2, pureAtLeastProp)
            canProve(p._2, OrPlusAtLeastOnRightProp)
            canProve(p._2, OrPlusAtLeastOnLeftProp)

            // prover should be unable to prove ands
            cannotProve(p._2, AndPlusAtLeastOnRightProp)
            cannotProve(p._2, AndPlusAtLeastOnLeftProp)

            // prover with more secrets should be able to prove ands
            canProve(pWithSecret6, AndPlusAtLeastOnRightProp)
            canProve(pWithSecret6, AndPlusAtLeastOnLeftProp)

          } else { // not enough secrets for atLeast
            // prover should be unable to prove pure and both ors and both ands
            cannotProve(p._2, pureAtLeastProp)
            cannotProve(p._2, OrPlusAtLeastOnRightProp)
            cannotProve(p._2, OrPlusAtLeastOnLeftProp)
            cannotProve(p._2, AndPlusAtLeastOnRightProp)
            cannotProve(p._2, AndPlusAtLeastOnLeftProp)

            // prover with more secrets should be able to prove both ors
            canProve(pWithSecret6, OrPlusAtLeastOnRightProp)
            canProve(pWithSecret6, OrPlusAtLeastOnLeftProp)

            // prover with more secrets should still be unable to prove pure and both ands
            cannotProve(pWithSecret6, pureAtLeastProp)
            cannotProve(pWithSecret6, AndPlusAtLeastOnRightProp)
            cannotProve(pWithSecret6, AndPlusAtLeastOnLeftProp)
          }
        }

      }
      twoToi *= 2
    }
  }
}
