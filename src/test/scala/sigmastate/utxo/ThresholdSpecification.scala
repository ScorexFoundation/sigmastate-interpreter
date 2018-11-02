package sigmastate.utxo

import org.ergoplatform.ErgoLikeContext
import scapi.sigma.DLogProtocol.DLogProverInput
import sigmastate.Values.{ConcreteCollection, Value}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._


class ThresholdSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

  ignore("basic threshold compilation/execution") { // TODO Error in evaluate( ... SigmaDslBuilder.atLeast(...
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val skA = proverA.dlogSecrets.head
    val skB = proverB.dlogSecrets.head
    val skC = proverC.dlogSecrets.head

    val pubkeyA = skA.publicImage.isValid
    val pubkeyB = skB.publicImage.isValid
    val pubkeyC = skC.publicImage.isValid

    val proverABC = proverA.withSecrets(Seq(skB, skC))
    val proverAB = proverA.withSecrets(Seq(skB))
    val proverAC = proverA.withSecrets(Seq(skC))
    val proverBC = proverB.withSecrets(Seq(skC))

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)

    // Basic compilation
    val compiledProp1 = compile(env, """atLeast(2, Array(pubkeyA, pubkeyB, pubkeyC))""")
    val prop1 = AtLeast(2, pubkeyA, pubkeyB, pubkeyC)
    compiledProp1 shouldBe prop1

    // this example is from the white paper
    val compiledProp2 = compile(env,
      """{
        |    val array = Array(pubkeyA, pubkeyB, pubkeyC)
        |    atLeast(array.size, array)
        |}""".stripMargin).asBoolValue


    val prop2 = AtLeast(
      SizeOf(
        ConcreteCollection(Vector(pubkeyA, pubkeyB, pubkeyC))
      ),
      pubkeyA, pubkeyB, pubkeyC)
    compiledProp2 shouldBe prop2

    val proof = proverABC.prove(compiledProp2, ctx, fakeMessage).get
    verifier.verify(compiledProp2, ctx, proof, fakeMessage).get._1 shouldBe true

    val nonWorkingProvers2 = Seq(proverA, proverB, proverC, proverAB, proverAC, proverBC, proverD)
    for (prover <- nonWorkingProvers2) {
      prover.prove(compiledProp2, ctx, fakeMessage).isFailure shouldBe true
    }

    val prop2And = AND(pubkeyA, pubkeyB, pubkeyC)
    proverA.reduceToCrypto(ctx, compiledProp2).get._1 shouldBe proverA.reduceToCrypto(ctx, prop2And).get._1

    // this example is from the white paper
    val compiledProp3 = compile(env,
      """{
        |    val array = Array(pubkeyA, pubkeyB, pubkeyC)
        |    atLeast(1, array)
        |}""".stripMargin).asBoolValue
    val prop3 = AtLeast(1, pubkeyA, pubkeyB, pubkeyC)
    compiledProp3 shouldBe prop3

    val workingProvers3 = Seq(proverA, proverB, proverC, proverAB, proverBC, proverAC, proverABC)
    for (prover <- workingProvers3) {
      val proof = prover.prove(compiledProp3, ctx, fakeMessage).get
      verifier.verify(compiledProp3, ctx, proof, fakeMessage).get._1 shouldBe true
    }
    proverD.prove(compiledProp3, ctx, fakeMessage).isFailure shouldBe true

    val prop3Or = OR(pubkeyA, pubkeyB, pubkeyC)
    proverA.reduceToCrypto(ctx, compiledProp3).get._1 shouldBe proverA.reduceToCrypto(ctx, prop3Or).get._1

    val compiledProp4 = compile(env,
      """{
        |    val array = Array(pubkeyA, pubkeyB, pubkeyC)
        |    atLeast(2, array)
        |}""".stripMargin).asBoolValue
    val prop4 = AtLeast(2, pubkeyA, pubkeyB, pubkeyC)
    compiledProp4 shouldBe prop4

    val workingProvers4 = Seq(proverAB, proverBC, proverAC, proverABC)
    for (prover <- workingProvers4) {
      val proof = prover.prove(compiledProp4, ctx, fakeMessage).get
      verifier.verify(compiledProp4, ctx, proof, fakeMessage).get._1 shouldBe true
    }
    val nonWorkingProvers4 = Seq(proverA, proverB, proverC, proverD)
    for (prover <- nonWorkingProvers4) {
      prover.prove(compiledProp4, ctx, fakeMessage).isFailure shouldBe true
    }
  }

  ignore("threshold reduce to crypto") {
    val prover = new ErgoLikeTestProvingInterpreter
    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    case class TestCase(numTrue: Int, vector: Seq[Value[SBoolean.type]], dlogOnlyVector: DlogOnlyVector)
    case class DlogOnlyVector(v: Seq[Value[SBoolean.type]]) {
      lazy val orVersion = prover.reduceToCrypto(ctx, OR(v)).get._1
      lazy val andVersion = prover.reduceToCrypto(ctx, AND(v)).get._1
    }

    // Sequence of three secrets, in order to build test cases with 0, 1, 2, or 3 ProveDlogs inputs
    val secrets = Seq[DLogProverInput](DLogProverInput.random(), DLogProverInput.random(), DLogProverInput.random())

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


    var case0TrueHit = false
    var case0FalseHit = false
    var case1TrueHit = false
    var case1FalseHit = false
    var case1DLogHit = false
    var case2TrueHit = false
    var case2FalseHit = false
    var case2OrHit = false
    var case2AndHit = false
    var case2AtLeastHit = false

    // for each test case, make into atleast and reduce it to crypto with different thresholds
    for (t <- testCaseSeq) {
      for (bound <- 0 to testCaseSeq.length + 1) {
        val pReduced = prover.reduceToCrypto(ctx, AtLeast(bound, t.vector))
        pReduced.isSuccess shouldBe true
        if (t.dlogOnlyVector.v.isEmpty) { // Case 0: no ProveDlogs in the test vector -- just booleans
          if (t.numTrue >= bound) {
            pReduced.get._1 shouldBe tr
            case0TrueHit = true
          }
          else {
            pReduced.get._1 shouldBe fls
            case0FalseHit = true
          }
        }
        else if (t.dlogOnlyVector.v.length == 1) { // Case 1: 1 ProveDlog in the test vector
          // Should be just true if numTrue>=bound
          if (t.numTrue >= bound) {
            pReduced.get._1 shouldBe tr
            case1TrueHit = true
          }
          // Should be false if bound>numTrue + 1
          else if (bound > t.numTrue + 1) {
            pReduced.get._1 shouldBe fls
            case1FalseHit = true
          }
          // if bound is exactly numTrue+1, should be just dlog
          else if (bound == t.numTrue + 1) {
            pReduced.get._1 shouldBe t.dlogOnlyVector.v.head
            case1DLogHit = true
          }
        }
        else { // Case 2: more than 1 ProveDlogs in the test vector
          // Should be just true if numTrue>=bound
          if (t.numTrue >= bound) {
            pReduced.get._1 shouldBe tr
            case2TrueHit = true
          }
          // Should be false if bound>numTrue + dlogOnlyVector.length
          else if (bound > t.numTrue + t.dlogOnlyVector.v.length) {
            pReduced.get._1 shouldBe fls
            case2FalseHit = true
          }
          // if bound is exactly numTrue+dlogOnlyVector, should be just AND of all dlogs
          else if (bound == t.numTrue + t.dlogOnlyVector.v.length) {
            pReduced.get._1 shouldBe t.dlogOnlyVector.andVersion
            case2AndHit = true

          }
          // if bound is exactly numTrue+1, should be just OR of all dlogs
          else if (bound == t.numTrue + 1) {
            pReduced.get._1 shouldBe t.dlogOnlyVector.orVersion
            case2OrHit = true
          }
          // else should be AtLeast
          else {
            val atLeastReduced = prover.reduceToCrypto(ctx, AtLeast(bound - t.numTrue, t.dlogOnlyVector.v))
            pReduced.get._1 shouldBe atLeastReduced.get._1
            case2AtLeastHit = true
          }
        }
      }
    }
    case0FalseHit && case0TrueHit shouldBe true
    case1FalseHit && case1TrueHit && case1DLogHit shouldBe true
    case2FalseHit && case2TrueHit && case2AndHit && case2OrHit && case2AtLeastHit shouldBe true
  }

  // TODO LHF
  ignore("3-out-of-6 threshold") {
    // This example is from the white paper
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter
    val proverC = new ErgoLikeTestProvingInterpreter
    val proverD = new ErgoLikeTestProvingInterpreter
    val proverE = new ErgoLikeTestProvingInterpreter
    val proverF = new ErgoLikeTestProvingInterpreter
    val proverG = new ErgoLikeTestProvingInterpreter
    val proverH = new ErgoLikeTestProvingInterpreter
    val proverI = new ErgoLikeTestProvingInterpreter

    val skA = proverA.dlogSecrets.head
    val skB = proverB.dlogSecrets.head
    val skC = proverC.dlogSecrets.head
    val skD = proverD.dlogSecrets.head
    val skE = proverE.dlogSecrets.head
    val skF = proverF.dlogSecrets.head
    val skG = proverG.dlogSecrets.head
    val skH = proverH.dlogSecrets.head
    val skI = proverI.dlogSecrets.head

    val pkA = skA.publicImage
    val pkB = skB.publicImage
    val pkC = skC.publicImage
    val pkD = skD.publicImage
    val pkE = skE.publicImage
    val pkF = skF.publicImage
    val pkG = skG.publicImage
    val pkH = skH.publicImage
    val pkI = skI.publicImage


    val env = Map("pkA" -> pkA, "pkB" -> pkB, "pkC" -> pkC,
      "pkD" -> pkD, "pkE" -> pkE, "pkF" -> pkF,
      "pkG" -> pkG, "pkH" -> pkH, "pkI" -> pkI)
    val compiledProp = compile(env, """atLeast(3, Array (pkA, pkB, pkC, pkD && pkE, pkF && pkG, pkH && pkI))""")
    val prop = AtLeast(3, pkA, pkB, pkC, AND(pkD, pkE), AND(pkF, pkG), AND(pkH, pkI))

    compiledProp shouldBe prop

    val badProver = proverH.withSecrets(Seq(skB, skC, skE))
    val goodProver1 = badProver.withSecrets(Seq(skD))
    val goodProver2 = badProver.withSecrets(Seq(skA))
    val goodProver3 = badProver.withSecrets(Seq(skF, skG))
    val goodProver4 = badProver.withSecrets(Seq(skF, skG, skA))

    val goodProvers = Seq(goodProver1, goodProver2, goodProver3, goodProver4)

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val verifier = new ErgoLikeTestInterpreter


    for (prover <- goodProvers) {
      val proof = prover.prove(prop, ctx, fakeMessage).get
      verifier.verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
    }

    badProver.prove(prop, ctx, fakeMessage).isFailure shouldBe true

  }

  // TODO LHF
  ignore("threshold proving of different trees") {
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
    var provers = Seq[(Int, ErgoLikeTestProvingInterpreter)]((0, new ErgoLikeTestProvingInterpreter))
    // create 32 different provers
    for (i <- secrets.indices) {
      provers = provers ++ provers.map(p => (p._1 + 1, p._2.withSecrets(secrets(i))))
    }
    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val verifier = new ErgoLikeTestInterpreter

    def canProve(prover: ErgoLikeTestProvingInterpreter, proposition: Value[SBoolean.type]): Unit = {
      val proof = prover.prove(proposition, ctx, fakeMessage).get
      verifier.verify(proposition, ctx, proof, fakeMessage).get._1 shouldBe true
    }

    def cannotProve(prover: ErgoLikeTestProvingInterpreter, proposition: Value[SBoolean.type]): Unit = {
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
