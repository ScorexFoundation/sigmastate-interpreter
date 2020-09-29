package sigmastate.serialization

import java.util

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.eval.IRContextFactoryImpl
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, SigmaTestingCommons, ErgoLikeTransactionTesting}
import sigmastate.serialization.generators.ObjectGenerators

import scala.util.Random

class SigSerializerSpecification extends SigmaTestingCommons with ObjectGenerators {
  def createIR = new TestingIRContext
  implicit lazy val IR = createIR
  implicit lazy val irFactory = new IRContextFactoryImpl(createIR)
  private lazy implicit val arbExprGen: Arbitrary[SigmaBoolean] = Arbitrary(exprTreeGen)

  private lazy val prover = new ContextEnrichingTestProvingInterpreter()

  private lazy val interpreterProveDlogGen: Gen[ProveDlog] =
    Gen.oneOf(prover.dlogSecrets.map(secret => ProveDlog(secret.publicImage.h)))

  private lazy val interpreterProveDHTGen =
    Gen.oneOf(
      prover.dhSecrets
        .map(_.commonInput)
        .map(ci => ProveDHTuple(ci.g, ci.h, ci.u, ci.v)))

  private def exprTreeNodeGen: Gen[SigmaBoolean] = for {
    left <- exprTreeGen
    right <- exprTreeGen
    node <- Gen.oneOf(
      COR(Seq(left, right)),
      CAND(Seq(left, right))
    )
  } yield node

  private def exprTreeGen: Gen[SigmaBoolean] =
    Gen.oneOf(interpreterProveDlogGen, interpreterProveDHTGen, Gen.delay(exprTreeNodeGen))

  private def isEquivalent(expected: ProofTree, actual: ProofTree): Boolean = (expected, actual) match {
    case (NoProof, NoProof) => true
    case (dht1: UncheckedDiffieHellmanTuple, dht2: UncheckedDiffieHellmanTuple) =>
      // `firstMessageOpt` is not serialized
      dht1.copy(commitmentOpt = None) == dht2
    case (sch1: UncheckedSchnorr, sch2: UncheckedSchnorr) =>
      // `firstMessageOpt` is not serialized
      sch1.copy(commitmentOpt = None) == sch2
    case (conj1: UncheckedConjecture, conj2: UncheckedConjecture) =>
      util.Arrays.equals(conj1.challenge, conj2.challenge) &&
        conj1.children.zip(conj2.children).forall(t => isEquivalent(t._1, t._2))
    case _ => false
  }


  private def roundTrip(uncheckedTree: UncheckedTree, exp: SigmaBoolean): Assertion = {
    val bytes = SigSerializer.toBytes(uncheckedTree)
    val parsedUncheckedTree = SigSerializer.parseAndComputeChallenges(exp, bytes)
    isEquivalent(uncheckedTree, parsedUncheckedTree) shouldBe true
  }

  property("SigSerializer no proof round trip") {
    roundTrip(NoProof, TrivialProp.TrueProp)
  }

  property("SigSerializer round trip") {
    forAll(configParams = MinSuccessful(100)) { sb: SigmaBoolean =>
      val expr = sb.toSigmaProp
      val challenge = Array.fill(32)(Random.nextInt(100).toByte)

      val ctx = ErgoLikeContextTesting(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(fakeSelf),
        spendingTransaction = ErgoLikeTransactionTesting.dummy,
        self = fakeSelf)
        .withCostLimit(Long.MaxValue) // To avoid occasional cost limit exceptions which are irrelevant here

      try {
        // get sigma conjectures out of transformers
        val prop = prover.reduceToCrypto(ctx, expr).get._1

        val proof = prover.prove(expr, ctx, challenge).get.proof
        val proofTree = SigSerializer.parseAndComputeChallenges(prop, proof)
        roundTrip(proofTree, prop)
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          throw t
      }
    }
  }

}
