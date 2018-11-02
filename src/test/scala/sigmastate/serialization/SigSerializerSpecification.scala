package sigmastate.serialization

import java.util

import org.ergoplatform.ErgoLikeContext
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.Values.{TrueLeaf, Value}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.generators.ValueGenerators
import sigmastate.utxo.Transformer

import scala.util.Random

class SigSerializerSpecification extends SigmaTestingCommons with ValueGenerators {
  implicit lazy val IR = new TestingIRContext
  private lazy implicit val arbExprGen: Arbitrary[Value[SBoolean.type]] = Arbitrary(exprTreeGen)

  private lazy val prover = new ErgoLikeTestProvingInterpreter()

  private lazy val interpreterProveDlogGen: Gen[ProveDlog] =
    Gen.oneOf(prover.dlogSecrets.map(secret => ProveDlog(secret.publicImage.h)))

  private lazy val interpreterProveDHTGen =
    Gen.oneOf(
      prover.dhSecrets
        .map(_.commonInput)
        .map(ci => ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.v)))

  private def exprTreeNodeGen: Gen[Transformer[SCollection[SBoolean.type], SBoolean.type]] = for {
    left <- exprTreeGen
    right <- exprTreeGen
    node <- Gen.oneOf(
      OR(left, right),
      AND(left, right)
    )
  } yield node

  private def exprTreeGen: Gen[Value[SBoolean.type]] =
    Gen.oneOf(interpreterProveDlogGen, interpreterProveDHTGen, Gen.delay(exprTreeNodeGen))

  private def isEquivalent(expected: ProofTree,
                           actual: ProofTree): Boolean = (expected, actual) match {
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


  private def roundTrip(uncheckedTree: UncheckedTree, exp: Value[SBoolean.type]): Assertion = {
    val bytes = SigSerializer.toBytes(uncheckedTree)
    val parsedUncheckedTree = SigSerializer.parseAndComputeChallenges(exp, bytes)
    isEquivalent(uncheckedTree, parsedUncheckedTree) shouldBe true
  }

  property("SigSerializer no proof round trip") {
    roundTrip(NoProof, TrueLeaf)
  }

  ignore("SigSerializer round trip") {  // TODO ClassCastException in SigmaDslBuilder.allOf(...
    forAll { expr: Value[SBoolean.type] =>
      val challenge = Array.fill(32)(Random.nextInt(100).toByte)

      val ctx = ErgoLikeContext(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContext.dummyPubkey,
        boxesToSpend = IndexedSeq(),
        spendingTransaction = null,
        self = fakeSelf)

      // get sigma conjectures out of transformers
      val prop = prover.reduceToCrypto(ctx, expr).get._1

      val proof = prover.prove(expr, ctx, challenge).get.proof
      val proofTree = SigSerializer.parseAndComputeChallenges(prop, proof)
      roundTrip(proofTree, prop)
    }
  }

}
