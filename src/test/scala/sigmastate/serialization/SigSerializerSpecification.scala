package sigmastate.serialization

import org.ergoplatform.ErgoLikeContext
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.Values.{TrueLeaf, Value}
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.generators.ValueGenerators
import sigmastate.utils.Helpers
import sigmastate.utxo.Transformer

import scala.util.Random

class SigSerializerSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with SigmaTestingCommons
  with Matchers {

  private implicit val arbExprGen: Arbitrary[Value[SBoolean.type]] = Arbitrary(exprTreeGen)

  private val prover = new ErgoLikeProvingInterpreter()

  private val interpreterProveDlogGen: Gen[ProveDlog] =
    Gen.oneOf(prover.dlogSecrets.map(secret => ProveDlog(secret.publicImage.h)))

  private val interpreterProveDHTGen =
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
                           actual: ProofTree): Boolean =  (expected, actual) match {
    case (NoProof, NoProof) => true
    case (dht1: UncheckedDiffieHellmanTuple, dht2: UncheckedDiffieHellmanTuple) =>
      // `firstMessageOpt` is not serialized
      dht1.copy(commitmentOpt = None) == dht2
    case (sch1: UncheckedSchnorr, sch2: UncheckedSchnorr) =>
      // `firstMessageOpt` is not serialized
      sch1.copy(commitmentOpt = None) == sch2
    case (conj1: UncheckedConjecture, conj2: UncheckedConjecture) =>
        Helpers.optionArrayEquals(conj1.challengeOpt, conj2.challengeOpt) &&
        conj1.commitments == conj2.commitments &&
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

  property("SigSerializer round trip") {
    forAll { expr: Value[SBoolean.type] =>
      val challenge = Array.fill(32)(Random.nextInt(100).toByte)

      val ctx = ErgoLikeContext(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
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
