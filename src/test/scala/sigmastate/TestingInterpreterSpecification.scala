package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scapi.sigma.rework.DLogProtocol.{DLogNode, DLogProverInput}
import scorex.crypto.hash.Blake2b256

import scala.util.Random


case class TestingContext(height: Int,
                          override val extension: ContextExtension = ContextExtension(value = Map())) extends Context[TestingContext] {
  override def withExtension(newExtension: ContextExtension): TestingContext = this.copy(extension = newExtension)
}


object TestingInterpreter extends Interpreter with DLogProverInterpreter {
  override type StateT = StateTree
  override type CTX = TestingContext

  override val maxDepth = 50

  override def specificPhases(tree: SigmaStateTree, context: TestingContext): SigmaStateTree = everywherebu(rule[Value] {
    case Height => IntLeaf(context.height)
  })(tree).get.asInstanceOf[SigmaStateTree]

  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._

    Seq(DLogProverInput.random()._1, DLogProverInput.random()._1)
  }

  override val contextExtensions: Map[Int, ByteArrayLeaf] = Map[Int, ByteArrayLeaf]()
}

class TestingInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  import TestingInterpreter._

  implicit val soundness = 256
  implicit val dlogGroup: DlogGroup = new BcDlogECFp()

  property("Reduction to crypto #1") {
    forAll() { (h: Int) =>
      whenever(h > 0 && h < Int.MaxValue - 1) {
        val dk1 = DLogNode(DLogProverInput.random()._2.h)

        val env = TestingContext(h)
        assert(reduceToCrypto(AND(GE(Height, IntLeaf(h - 1)), dk1), env).get.isInstanceOf[DLogNode])
        assert(reduceToCrypto(AND(GE(Height, IntLeaf(h)), dk1), env).get.isInstanceOf[DLogNode])
        assert(reduceToCrypto(AND(GE(Height, IntLeaf(h + 1)), dk1), env).get.isInstanceOf[FalseConstantTree.type])

        assert(reduceToCrypto(OR(GE(Height, IntLeaf(h - 1)), dk1), env).get.isInstanceOf[TrueConstantTree.type])
        assert(reduceToCrypto(OR(GE(Height, IntLeaf(h)), dk1), env).get.isInstanceOf[TrueConstantTree.type])
        assert(reduceToCrypto(OR(GE(Height, IntLeaf(h + 1)), dk1), env).get.isInstanceOf[DLogNode])
      }
    }
  }

  property("Reduction to crypto #2") {
    forAll() { (h: Int) =>

      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = DLogNode(DLogProverInput.random()._2.h)
        val dk2 = DLogNode(DLogProverInput.random()._2.h)

        val env = TestingContext(h)

        assert(reduceToCrypto(OR(
          AND(LE(Height, IntLeaf(h + 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeaf(h + 1)), dk1)
        ), env).get.isInstanceOf[CAND])


        assert(reduceToCrypto(OR(
          AND(LE(Height, IntLeaf(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeaf(h - 1)), dk1)
        ), env).get.isInstanceOf[DLogNode])


        assert(reduceToCrypto(OR(
          AND(LE(Height, IntLeaf(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeaf(h + 1)), dk1)
        ), env).get.isInstanceOf[FalseConstantTree.type])

        assert(reduceToCrypto(OR(OR(
          AND(LE(Height, IntLeaf(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeaf(h + 1)), dk1)
        ), AND(GT(Height, IntLeaf(h - 1)), LE(Height, IntLeaf(h + 1)))), env).get.isInstanceOf[TrueConstantTree.type])

      }
    }
  }

  property("Evaluation example #1") {
    val dk1 = DLogNode(secrets(0).publicImage.h)
    val dk2 = DLogNode(secrets(1).publicImage.h)

    val env1 = TestingContext(99)
    val env2 = TestingContext(101)

    val prop = OR(
      AND(LE(Height, IntLeaf(100)), AND(dk1, dk2)),
      AND(GT(Height, IntLeaf(100)), dk1)
    )

    val challenge: ProofOfKnowledge.Challenge = Array.fill(32)(Random.nextInt(100).toByte)

    val proof1 = TestingInterpreter.prove(prop, env1, challenge).get._1

    evaluate(prop, env1, proof1, challenge).getOrElse(false) shouldBe true

    evaluate(prop, env2, proof1, challenge).getOrElse(false) shouldBe false
  }

  property("Evaluation - no real proving - true case") {
    val prop1 = TrueConstantTree

    val challenge: ProofOfKnowledge.Challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    evaluate(prop1, env, proof, challenge).getOrElse(false) shouldBe true

    val prop2 = OR(TrueConstantTree, FalseConstantTree)
    evaluate(prop2, env, proof, challenge).getOrElse(false) shouldBe true

    val prop3 = AND(TrueConstantTree, TrueConstantTree)
    evaluate(prop3, env, proof, challenge).getOrElse(false) shouldBe true

    val prop4 = GT(Height, IntLeaf(90))
    evaluate(prop4, env, proof, challenge).getOrElse(false) shouldBe true
  }

  property("Evaluation - no real proving - false case") {
    val prop1 = FalseConstantTree

    val challenge: ProofOfKnowledge.Challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    evaluate(prop1, env, proof, challenge).getOrElse(false) shouldBe false

    val prop2 = OR(FalseConstantTree, FalseConstantTree)
    evaluate(prop2, env, proof, challenge).getOrElse(false) shouldBe false

    val prop3 = AND(FalseConstantTree, TrueConstantTree)
    evaluate(prop3, env, proof, challenge).getOrElse(false) shouldBe false

    val prop4 = GT(Height, IntLeaf(100))
    evaluate(prop4, env, proof, challenge).getOrElse(false) shouldBe false
  }

  property("Evaluation - hash function"){
    val bytes = "hello world".getBytes
    val hash = Blake2b256(bytes)

    val prop1 = EQ(CalcBlake2b256(ByteArrayLeaf(bytes)), ByteArrayLeaf(hash))

    val challenge: ProofOfKnowledge.Challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    evaluate(prop1, env, proof, challenge).getOrElse(false) shouldBe true

    val prop2 = NEQ(CalcBlake2b256(ByteArrayLeaf(bytes)), ByteArrayLeaf(hash))

    evaluate(prop2, env, proof, challenge).getOrElse(false) shouldBe false

    val prop3 = EQ(CalcBlake2b256(ByteArrayLeaf(bytes)), ByteArrayLeaf(bytes))

    evaluate(prop3, env, proof, challenge).getOrElse(false) shouldBe false
  }
}