package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scapi.sigma.DLogProtocol.{ProveDlog, DLogProverInput}
import scorex.crypto.hash.Blake2b256
import sigmastate.interpreter._
import sigmastate.utxo.CostTable

import scala.util.Random


case class TestingContext(height: Int,
                          override val extension: ContextExtension = ContextExtension(values = Map())) extends Context[TestingContext] {
  override def withExtension(newExtension: ContextExtension): TestingContext = this.copy(extension = newExtension)
}


object TestingInterpreter extends Interpreter with ProverInterpreter {
  override type StateT = StateTree
  override type CTX = TestingContext

  override val maxCost = CostTable.ScriptLimit

  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._

    Seq(DLogProverInput.random()._1, DLogProverInput.random()._1)
  }

  override val contextExtenders: Map[Byte, ByteArrayLeafConstant] = Map[Byte, ByteArrayLeafConstant]()

  override def specificTransformations(context: TestingContext): PartialFunction[SigmaStateTree, SigmaStateTree] = {
    case Height => IntLeafConstant(context.height)
  }
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
        val dk1 = ProveDlog(DLogProverInput.random()._2.h)

        val env = TestingContext(h)
        assert(reduceToCrypto(AND(GE(Height, IntLeafConstant(h - 1)), dk1), env).get.isInstanceOf[ProveDlog])
        assert(reduceToCrypto(AND(GE(Height, IntLeafConstant(h)), dk1), env).get.isInstanceOf[ProveDlog])
        assert(reduceToCrypto(AND(GE(Height, IntLeafConstant(h + 1)), dk1), env).get.isInstanceOf[FalseLeaf.type])

        assert(reduceToCrypto(OR(GE(Height, IntLeafConstant(h - 1)), dk1), env).get.isInstanceOf[TrueLeaf.type])
        assert(reduceToCrypto(OR(GE(Height, IntLeafConstant(h)), dk1), env).get.isInstanceOf[TrueLeaf.type])
        assert(reduceToCrypto(OR(GE(Height, IntLeafConstant(h + 1)), dk1), env).get.isInstanceOf[ProveDlog])
      }
    }
  }

  property("Reduction to crypto #2") {
    forAll() { (h: Int) =>

      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = ProveDlog(DLogProverInput.random()._2.h)
        val dk2 = ProveDlog(DLogProverInput.random()._2.h)

        val env = TestingContext(h)

        assert(reduceToCrypto(OR(
          AND(LE(Height, IntLeafConstant(h + 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeafConstant(h + 1)), dk1)
        ), env).get.isInstanceOf[CAND])


        assert(reduceToCrypto(OR(
          AND(LE(Height, IntLeafConstant(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeafConstant(h - 1)), dk1)
        ), env).get.isInstanceOf[ProveDlog])


        assert(reduceToCrypto(OR(
          AND(LE(Height, IntLeafConstant(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeafConstant(h + 1)), dk1)
        ), env).get.isInstanceOf[FalseLeaf.type])

        assert(reduceToCrypto(OR(OR(
          AND(LE(Height, IntLeafConstant(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntLeafConstant(h + 1)), dk1)
        ), AND(GT(Height, IntLeafConstant(h - 1)), LE(Height, IntLeafConstant(h + 1)))), env).get.isInstanceOf[TrueLeaf.type])

      }
    }
  }

  property("Evaluation example #1") {
    val dk1 = ProveDlog(secrets(0).publicImage.h)
    val dk2 = ProveDlog(secrets(1).publicImage.h)

    val env1 = TestingContext(99)
    val env2 = TestingContext(101)

    val prop = OR(
      AND(LE(Height, IntLeafConstant(100)), AND(dk1, dk2)),
      AND(GT(Height, IntLeafConstant(100)), dk1)
    )

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)

    val proof1 = TestingInterpreter.prove(prop, env1, challenge).get.proof

    verify(prop, env1, proof1, challenge).getOrElse(false) shouldBe true

    verify(prop, env2, proof1, challenge).getOrElse(false) shouldBe false
  }

  property("Evaluation - no real proving - true case") {
    val prop1 = TrueLeaf

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    verify(prop1, env, proof, challenge).getOrElse(false) shouldBe true

    val prop2 = OR(TrueLeaf, FalseLeaf)
    verify(prop2, env, proof, challenge).getOrElse(false) shouldBe true

    val prop3 = AND(TrueLeaf, TrueLeaf)
    verify(prop3, env, proof, challenge).getOrElse(false) shouldBe true

    val prop4 = GT(Height, IntLeafConstant(90))
    verify(prop4, env, proof, challenge).getOrElse(false) shouldBe true
  }

  property("Evaluation - no real proving - false case") {
    val prop1 = FalseLeaf

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    verify(prop1, env, proof, challenge).getOrElse(false) shouldBe false

    val prop2 = OR(FalseLeaf, FalseLeaf)
    verify(prop2, env, proof, challenge).getOrElse(false) shouldBe false

    val prop3 = AND(FalseLeaf, TrueLeaf)
    verify(prop3, env, proof, challenge).getOrElse(false) shouldBe false

    val prop4 = GT(Height, IntLeafConstant(100))
    verify(prop4, env, proof, challenge).getOrElse(false) shouldBe false
  }

  property("Evaluation - hash function") {
    val bytes = "hello world".getBytes
    val hash = Blake2b256(bytes)

    val prop1 = EQ(CalcBlake2b256(ByteArrayLeafConstant(bytes)), ByteArrayLeafConstant(hash))

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    verify(prop1, env, proof, challenge).getOrElse(false) shouldBe true

    val prop2 = NEQ(CalcBlake2b256(ByteArrayLeafConstant(bytes)), ByteArrayLeafConstant(hash))

    verify(prop2, env, proof, challenge).getOrElse(false) shouldBe false

    val prop3 = EQ(CalcBlake2b256(ByteArrayLeafConstant(bytes)), ByteArrayLeafConstant(bytes))

    verify(prop3, env, proof, challenge).getOrElse(false) shouldBe false
  }
}