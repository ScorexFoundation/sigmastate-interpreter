package sigmastate.utxo

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.rework.DLogProtocol.{DLogCommonInput, DLogProverInput}
import sigmastate._


class TestingInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  import TestingInterpreter._

  implicit val soundness = 256
  implicit val dlogGroup: DlogGroup = new BcDlogECFp()

  property("Reduction to crypto example#1") {
    forAll() { (h: Int) =>
      whenever(h > 0 && h < Int.MaxValue - 1) {
        val dk1 = DLogProverInput.random()._2

        val env = TestingReducerInput(h)
        assert(reduceToCrypto(And(HeightFromProposition(h - 1), dk1), env).isInstanceOf[DLogCommonInput])
        assert(reduceToCrypto(And(HeightFromProposition(h), dk1), env).isInstanceOf[DLogCommonInput])
        assert(reduceToCrypto(And(HeightFromProposition(h + 1), dk1), env).isInstanceOf[FalseProposition.type])

        assert(reduceToCrypto(Or(HeightFromProposition(h - 1), dk1), env).isInstanceOf[TrueProposition.type])
        assert(reduceToCrypto(Or(HeightFromProposition(h), dk1), env).isInstanceOf[TrueProposition.type])
        assert(reduceToCrypto(Or(HeightFromProposition(h + 1), dk1), env).isInstanceOf[DLogCommonInput])
      }
    }
  }

  property("Reduction to crypto example#2") {
    forAll() { (h: Int) =>

      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = DLogProverInput.random()._2
        val dk2 = DLogProverInput.random()._2

        val env = TestingReducerInput(h)

        assert(reduceToCrypto(Or(
          And(HeightUntilProposition(h + 1), And(dk1, dk2)),
          And(HeightFromProposition(h + 1), dk1)
        ), env).isInstanceOf[CAnd])

        assert(reduceToCrypto(Or(
          And(HeightUntilProposition(h - 1), And(dk1, dk2)),
          And(HeightFromProposition(h - 1), dk1)
        ), env).isInstanceOf[DLogCommonInput])

        assert(reduceToCrypto(Or(
          And(HeightUntilProposition(h - 1), And(dk1, dk2)),
          And(HeightFromProposition(h + 1), dk1)
        ), env).isInstanceOf[FalseProposition.type])

        assert(reduceToCrypto(Or(Or(
          And(HeightUntilProposition(h - 1), And(dk1, dk2)),
          And(HeightFromProposition(h + 1), dk1)
        ), HeightBetweenProposition(h - 1, h + 1)), env).isInstanceOf[TrueProposition.type])
      }
    }
  }

  property("Evaluation example #1") {
    val dk1 = secrets(0).publicImage
    val dk2 = secrets(1).publicImage

    val env1 = TestingReducerInput(99)
    val env2 = TestingReducerInput(101)

    val prop = Or(
      And(HeightUntilProposition(100), And(dk1, dk2)),
      And(HeightFromProposition(100), dk1)
    )

    val challenge: ProofOfKnowledge.Challenge = dk1.bytes

    val proof1 = TestingInterpreter.prove(prop, env1, challenge).get

    evaluate(prop, env1, proof1, challenge).getOrElse(false) shouldBe true

    evaluate(prop, env2, proof1, challenge).getOrElse(false) shouldBe false
  }
}
