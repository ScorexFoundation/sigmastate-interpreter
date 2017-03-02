package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import sigmastate._


class UtxoBlockchainReducerSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  import TestingInterpreter._

  property("Reduction to crypto example#1") {
    forAll() { (h: Int) =>
      whenever(h > 0 && h < Int.MaxValue - 1) {
        val dk1 = DLogProposition(Array.fill(32)(0: Byte))

        val env = TestingReducerInput(h)
        assert(reduceToCrypto(And(HeightFromProposition(h - 1), dk1), env).isInstanceOf[DLogProposition])
        assert(reduceToCrypto(And(HeightFromProposition(h), dk1), env).isInstanceOf[DLogProposition])
        assert(reduceToCrypto(And(HeightFromProposition(h + 1), dk1), env).isInstanceOf[FalseProposition.type])

        assert(reduceToCrypto(Or(HeightFromProposition(h - 1), dk1), env).isInstanceOf[TrueProposition.type])
        assert(reduceToCrypto(Or(HeightFromProposition(h), dk1), env).isInstanceOf[TrueProposition.type])
        assert(reduceToCrypto(Or(HeightFromProposition(h + 1), dk1), env).isInstanceOf[DLogProposition])
      }
    }
  }

  property("Reduction to crypto example#2") {
    forAll() { (h: Int) =>

      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = DLogProposition(Array.fill(32)(0: Byte))
        val dk2 = DLogProposition(Array.fill(32)(1: Byte))

        val env = TestingReducerInput(h)

        assert(reduceToCrypto(Or(
          And(HeightUntilProposition(h + 1), And(dk1, dk2)),
          And(HeightFromProposition(h + 1), dk1)
        ), env).isInstanceOf[CAnd[DLogProposition, DLogProposition]])

        assert(reduceToCrypto(Or(
          And(HeightUntilProposition(h - 1), And(dk1, dk2)),
          And(HeightFromProposition(h - 1), dk1)
        ), env).isInstanceOf[DLogProposition])

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
    val dk1 = DLogProposition(Array.fill(32)(0: Byte))
    val dk2 = DLogProposition(Array.fill(32)(1: Byte))


    val env1 = TestingReducerInput(101)
    val env2 = TestingReducerInput(99)

    val prop = Or(
      And(HeightUntilProposition(100), And(dk1, dk2)),
      And(HeightFromProposition(100), dk1)
    )

    val challenge: Proof.Challenge = dk1.bytes

    evaluate(prop, env1, FakeSchnorrSignature, challenge).getOrElse(false) shouldBe true

    evaluate(prop, env2, FakeSchnorrSignature, challenge).getOrElse(false) shouldBe false
  }
}
