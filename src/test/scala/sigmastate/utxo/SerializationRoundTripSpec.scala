package sigmastate.utxo

import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransaction}
import org.ergoplatform._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.Serializer
import sigmastate.serialization.generators.ValueGenerators

class SerializationRoundTripSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ValueGenerators {

  private def roundTripTest[T](v: T)(implicit serializer: Serializer[T, T]): Assertion = {
    val bytes = serializer.toBytes(v)
    serializer.parseBody(Serializer.startReader(bytes)) shouldBe v
  }

  private def roundTripTestWithPos[T](v: T)(implicit serializer: Serializer[T, T]): Assertion = {
    val randomBytesCount = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
    val bytes = serializer.toBytes(v)
    serializer.parseBody(Serializer.startReader(bytes)) shouldBe v
    serializer.parseBody(Serializer.startReader(randomBytes ++ bytes, randomBytesCount)) shouldBe v
  }

  property("ErgoBoxCandidate: Serializer round trip") {
    forAll { t: ErgoBoxCandidate => roundTripTest(t)(ErgoBoxCandidate.serializer) }
    forAll { t: ErgoBoxCandidate => roundTripTestWithPos(t)(ErgoBoxCandidate.serializer) }
  }

  property("ErgoBox: Serializer round trip") {
    forAll { t: ErgoBox => roundTripTest(t)(ErgoBox.serializer) }
    forAll { t: ErgoBox => roundTripTestWithPos(t)(ErgoBox.serializer) }
  }

  property("ErgoLikeTransaction: Serializer round trip") {
    forAll { t: ErgoLikeTransaction => roundTripTest(t)(ErgoLikeTransaction.serializer) }
    forAll { t: ErgoLikeTransaction => roundTripTestWithPos(t)(ErgoLikeTransaction.serializer) }
  }

  property("ContextExtension: Serializer round trip") {
    forAll { t: ContextExtension => roundTripTest(t)(ContextExtension.serializer) }
    forAll { t: ContextExtension => roundTripTestWithPos(t)(ContextExtension.serializer) }
  }

  property("SerializedProverResult: Serializer round trip") {
    forAll { t: ProverResult => roundTripTest(t)(ProverResult.serializer) }
    forAll { t: ProverResult => roundTripTestWithPos(t)(ProverResult.serializer) }
  }

  property("UnsignedInput: Serializer round trip") {
    forAll { t: UnsignedInput => roundTripTest(t)(UnsignedInput.serializer) }
    forAll { t: UnsignedInput => roundTripTestWithPos(t)(UnsignedInput.serializer) }
  }

  property("Input: Serializer round trip") {
    forAll { t: Input => roundTripTest(t)(Input.serializer) }
    forAll { t: Input => roundTripTestWithPos(t)(Input.serializer) }
  }
}
