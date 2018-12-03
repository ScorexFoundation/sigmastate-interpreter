package sigmastate.utxo

import org.ergoplatform.ErgoLikeTransaction.FlattenedTransaction
import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransaction, _}
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import scorex.util.serialization._
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.SigmaSerializer
import sigmastate.serialization.generators.ValueGenerators

class SerializationRoundTripSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ValueGenerators {

  private def roundTripTest[T](v: T)(implicit serializer: SigmaSerializer[T, T]): Assertion = {
    val bytes = serializer.toBytes(v)
    serializer.parse(SigmaSerializer.startReader(bytes)) shouldBe v
  }

  private def roundTripTestErgo[T](v: T)(implicit serializer: Serializer[T, T, Reader, Writer]): Assertion = {
    val w = new VLQByteStringWriter()
    serializer.serialize(v, w)
    val bytes = w.result()
    bytes.nonEmpty shouldBe true
    serializer.parse(new VLQByteStringReader(bytes)) shouldBe v
  }

  private def roundTripTestWithPos[T](v: T)(implicit serializer: SigmaSerializer[T, T]): Assertion = {
    val randomBytesCount = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
    val bytes = serializer.toBytes(v)
    serializer.parse(SigmaSerializer.startReader(bytes)) shouldBe v
    serializer.parse(SigmaSerializer.startReader(randomBytes ++ bytes, randomBytesCount)) shouldBe v
  }

  property("ErgoBoxCandidate: Serializer round trip") {
    forAll { t: ErgoBoxCandidate => roundTripTest(t)(ErgoBoxCandidate.serializer) }
    forAll { t: ErgoBoxCandidate => roundTripTestWithPos(t)(ErgoBoxCandidate.serializer) }
  }

  property("ErgoBox: Serializer round trip") {
    forAll { t: ErgoBox => roundTripTest(t)(ErgoBox.sigmaSerializer) }
    forAll { t: ErgoBox => roundTripTestWithPos(t)(ErgoBox.sigmaSerializer) }
  }

  property("ErgoLikeTransaction: Serializer round trip") {
    forAll { t: ErgoLikeTransaction => roundTripTest(t)(ErgoLikeTransaction.serializer) }
    forAll { t: ErgoLikeTransaction => roundTripTestWithPos(t)(ErgoLikeTransaction.serializer) }
  }

  property("ErgoBox: Ergo serializer round trip") {
    forAll { t: ErgoBox => roundTripTestErgo(t)(ErgoBox.ergoSerializer) }
  }

  property("FlattenedTx: Ergo serializer round trip") {
    forAll { t: ErgoLikeTransaction =>
      roundTripTestErgo(FlattenedTransaction(t))(ErgoLikeTransaction.FlattenedTransaction.ergoSerializer)
    }
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
