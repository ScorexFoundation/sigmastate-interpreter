package sigmastate.utxo

import org.scalatest.{Assertion, Matchers}
import org.scalatest.TryValues._
import sigmastate.serialization.Serializer

trait SerializationRoundTripSpec extends Matchers {

  protected def roundTripTest[T](v: T)(implicit serializer: Serializer[T]): Assertion = {
    val bytes = serializer.toBytes(v)
    predefinedBytesTest(bytes, v)
  }

  protected def predefinedBytesTest[T](bytes: Array[Byte], v: T)(implicit serializer: Serializer[T]): Assertion = {
    serializer.parseBytes(bytes).success.value shouldBe v
  }
}