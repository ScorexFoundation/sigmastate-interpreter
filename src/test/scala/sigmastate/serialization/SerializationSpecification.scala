package sigmastate.serialization

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.{SType, Value}

trait SerializationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Matchers {

  protected def roundTripTest[V <: Value[_ <: SType]](v: V): Assertion = {
    val bytes = SigmaSerializer.serialize(v)
    val t = SigmaSerializer.deserialize(bytes)
    t shouldBe v
  }

  protected def predefinedBytesTest[V <: Value[_ <: SType]](bytes: Array[Byte], v: V): Assertion = {
    SigmaSerializer.deserialize(bytes) shouldBe v
  }
}