package sigmastate.serialization

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.Values._
import sigmastate.SType
import sigmastate.serialization.generators.{ConcreteCollectionGenerators, ValueGenerators}

trait SerializationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Matchers
  with ValueGenerators
  with ConcreteCollectionGenerators {

  protected def roundTripTest[V <: Value[_ <: SType]](v: V): Assertion = {
    val bytes = ValueSerializer.serialize(v)
    predefinedBytesTest(bytes, v)
  }

  protected def predefinedBytesTest[V <: Value[_ <: SType]](bytes: Array[Byte], v: V): Assertion = {
    ValueSerializer.deserialize(bytes) shouldBe v
  }
}