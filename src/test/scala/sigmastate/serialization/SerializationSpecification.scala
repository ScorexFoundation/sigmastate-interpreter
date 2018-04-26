package sigmastate.serialization

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.Values._
import sigmastate.SType
import sigmastate.serialization.generators.{ConcreteCollectionGenerators, RelationGenerators, TransformerGenerators, ValueGeneratots}

trait SerializationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Matchers
  with ValueGeneratots
  with ConcreteCollectionGenerators
  with TransformerGenerators
  with RelationGenerators {

  protected def roundTripTest[V <: Value[_ <: SType]](v: V): Assertion = {
    val bytes = ValueSerializer.serialize(v)
    predefinedBytesTest(bytes, v)
  }

  protected def predefinedBytesTest[V <: Value[_ <: SType]](bytes: Array[Byte], v: V): Assertion = {
    ValueSerializer.deserialize(bytes) shouldBe v
  }
}