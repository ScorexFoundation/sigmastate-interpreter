package sigmastate.serialization

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.SType
import sigmastate.Values._
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
    predefinedBytesTestNotFomZeroElement(bytes, v)
  }

  protected def predefinedBytesTest[V <: Value[_ <: SType]](bytes: Array[Byte], v: V): Assertion = {
    ValueSerializer.deserialize(bytes) shouldBe v
  }

  //check that pos and consumed are being implented correctly
  protected def predefinedBytesTestNotFomZeroElement[V <: Value[_ <: SType]](bytes: Array[Byte], v: V): Assertion = {
    val randomInt = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomInt, arbByte.arbitrary).sample.get.toArray
    ValueSerializer.deserialize(randomBytes ++ bytes, randomInt)._1 shouldBe v
  }

}