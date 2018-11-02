package sigmastate.serialization

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import org.scalacheck.Arbitrary._
import sigmastate.Values._
import sigmastate.SType
import sigmastate.serialization.generators._

trait SerializationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Matchers
  with ValueGenerators
  with ConcreteCollectionGenerators
  with OpcodesGen
  with TransformerGenerators
  with RelationGenerators {

  protected def roundTripTest[V <: Value[_ <: SType]](v: V): Assertion = {
    val bytes = ValueSerializer.serialize(v)
    predefinedBytesTest(v, bytes)
    predefinedBytesTestNotFomZeroElement(bytes, v)
  }

  protected def predefinedBytesTest[V <: Value[_ <: SType]](v: V, bytes: Array[Byte]): Assertion = {
    ValueSerializer.serialize(v) shouldEqual bytes
    ValueSerializer.deserialize(bytes) shouldEqual v
  }

  //check that pos and consumed are being implented correctly
  protected def predefinedBytesTestNotFomZeroElement[V <: Value[_ <: SType]](bytes: Array[Byte], v: V): Assertion = {
    val randomInt = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomInt, arbByte.arbitrary).sample.get.toArray
    val parsedVal = ValueSerializer.deserialize(randomBytes ++ bytes, randomInt)
    parsedVal shouldEqual v
  }
}
