package sigmastate.serialization

import org.ergoplatform.validation.ValidationSpecification
import org.scalacheck.Gen
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks, GeneratorDrivenPropertyChecks}
import org.scalatest.{PropSpec, Assertion, Matchers}
import org.scalacheck.Arbitrary._
import sigmastate.Values._
import sigmastate.SType
import sigmastate.serialization.generators._
import sigmastate.utils.{SigmaByteReader, Helpers}

trait SerializationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Matchers
  with ObjectGenerators
  with ConcreteCollectionGenerators
  with OpcodesGen
  with TransformerGenerators
  with RelationGenerators
  with ValidationSpecification {

  protected def roundTripTest[V <: Value[_ <: SType]](
      v: V,
      getReader: Array[Byte] => SigmaByteReader = SigmaSerializer.startReader(_)
  ): Assertion = {
    val bytes = ValueSerializer.serialize(v)
    predefinedBytesTest(v, bytes, getReader)
    predefinedBytesTestNotFromZeroElement(bytes, v, getReader)
  }

  protected def predefinedBytesTest[V <: Value[_ <: SType]](
      v: V, bytes: Array[Byte],
      getReader: Array[Byte] => SigmaByteReader = SigmaSerializer.startReader(_)
  ): Assertion = {
    ValueSerializer.serialize(v) shouldEqual bytes
    val r = getReader(bytes)
    val positionLimitBefore = r.positionLimit
    val dv = ValueSerializer.deserialize(r)
    dv shouldEqual v
    r.positionLimit shouldBe positionLimitBefore
  }

  //check that pos and consumed are being implented correctly
  protected def predefinedBytesTestNotFromZeroElement[V <: Value[_ <: SType]](
      bytes: Array[Byte], v: V,
      getReader: Array[Byte] => SigmaByteReader
  ): Assertion = {
    val randomInt = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomInt, arbByte.arbitrary).sample.get.toArray
    val bytesCombined = Helpers.concatArrays(randomBytes, bytes)
    val r = getReader(bytesCombined)
    r.position = randomInt
    val parsedVal = ValueSerializer.deserialize(r)
    parsedVal shouldEqual v
  }

  def enrichedReaderForTuple(tuple: Tuple) = { bytes: Array[Byte] =>
    val r = SigmaSerializer.startReader(bytes)
    for (vu <- tuple.items.collect { case vu: ValUse[_] => vu }) {
      r.valDefTypeStore(vu.valId) = vu.tpe
    }
    r
  }

  def enrichedReader[T <: SType](vu: ValUse[T]) = { bytes: Array[Byte] =>
    val r = SigmaSerializer.startReader(bytes)
    r.valDefTypeStore(vu.valId) = vu.tpe
    r
  }

}
