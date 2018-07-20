package sigmastate.serialization

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.lang.exceptions.{InvalidTypePrefix, SerializerException}

class DeserializationResilience extends PropSpec
  with PropertyChecks
  with Matchers {

  property("empty") {
    an[ArrayIndexOutOfBoundsException] should be thrownBy ValueSerializer.deserialize(Array[Byte]())
  }

  property("max size limit") {
    val bytes = Array.fill[Byte](Serializer.MaxInputSize + 1)(1)
    an[AssertionError] should be thrownBy ValueSerializer.deserialize(bytes)
    an[AssertionError] should be thrownBy ValueSerializer.deserialize(bytes, 0)
    // deliberately omitted assertion (hot path)
    ValueSerializer.deserialize(Serializer.startReader(bytes, 0))
  }

  property("zeroes") {
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](1)(0))
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](2)(0))
  }
}
