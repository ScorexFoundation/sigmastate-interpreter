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
    val size = 1024 * 1024 * 10
    an[SerializerException] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](size)(1))
  }

  property("zeroes") {
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](1)(0))
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](2)(0))
  }
}
