package sigmastate.serialization

import sigmastate.lang.exceptions.{InputSizeLimitExceeded, InvalidOpCode, InvalidTypePrefix, ValueDeserializeCallDepthExceeded}
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.{AND, SBoolean}

class DeserializationResilience extends SerializationSpecification {

  property("empty") {
    an[ArrayIndexOutOfBoundsException] should be thrownBy ValueSerializer.deserialize(Array[Byte]())
  }

  property("max size limit") {
    val bytes = Array.fill[Byte](SigmaSerializer.MaxInputSize + 1)(1)
    an[InputSizeLimitExceeded] should be thrownBy ValueSerializer.deserialize(bytes)
    an[InputSizeLimitExceeded] should be thrownBy ValueSerializer.deserialize(SigmaSerializer.startReader(bytes, 0))
  }

  property("zeroes (invalid type code in constant deserialization path") {
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](1)(0))
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](2)(0))
  }

  property("AND/OR nested crazy deep") {
    val evilBytes = List.tabulate(SigmaSerializer.MaxTreeDepth + 1)(_ => Array[Byte](AndCode, ConcreteCollectionCode, 2, SBoolean.typeCode))
      .toArray.flatten
    an[ValueDeserializeCallDepthExceeded] should be thrownBy
      SigmaSerializer.startReader(evilBytes, 0).getValue()
    // test other API endpoints
    an[ValueDeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(evilBytes, 0)
    an[ValueDeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(SigmaSerializer.startReader(evilBytes, 0))

    // guard should not be tripped up by a huge collection
    val goodBytes = SigmaSerializer.startWriter()
      .putValue(AND(List.tabulate(SigmaSerializer.MaxTreeDepth + 1)(_ => booleanExprGen.sample.get)))
      .toBytes
    ValueSerializer.deserialize(goodBytes, 0)
    // test other API endpoints
    ValueSerializer.deserialize(SigmaSerializer.startReader(goodBytes, 0))
    SigmaSerializer.startReader(goodBytes, 0).getValue()
  }

  property("invalid op code") {
    an[InvalidOpCode] should be thrownBy
      ValueSerializer.deserialize(Array.fill[Byte](1)(117.toByte))
  }
}
