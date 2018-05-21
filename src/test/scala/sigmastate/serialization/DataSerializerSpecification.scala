package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate.SCollection.SByteArray
import sigmastate.{SInt, SBoolean, SType, SByte}
import sigmastate.utils.ByteArrayBuilder

class DataSerializerSpecification extends SerializationSpecification {

  def test[T <: SType](obj: T#WrappedType, tpe: T) = {
    val b = new ByteArrayBuilder()
    DataSerializer.serialize(obj, tpe, b)
    val buf = ByteBuffer.wrap(b.toBytes)
    val res = DataSerializer.deserialize(tpe, buf)
    res shouldBe obj
  }

  property("Data serialization round trip") {
    forAll { x: Byte => test[SByte.type](x, SByte) }
    forAll { x: Boolean => test[SBoolean.type](x, SBoolean) }
    forAll { x: Long => test[SInt.type](x, SInt) }
    forAll { x: Array[Byte] => test[SByteArray](x, SByteArray) }
  }

}
