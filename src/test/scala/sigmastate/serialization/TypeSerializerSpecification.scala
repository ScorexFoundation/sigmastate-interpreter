package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate._

class TypeSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](tpe: T) = {
    val w = Serializer.startWriter()
        .putType(tpe)
    val bytes = w.toBytes
    val r = Serializer.startReader(bytes, 0)
    val res = r.getType()
    res shouldBe tpe
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = Serializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = r2.getType()
    res2 shouldBe tpe
  }

  property("Primitive type serialization roundtrip") {
    forAll { t: SPrimType => roundtrip(t) }
  }

}
