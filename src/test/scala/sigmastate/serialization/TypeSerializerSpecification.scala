package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.scalacheck.Arbitrary._
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate._

class TypeSerializerSpecification extends SerializationSpecification {

  def roundtrip[T <: SType](tpe: T, size: Int) = {
    val w = Serializer.startWriter()
        .putType(tpe)
    val bytes = w.toBytes
    bytes.length shouldBe size
    val r = Serializer.startReader(bytes, 0)
    val res = r.getType()
    res shouldBe tpe
    val randomPrefix = arrayGen[Byte].sample.get
    val r2 = Serializer.startReader(randomPrefix ++ bytes, randomPrefix.length)
    val res2 = r2.getType()
    res2 shouldBe tpe
  }

  property("Codes of primitive types have correct order") {
    for (i <- TypeSerializer.primIdToType.indices)
      i shouldBe TypeSerializer.primIdToType(i).typeCode
  }

  property("Primitive type serialization roundtrip") {
    forAll { t: SPrimType =>
      roundtrip(t, 1)
      roundtrip(SCollection(t), 1)
      roundtrip(SCollection(SCollection(t)), 1)
      roundtrip(SOption(t), 1)
      roundtrip(SOption(SCollection(t)), 1)
      roundtrip(STuple(t, t), 1)
      if (t != SInt) {
        roundtrip(STuple(t, SInt), 2)
        roundtrip(STuple(SInt, t), 2)
      }
      roundtrip(STuple(SCollection(SInt), t), 2)
    }
  }

  property("Complex type serialization roundtrip") {
    forAll { t: SPrimType =>
      roundtrip(SCollection(STuple(t, t)), 2)
      roundtrip(SCollection(SOption(t)), 2)
      roundtrip(SCollection(SCollection(STuple(t, t))), 3)
      roundtrip(SCollection(SOption(STuple(t, t))), 3)
      roundtrip(SOption(STuple(t, t)), 2)
      roundtrip(SOption(SCollection(STuple(t, t))), 3)
    }
  }

  property("Specific types serialization roundtrip") {
    roundtrip(STuple(SInt, SInt, SByte), 5)
    roundtrip(STuple(SCollection(SInt), SInt, SByte), 5)
    roundtrip(STuple(SCollection(SInt), SOption(SInt), SByte), 5)
    roundtrip(STuple(SCollection(SCollection(SInt)), SOption(SCollection(SInt)), SByte), 5)

    roundtrip(STuple(SCollection(SInt), SCollection(SInt)), 4)
    roundtrip(STuple(SCollection(SInt), SOption(SInt)), 4)
  }
}
