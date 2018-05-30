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
    for (i <- TypeSerializer.embeddableIdToType.indices.drop(1))
      i shouldBe TypeSerializer.embeddableIdToType(i).typeCode
  }

  property("Primitive type serialization roundtrip") {
    forAll { t: SPrimType =>
      roundtrip(t, 1)
      roundtrip(SCollection(t), 1)
      roundtrip(SCollection(SCollection(t)), 1)
      roundtrip(SOption(t), 1)
      roundtrip(SOption(SCollection(t)), 1)
      roundtrip(STuple(t, t), 1)
      if (t != SLong) {
        roundtrip(STuple(t, SLong), 2)
        roundtrip(STuple(SLong, t), 2)
      }
      roundtrip(STuple(SCollection(SLong), t), 2)
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
    roundtrip(STuple(SLong, SLong, SByte), 5)
    roundtrip(STuple(SCollection(SLong), SLong, SByte), 5)
    roundtrip(STuple(SCollection(SLong), SOption(SLong), SByte), 5)
    roundtrip(STuple(SCollection(SCollection(SLong)), SOption(SCollection(SLong)), SByte), 5)

    roundtrip(STuple(SCollection(SLong), SCollection(SLong)), 4)
    roundtrip(STuple(SCollection(SLong), SOption(SLong)), 4)
  }
}
