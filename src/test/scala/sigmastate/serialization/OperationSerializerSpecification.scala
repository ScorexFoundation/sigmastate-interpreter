package sigmastate.serialization

import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}

class OperationSerializerSpecification extends SerializationSpecification {

  val keyLength: Int = 32

  property("operation seq serialization") {
    forAll(Gen.nonEmptyListOf(operationGen)) { ops =>
      val serializer = new OperationSerializer(keyLength, None)
      val bytes = serializer.serializeSeq(ops)
      val parsed = serializer.parseSeq(Serializer.startReader(bytes, 0))
      val bytes2 = serializer.serializeSeq(parsed)
      bytes2 shouldEqual bytes
    }
  }

  property("operation serialization") {
    def roundTrip(op: Operation, serializer: OperationSerializer) = {
      val randValueBytes = serializer.toBytes(op)
      val randValueRecovered = serializer.parseBody(Serializer.startReader(randValueBytes, 0))
      serializer.toBytes(randValueRecovered) shouldEqual randValueBytes
    }

    forAll(operationGen) { op =>
      val valueLength = op match {
        case Insert(k, v) => v.length
        case Update(k, v) => v.length
        case InsertOrUpdate(k, v) => v.length
        case _ => 0
      }
      roundTrip(op, new OperationSerializer(keyLength, None))
      roundTrip(op, new OperationSerializer(keyLength, Some(valueLength)))
    }
  }

  lazy val operationGen: Gen[Operation] = for {
    tp <- Gen.choose(1, 6)
    key <- Gen.listOfN(keyLength, Arbitrary.arbitrary[Byte]).map(_.toArray).map(k => ADKey @@ k)
    value <- Arbitrary.arbitrary[Array[Byte]].map(k => ADValue @@ k)
  } yield {
    tp match {
      case 1 => Lookup(key)
      case 2 => Remove(key)
      case 3 => RemoveIfExists(key)
      case 4 => Insert(key, value)
      case 5 => Update(key, value)
      case 6 => InsertOrUpdate(key, value)
      case _ => ???
    }
  }
}
