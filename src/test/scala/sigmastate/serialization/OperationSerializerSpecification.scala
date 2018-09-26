package sigmastate.serialization

import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}

class OperationSerializerSpecification extends SerializationSpecification {

  property("operation serialization") {
    def roundTrip(op: Operation, serializer: OperationSerializer) = {
      val randValueBytes = serializer.serialize(op)
      val randValueRecovered = serializer.deserialize(randValueBytes)
      serializer.serialize(randValueRecovered) shouldEqual randValueBytes
    }

    forAll(operationGen) { op =>
      val keyLength = op.key.length
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
    tp <- Gen.choose(0, 6)
    key <- Arbitrary.arbitrary[Array[Byte]].map(k => ADKey @@ k)
    value <- Arbitrary.arbitrary[Array[Byte]].map(k => ADValue @@ k)
  } yield {
    tp match {
      case 1 => Lookup(key)
      case 2 => Remove(key)
      case 3 => RemoveIfExists(key)
      case 4 => Insert(key, value)
      case 5 => Update(key, value)
      case 6 => InsertOrUpdate(key, value)
      case _ => UnknownModification
    }
  }
}
