package sigmastate.serialization

import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

import scala.annotation.tailrec

class OperationSerializer(keyLength: Int, valueLengthOpt: Option[Int]) extends Serializer[Operation, Operation] {

  def parseSeq(r: SigmaByteReader): Seq[Operation] = {
    @tailrec
    def parse(r: SigmaByteReader, acc: Seq[Operation]): Seq[Operation] = if (r.remaining > 0) {
      val op = parseBody(r)
      parse(r, op +: acc)
    } else {
      acc.reverse
    }

    parse(r, Seq())
  }

  def serializeSeq(ops: Seq[Operation]): Array[Byte] = {
    val w = Serializer.startWriter()
    ops.foreach(o => serializeBody(o, w))
    w.toBytes
  }

  override def parseBody(r: SigmaByteReader): Operation = {
    def parseValue(): ADValue = {
      val vl: Int = valueLengthOpt.getOrElse(r.getShort())
      ADValue @@ r.getBytes(vl)
    }

    r.getByte() match {
      case 1 => Lookup(ADKey @@ r.getBytes(keyLength))
      case 2 => Remove(ADKey @@ r.getBytes(keyLength))
      case 3 => RemoveIfExists(ADKey @@ r.getBytes(keyLength))
      case 4 => Insert(ADKey @@ r.getBytes(keyLength), parseValue())
      case 5 => Update(ADKey @@ r.getBytes(keyLength), parseValue())
      case 6 => InsertOrUpdate(ADKey @@ r.getBytes(keyLength), parseValue())
      case _ => throw new Exception("Unknown operation")
    }
  }

  override def serializeBody(o: Operation, w: SigmaByteWriter): Unit = {
    def serializeKey(tp: Byte, key: Array[Byte]): Unit = {
      SerializeLog.logPrintf(true, true, false, "Key")

      SerializeLog.logPrintf(true, true, false, "tp")
      w.put(tp)
      SerializeLog.logPrintf(false, true, false, "tp")

      SerializeLog.logPrintf(true, true, false, "key")
      w.putBytes(key)
      SerializeLog.logPrintf(false, true, false, "key")

      SerializeLog.logPrintf(false, true, false, "Key")
    }

    def serializeKeyValue(tp: Byte, key: Array[Byte], value: Array[Byte]): Unit = {
      SerializeLog.logPrintf(true, true, false, "KeyValue")

      SerializeLog.logPrintf(true, true, false, "key")
      serializeKey(tp, key)
      SerializeLog.logPrintf(false, true, false, "key")

      if (valueLengthOpt.isEmpty) {
        SerializeLog.logPrintf(true, true, false, "[valueLengthOpt.isEmpty]")

        SerializeLog.logPrintf(true, true, false, "value.length.toShort")
        w.putShort(value.length.toShort)
        SerializeLog.logPrintf(false, true, false, "value.length.toShort")

        SerializeLog.logPrintf(false, true, false, "[valueLengthOpt.isEmpty]")
      }

      SerializeLog.logPrintf(true, true, false, "value")
      w.putBytes(value)
      SerializeLog.logPrintf(false, true, false, "value")

      SerializeLog.logPrintf(false, true, false, "KeyValue")
    }

    SerializeLog.logPrintf(true, true, false, "Operation")

    o match {
      case Lookup(key) => serializeKey(1: Byte, key)
      case Remove(key) => serializeKey(2: Byte, key)
      case RemoveIfExists(key) => serializeKey(3: Byte, key)
      case Insert(key, value) => serializeKeyValue(4: Byte, key, value)
      case Update(key, value) => serializeKeyValue(5: Byte, key, value)
      case InsertOrUpdate(key, value) => serializeKeyValue(6: Byte, key, value)
      case _ => w.put(0: Byte)
    }

    SerializeLog.logPrintf(false, true, false, "Operation")
  }

}