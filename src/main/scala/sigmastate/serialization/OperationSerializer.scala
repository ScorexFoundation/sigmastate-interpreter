package sigmastate.serialization

import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import sigmastate.utils.{ByteReader, ByteWriter}

import scala.annotation.tailrec

class OperationSerializer(keyLength: Int, valueLengthOpt: Option[Int]) extends Serializer[Operation, Operation] {

  def parseSeq(r: ByteReader): Seq[Operation] = {
    @tailrec
    def parse(r: ByteReader, acc: Seq[Operation]): Seq[Operation] = if (r.remaining > 0) {
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

  override def parseBody(r: ByteReader): Operation = {
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
      case _ => UnknownModification
    }
  }

  override def serializeBody(o: Operation, w: ByteWriter): Unit = {
    def serializeKey(tp: Byte, key: Array[Byte]): Unit = {
      w.put(tp)
      w.putBytes(key)
    }

    def serializeKeyValue(tp: Byte, key: Array[Byte], value: Array[Byte]): Unit = {
      serializeKey(tp, key)
      if (valueLengthOpt.isEmpty) {
        w.putShort(value.length.toShort)
      }
      w.putBytes(value)
    }

    o match {
      case Lookup(key) => serializeKey(1: Byte, key)
      case Remove(key) => serializeKey(2: Byte, key)
      case RemoveIfExists(key) => serializeKey(3: Byte, key)
      case Insert(key, value) => serializeKeyValue(4: Byte, key, value)
      case Update(key, value) => serializeKeyValue(5: Byte, key, value)
      case InsertOrUpdate(key, value) => serializeKeyValue(6: Byte, key, value)
      case _ => w.put(0: Byte)
    }
  }

}