package sigmastate.serializer.bytes

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import scorex.core.serialization.Serializer

import scala.util.Try

trait BytesSerializable

object BytesSerializer {
  def shortBytesEnsureCapacity(i: Short): Array[Byte] = Bytes.ensureCapacity(Shorts.toByteArray(i), 2, 0)
  def intBytesEnsureCapacity(i: Int): Array[Byte] = Bytes.ensureCapacity(Ints.toByteArray(i), 4, 0)
  def longBytesEnsureCapacity(l: Long): Array[Byte] = Bytes.ensureCapacity(Longs.toByteArray(l), 8, 0)
  def arrayWithKnownSize(b: Array[Byte]): Array[Byte] = Shorts.toByteArray(b.length.toShort) ++ b
  def booleanToByte(b: Boolean): Byte = (if (b) 1 else 0).toByte
  def optionByteArrayToByteArray(a: Option[Array[Byte]]): Array[Byte] = a.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
}

case class SerializationError(cause: Throwable) extends RuntimeException(cause)
class SeqBytesSerializer[BS](implicit bs: Serializer[BS]) extends Serializer[Seq[BS]] {
  override def toBytes(seq: Seq[BS]): Array[Byte] = {
    val txsBytes = seq.map(bs.toBytes).foldLeft(Array.empty[Byte]) {
      case (obj, result) => Bytes.concat(result, obj)
    }
    Bytes.concat(Ints.toByteArray(seq.size), txsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Seq[BS]] = {
    ???
  }
}

object BytesDeserializer {
  type BytesArrayRead[N] = Try[(N, Array[Byte])]
  def shortBytes(bytes: Array[Byte]): BytesArrayRead[Int] = Try(Shorts.fromByteArray(bytes.take(2)), bytes.drop(2))
  def intBytes(bytes: Array[Byte]): BytesArrayRead[Int] = Try(Ints.fromByteArray(bytes.take(4)), bytes.drop(4))
  def longBytes(bytes: Array[Byte]): BytesArrayRead[Long] = Try(Longs.fromByteArray(bytes.take(8)), bytes.drop(8))
  def arrayWithSize(bytes: Array[Byte], size: Int): BytesArrayRead[Array[Byte]] = Try(bytes, bytes.take(size))
  def byte(bytes: Array[Byte]): BytesArrayRead[Byte] = Try(bytes.head, bytes.drop(1))
  def skip(bytes: Array[Byte], skip: Int): BytesArrayRead[Unit] = Try((), bytes.drop(skip))
  def arrayWithoutKnownSize(bytes: Array[Byte]): BytesArrayRead[Array[Byte]] = for {
    (size, array) <- shortBytes(bytes)
  } yield {
    (array.take(size), array.drop(size))
  }
  def booleanFromByte(bytes: Array[Byte]): BytesArrayRead[Boolean] = Try(bytes.head != 0, bytes.drop(1))
}

case class DeserializationError(cause: Throwable) extends RuntimeException(cause)
