package sigmastate.utils

import java.nio.ByteBuffer

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.serialization.generators.ValueGenerators
import sigmastate.utils.ByteArrayWriter.{encodeZigZagInt, encodeZigZagLong}
import sigmastate.utils.ByteBufferReader.{decodeZigZagInt, decodeZigZagLong}

class ByteReaderWriterImpSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with Matchers {

  private val seqPrimValGen: Gen[Seq[Any]] = for {
    length <- Gen.chooseNum(1, 1000)
    anyValSeq <- Gen.listOfN(length,
      Gen.oneOf(
        Arbitrary.arbByte.arbitrary,
        Arbitrary.arbShort.arbitrary,
        Arbitrary.arbInt.arbitrary,
        Arbitrary.arbLong.arbitrary,
        arrayGen[Byte]))
  } yield anyValSeq

  private implicit val arbSeqPrimVal: Arbitrary[Seq[Any]] = Arbitrary(seqPrimValGen)

  /**
    * source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L133
    * Helper to construct a byte array from a bunch of bytes. The inputs are actually ints so that I
    * can use hex notation and not get stupid errors about precision.
    */
  private def bytes(bytesAsInts: Int*): Array[Byte] = bytesAsInts.map(_.toByte).toArray

  // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L239
  private val expectedValues: Seq[(Array[Byte], Long)] = Seq(
    (bytes(0x00), 0),
    (bytes(0x01), 1),
    (bytes(0x7f), 127),
    // 14882
    (bytes(0xa2, 0x74), (0x22 << 0) | (0x74 << 7)),
    // 2961488830
    (bytes(0xbe, 0xf7, 0x92, 0x84, 0x0b),
      (0x3e << 0) | (0x77 << 7) | (0x12 << 14) | (0x04 << 21) | (0x0bL << 28)),
    // 64-bit
    // 7256456126
    (bytes(0xbe, 0xf7, 0x92, 0x84, 0x1b),
      (0x3e << 0) | (0x77 << 7) | (0x12 << 14) | (0x04 << 21) | (0x1bL << 28)),
    // 41256202580718336
    (bytes(0x80, 0xe6, 0xeb, 0x9c, 0xc3, 0xc9, 0xa4, 0x49),
      (0x00 << 0) | (0x66 << 7) | (0x6b << 14) | (0x1c << 21) | (0x43L << 28) | (0x49L << 35) | (0x24L << 42) | (0x49L << 49)),
    // 11964378330978735131 (-6482365742730816485)
    (bytes(0x9b, 0xa8, 0xf9, 0xc2, 0xbb, 0xd6, 0x80, 0x85, 0xa6, 0x01),
      (0x1b << 0) | (0x28 << 7) | (0x79 << 14) | (0x42 << 21) | (0x3bL << 28) | (0x56L << 35) | (0x00L << 42) | (0x05L << 49) | (0x26L << 56) | (0x01L << 63))
  )

  private def byteBufReader(bytes: Array[Byte]): ByteBufferReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(0)
    new ByteBufferReader(buf)
  }

  private def byteArrayWriter(): ByteArrayWriter = new ByteArrayWriter(new ByteArrayBuilder())

  property("predefined values and serialized data round trip") {
    expectedValues.foreach { case (bytes, v) =>
      val writer = byteArrayWriter()
      writer.putULong(v)
      val encodedBytes = writer.toBytes
      encodedBytes shouldEqual bytes

      val r = byteBufReader(encodedBytes)
      r.getULong() shouldEqual v
      r.remaining shouldBe 0
    }
  }

  property("round trip serialization/deserialization of arbitrary value list") {
    forAll { values: Seq[Any] =>
      val writer = byteArrayWriter()
      for(any <- values) {
        any match {
          case v: Byte => writer.put(v)
          case v: Short => writer.putShort(v)
          case v: Int => writer.putInt(v)
          case v: Long =>
            // test all paths
            writer.putLong(v)
            writer.putULong(v)
            writer.putSLong(v)
          case v: Array[Byte] => writer.putInt(v.length).putBytes(v)
          case _ => fail(s"writer: unsupported value type: ${any.getClass}");
        }
      }
      val reader = byteBufReader(writer.toBytes)
      values.foreach {
        case v: Byte => reader.getByte() shouldEqual v
        case v: Short => reader.getShort() shouldEqual v
        case v: Int => reader.getInt() shouldEqual v
        case v: Long =>
          // test all paths
          reader.getLong() shouldEqual v
          reader.getULong() shouldEqual v
          reader.getSLong() shouldEqual v
        case v: Array[Byte] =>
          val size = reader.getInt()
          reader.getBytes(size) shouldEqual v
        case ref@_ => fail(s"reader: unsupported value type: ${ref.getClass}");
      }
    }
  }

  private def checkSize(low: Long, high: Long, size: Int): Assertion = {
    forAll(Gen.choose(low, high)) { v: Long =>
      val writer = new ByteArrayWriter(new ByteArrayBuilder())
      writer.putULong(v)
      val encodedBytes = writer.toBytes
      encodedBytes.length shouldEqual size
    }
  }

  property("size of serialized data") {
    // source: http://github.com/scodec/scodec/blob/055eed8386aa85ff27dba3f72b104a8aa3d6012d/unitTests/src/test/scala/scodec/codecs/VarLongCodecTest.scala#L16
    checkSize(0L, 127L, 1)
    checkSize(128L, 16383L, 2)
    checkSize(16384L, 2097151L, 3)
    checkSize(2097152L, 268435455L, 4)
    checkSize(268435456L, 34359738367L, 5)
    checkSize(34359738368L, 4398046511103L, 6)
    checkSize(4398046511104L, 562949953421311L, 7)
    checkSize(562949953421312L, 72057594037927935L, 8)
    checkSize(72057594037927936L, Long.MaxValue, 9)
  }

  property("malformed input for deserialization") {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L281
    assertThrows[RuntimeException](byteBufReader(bytes(0x80)).getULong())
    assertThrows[RuntimeException](byteBufReader(bytes(0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00)).getULong())
  }

  property("ZigZag round trip") {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedOutputStreamTest.java#L281
    assert(0 == encodeZigZagInt(0))
    assert(1 == encodeZigZagInt(-1))
    assert(2 == encodeZigZagInt(1))
    assert(3 == encodeZigZagInt(-2))
    assert(0x7FFFFFFE == encodeZigZagInt(0x3FFFFFFF))
    assert(0x7FFFFFFF == encodeZigZagInt(0xC0000000))
    assert(0xFFFFFFFE == encodeZigZagInt(0x7FFFFFFF))
    assert(0xFFFFFFFF == encodeZigZagInt(0x80000000))

    assert(0 == encodeZigZagLong(0))
    assert(1 == encodeZigZagLong(-1))
    assert(2 == encodeZigZagLong(1))
    assert(3 == encodeZigZagLong(-2))
    assert(0x000000007FFFFFFEL == encodeZigZagLong(0x000000003FFFFFFFL))
    assert(0x000000007FFFFFFFL == encodeZigZagLong(0xFFFFFFFFC0000000L))
    assert(0x00000000FFFFFFFEL == encodeZigZagLong(0x000000007FFFFFFFL))
    assert(0x00000000FFFFFFFFL == encodeZigZagLong(0xFFFFFFFF80000000L))
    assert(0xFFFFFFFFFFFFFFFEL == encodeZigZagLong(0x7FFFFFFFFFFFFFFFL))
    assert(0xFFFFFFFFFFFFFFFFL == encodeZigZagLong(0x8000000000000000L))



    // Some easier-to-verify round-trip tests.  The inputs (other than 0, 1, -1)
    // were chosen semi-randomly via keyboard bashing.
    assert(0 == encodeZigZagInt(decodeZigZagInt(0)))
    assert(1 == encodeZigZagInt(decodeZigZagInt(1)))
    assert(-1 == encodeZigZagInt(decodeZigZagInt(-1)))
    assert(14927 == encodeZigZagInt(decodeZigZagInt(14927)))
    assert(-3612 == encodeZigZagInt(decodeZigZagInt(-3612)))

    assert(0 == encodeZigZagLong(decodeZigZagLong(0)))
    assert(1 == encodeZigZagLong(decodeZigZagLong(1)))
    assert(-1 == encodeZigZagLong(decodeZigZagLong(-1)))
    assert(14927 == encodeZigZagLong(decodeZigZagLong(14927)))
    assert(-3612 == encodeZigZagLong(decodeZigZagLong(-3612)))

    assert(856912304801416L ==
      encodeZigZagLong(decodeZigZagLong(856912304801416L)))
    assert(-75123905439571256L ==
      encodeZigZagLong(decodeZigZagLong(-75123905439571256L)))
  }
}
