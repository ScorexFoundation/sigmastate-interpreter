package sigmastate.utils

import java.nio.ByteBuffer

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.serialization.generators.ValueGenerators
import sigmastate.utils.ByteArrayWriter.{encodeZigZagInt, encodeZigZagLong}
import sigmastate.utils.ByteBufferReader.{decodeZigZagInt, decodeZigZagLong}
import sigmastate.utils.Helpers.bytesFromInts

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
        arrayGen[Byte],
        arrayGen[Boolean]))
  } yield anyValSeq

  // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L239
  private val expectedValues: Seq[(Array[Byte], Long)] = Seq(
    (bytesFromInts(0x00), 0),
    (bytesFromInts(0x01), 1),
    (bytesFromInts(0x7f), 127),
    // 14882
    (bytesFromInts(0xa2, 0x74), (0x22 << 0) | (0x74 << 7)),
    // 2961488830
    (bytesFromInts(0xbe, 0xf7, 0x92, 0x84, 0x0b),
      (0x3e << 0) | (0x77 << 7) | (0x12 << 14) | (0x04 << 21) | (0x0bL << 28)),
    // 64-bit
    // 7256456126
    (bytesFromInts(0xbe, 0xf7, 0x92, 0x84, 0x1b),
      (0x3e << 0) | (0x77 << 7) | (0x12 << 14) | (0x04 << 21) | (0x1bL << 28)),
    // 41256202580718336
    (bytesFromInts(0x80, 0xe6, 0xeb, 0x9c, 0xc3, 0xc9, 0xa4, 0x49),
      (0x00 << 0) | (0x66 << 7) | (0x6b << 14) | (0x1c << 21) | (0x43L << 28) | (0x49L << 35) | (0x24L << 42) | (0x49L << 49)),
    // 11964378330978735131 (-6482365742730816485)
    (bytesFromInts(0x9b, 0xa8, 0xf9, 0xc2, 0xbb, 0xd6, 0x80, 0x85, 0xa6, 0x01),
      (0x1b << 0) | (0x28 << 7) | (0x79 << 14) | (0x42 << 21) | (0x3bL << 28) | (0x56L << 35) | (0x00L << 42) | (0x05L << 49) | (0x26L << 56) | (0x01L << 63))
  )

  private def byteBufReader(bytes: Array[Byte]): ByteBufferReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(0)
    new ByteBufferReader(buf)
  }

  private def byteArrayWriter(): ByteArrayWriter = new ByteArrayWriter(new ByteArrayBuilder())

  property("predefined long values and serialized data round trip") {
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
    // increase threshold to make sure we cover a lot of types combination
    // and a good diversity withing a values of the each type
    forAll(seqPrimValGen, minSuccessful(500)) { values: Seq[Any] =>
      val writer = byteArrayWriter()
      for(any <- values) {
        any match {
          case v: Byte => writer.put(v)
          case v: Short =>
            writer.putShort(v)
            if (v >= 0) writer.putUShort(v)
          case v: Int =>
            writer.putInt(v)
            if (v >= 0) writer.putUInt(v)
          case v: Long =>
            // test all paths
            writer.putLong(v)
            writer.putULong(v)
          case v: Array[Byte] => writer.putUShort(v.length.toShort).putBytes(v)
          case v: Array[Boolean] => writer.putUShort(v.length.toShort).putBits(v)
          case _ => fail(s"writer: unsupported value type: ${any.getClass}");
        }
      }
      val reader = byteBufReader(writer.toBytes)
      values.foreach {
        case v: Byte => reader.getByte() shouldEqual v
        case v: Short =>
          // test all paths
          reader.getShort() shouldEqual v
          if (v >= 0) reader.getUShort().toShort shouldEqual v
        case v: Int =>
          reader.getInt() shouldEqual v
          if (v >= 0) reader.getUInt() shouldEqual v
        case v: Long =>
          // test all paths
          reader.getLong() shouldEqual v
          reader.getULong() shouldEqual v
        case v: Array[Byte] =>
          val size = reader.getUShort()
          reader.getBytes(size) shouldEqual v
        case v: Array[Boolean] =>
          val size = reader.getUShort()
          reader.getBits(size) shouldEqual v
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
    checkSize(Long.MinValue, 0, 10)
  }

  property("fail deserialization by deliberately messing with different methods") {
    forAll(Gen.chooseNum(1, Long.MaxValue)) { v: Long =>
      val writer = byteArrayWriter()
      writer.putULong(v)
      writer.putLong(v)
      val reader = byteBufReader(writer.toBytes)
      reader.getLong() should not be v
      reader.getULong() should not be v
    }
  }

  property("malformed input for deserialization") {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L281
    assertThrows[RuntimeException](byteBufReader(bytesFromInts(0x80)).getULong())
    assertThrows[RuntimeException](byteBufReader(bytesFromInts(0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00)).getULong())
  }

  property("ZigZag encoding format") {
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
  }

  property("ZigZag Long round trip") {
    forAll(Gen.chooseNum(Long.MinValue, Long.MaxValue)) { v: Long =>
      decodeZigZagLong(encodeZigZagLong(v)) shouldBe v
    }
  }

  property("ZigZag Int round trip") {
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { v: Int =>
      decodeZigZagInt(encodeZigZagInt(v)) shouldBe v
    }
  }

  property("Array[Boolean] bit encoding format") {
    val expectations = Seq[(Array[Boolean], Array[Byte])](
      Array[Boolean]() -> Array[Byte](),
      Array(false) -> Array(0),
      Array(true) -> Array(1),
      Array(false, false, true) -> Array(4), // 00000100
      Array(true, true, false) -> Array(3), // 00000011
      Array(true, false, true) -> Array(5), // 00000101
      (Array.fill(8)(false) :+ true) -> Array(0, 1), // 00000000 00000001
      (Array.fill(9)(false) :+ true) -> Array(0, 2), // 00000000 00000010
      (Array.fill(10)(false) :+ true) -> Array(0, 4) // 00000000 00000100
    )
    expectations.foreach { case (bools, bytes) =>
        byteArrayWriter().putBits(bools).toBytes shouldEqual bytes
        byteBufReader(bytes).getBits(bools.length) shouldEqual bools
    }
  }

  property("putUByte range check assertion") {
    val w = byteArrayWriter()
    w.putUByte(0)
    w.putUByte(255)
    an[AssertionError] should be thrownBy w.putUByte(-1)
    an[AssertionError] should be thrownBy w.putUByte(256)
  }

  property("putUShort range check assertion") {
    val w = byteArrayWriter()
    w.putUShort(0)
    w.putUShort(0xFFFF)
    an[AssertionError] should be thrownBy w.putUShort(-1)
    an[AssertionError] should be thrownBy w.putUShort(0xFFFF + 1)
  }

  property("putUInt range check assertion") {
    val w = byteArrayWriter()
    w.putUInt(0)
    w.putUInt(0xFFFFFFFFL)
    an[AssertionError] should be thrownBy w.putUInt(-1)
    an[AssertionError] should be thrownBy w.putUInt(0xFFFFFFFFL + 1)
  }

  property("getUShort range check assertion") {
    def check(in: Int): Unit =
      byteBufReader(byteArrayWriter().putUInt(in).toBytes).getUShort() shouldBe in

    def checkFail(in: Int): Unit =
      an[AssertionError] should be thrownBy
        byteBufReader(byteArrayWriter().putUInt(in).toBytes).getUShort()

    check(0)
    check(0xFFFF)
    checkFail(-1)
    checkFail(0xFFFF + 1)
    checkFail(Int.MaxValue)
  }

  property("getUInt range check assertion") {
    def check(in: Long): Unit =
      byteBufReader(byteArrayWriter().putULong(in).toBytes).getUInt() shouldBe in

    def checkFail(in: Long): Unit =
      an[AssertionError] should be thrownBy
        byteBufReader(byteArrayWriter().putULong(in).toBytes).getUInt()

    check(0)
    check(0xFFFFFFFFL)
    checkFail(-1)
    checkFail(0xFFFFFFFFL + 1L)
    checkFail(Long.MaxValue)
  }
}
