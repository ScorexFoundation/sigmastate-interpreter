package sigmastate.utils

import java.nio.ByteBuffer

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.serialization.Serializer
import sigmastate.serialization.generators.ValueGenerators

class ByteReaderWriterImpSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with Matchers {

  private val seqPrimValGen: Gen[Seq[Any]] = for {
    length <- Gen.chooseNum(1, 100)
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
    * Borrowed from http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L133
    * Helper to construct a byte array from a bunch of bytes. The inputs are actually ints so that I
    * can use hex notation and not get stupid errors about precision.
    */
  private def bytes(bytesAsInts: Int*): Array[Byte] = bytesAsInts.map(_.toByte).toArray

  // Borrowed from http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/test/java/com/google/protobuf/CodedInputStreamTest.java#L239
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
    // 11964378330978735131
    (bytes(0x9b, 0xa8, 0xf9, 0xc2, 0xbb, 0xd6, 0x80, 0x85, 0xa6, 0x01),
      (0x1b << 0) | (0x28 << 7) | (0x79 << 14) | (0x42 << 21) | (0x3bL << 28) | (0x56L << 35) | (0x00L << 42) | (0x05L << 49) | (0x26L << 56) | (0x01L << 63))
  )

  property("round trip serialization/deserialization of arbitrary value list") {
    forAll { values: Seq[Any] =>
      val writer = Serializer.startWriter()
      for(any <- values) {
        any match {
          case v: Byte => writer.put(v)
          case v: Short => writer.putShort(v)
          case v: Int => writer.putInt(v)
          case v: Long => writer.putLong(v)
          case v: Array[Byte] => writer.putInt(v.length).putBytes(v)
          case _ => fail(s"writer: unsupported value type: ${any.getClass}");
        }
      }
      val reader = Serializer.startReader(writer.toBytes, 0)
      values.foreach {
        case v: Byte => reader.getByte() shouldEqual v
        case v: Short => reader.getShort() shouldEqual v
        case v: Int => reader.getInt() shouldEqual v
        case v: Long => reader.getLong() shouldEqual v
        case v: Array[Byte] =>
          val size = reader.getInt()
          reader.getBytes(size) shouldEqual v
        case ref@_ => fail(s"reader: unsupported value type: ${ref.getClass}");
      }
    }
  }

  property("predefined values and serialized data round trip") {
    expectedValues.foreach { case (bytes, v) =>
      val writer = new ByteArrayWriter(new ByteArrayBuilder())
      writer.putULong(v)
      val encodedBytes = writer.toBytes
      encodedBytes shouldEqual bytes

      val buf = ByteBuffer.wrap(encodedBytes)
      buf.position(0)
      val reader = new ByteBufferReader(buf)
      reader.getULong() shouldEqual v
      reader.remaining shouldBe 0
    }
  }
}
