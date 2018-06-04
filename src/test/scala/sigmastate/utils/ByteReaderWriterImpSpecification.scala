package sigmastate.utils

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
}
