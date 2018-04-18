package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import sigmastate.Values._

trait ValueGenerators {

  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedIntGen)
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedBoxGen)
  implicit val arbByteArrayConstant: Arbitrary[ByteArrayConstant] = Arbitrary(byteArrayConstantGen)
  implicit val arbBigIntConstant: Arbitrary[BigIntConstant] = Arbitrary(bigIntConstantGen)

  val intConstGen: Gen[IntConstant] = arbLong.arbitrary.map{v => IntConstant(v)}
  val taggedIntGen: Gen[TaggedInt] = arbByte.arbitrary.map{v => TaggedInt(v)}
  val taggedBoxGen: Gen[TaggedBox] = arbByte.arbitrary.map{v => TaggedBox(v)}
  val byteArrayConstantGen: Gen[ByteArrayConstant] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)
  val bigIntConstantGen: Gen[BigIntConstant] = arbBigInt.arbitrary.map {v => BigIntConstant(v.bigInteger)}

}
