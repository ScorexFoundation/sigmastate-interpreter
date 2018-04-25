package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.Values._

trait ValueGeneratots {

  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedIntGen)
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedBoxGen)
  implicit val arbByteArrayConstant: Arbitrary[ByteArrayConstant] = Arbitrary(byteArrayConstantGen)
  implicit val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstantGen)
  implicit val proveDlog: Arbitrary[ProveDlog] = Arbitrary(proveDlogGen)

  val intConstGen: Gen[IntConstant] = arbLong.arbitrary.map{v => IntConstant(v)}
  val taggedIntGen: Gen[TaggedInt] = arbByte.arbitrary.map{v => TaggedInt(v)}
  val taggedBoxGen: Gen[TaggedBox] = arbByte.arbitrary.map{v => TaggedBox(v)}
  val byteArrayConstantGen: Gen[ByteArrayConstant] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)
  val groupElementConstantGen: Gen[GroupElementConstant] = for {
    _ <- Gen.const(1)
    el = scapi.sigma.Curve25519.createRandomElement()
  } yield GroupElementConstant(el)

  val proveDlogGen: Gen[ProveDlog] = arbGroupElementConstant.arbitrary.map(v => ProveDlog(v))

}
