package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.Values._
import sigmastate.utxo.ErgoBox._

trait ValueGeneratots {

  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedIntGen)
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedBoxGen)
  implicit val arbByteArrayConstant: Arbitrary[ByteArrayConstant] = Arbitrary(byteArrayConstantGen)
  implicit val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstantGen)
  implicit val arbProveDlog: Arbitrary[ProveDlog] = Arbitrary(proveDlogGen)
  implicit val arbRegisterIdentifier: Arbitrary[RegisterIdentifier] = Arbitrary(registerIdentifierGen)
  implicit val arbAvlTree: Arbitrary[TaggedAvlTree] = Arbitrary(taggedAvlTreeGen)

  val intConstGen: Gen[IntConstant] = arbLong.arbitrary.map{v => IntConstant(v)}
  val taggedIntGen: Gen[TaggedInt] = arbByte.arbitrary.map{v => TaggedInt(v)}
  val taggedBoxGen: Gen[TaggedBox] = arbByte.arbitrary.map{v => TaggedBox(v)}
  val byteArrayConstantGen: Gen[ByteArrayConstant] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)
  val groupElementConstantGen: Gen[GroupElementConstant] = for {
    _ <- Gen.const(1)
    el = scapi.sigma.Curve25519.createRandomGenerator()
  } yield GroupElementConstant(el)

  val proveDlogGen: Gen[ProveDlog] = arbGroupElementConstant.arbitrary.map(v => ProveDlog(v))

  val registerIdentifierGen: Gen[RegisterIdentifier] = Gen.oneOf(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9)

  val taggedAvlTreeGen: Gen[TaggedAvlTree] = arbByte.arbitrary.map{v => TaggedAvlTree(v)}

}
