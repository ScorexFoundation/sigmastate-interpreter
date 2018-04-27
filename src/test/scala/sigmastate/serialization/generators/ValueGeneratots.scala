package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate.{AvlTreeData, SType}
import sigmastate.Values._
import sigmastate.utxo.ErgoBox
import sigmastate.utxo.ErgoBox._

import collection.JavaConverters._

trait ValueGeneratots {

  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedIntGen)
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedBoxGen)
  implicit val arbByteArrayConstant: Arbitrary[ByteArrayConstant] = Arbitrary(byteArrayConstantGen)
  implicit val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstantGen)
  implicit val arbProveDlog: Arbitrary[ProveDlog] = Arbitrary(proveDlogGen)
  implicit val arbRegisterIdentifier: Arbitrary[RegisterIdentifier] = Arbitrary(registerIdentifierGen)
  implicit val arbAvlTree: Arbitrary[TaggedAvlTree] = Arbitrary(taggedAvlTreeGen)
  implicit val arbBox: Arbitrary[ErgoBox] = Arbitrary(ergoBoxGen)
  implicit val arbBoxConstants: Arbitrary[BoxConstant] = Arbitrary(boxConstantGen)
  implicit val arbAvlTreeConstant: Arbitrary[AvlTreeConstant] = Arbitrary(avlTreeConstantGen)


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

  def arGen(cnt: Byte): Seq[Gen[(NonMandatoryIdentifier, Value[SType])]] = {
    (0 until cnt).map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI.toByte).asInstanceOf[NonMandatoryIdentifier])
      .map(r => Gen.oneOf(TrueLeaf, FalseLeaf).map(v => r -> v))
  }

  val ergoBoxGen: Gen[ErgoBox] = for {
    l <- arbLong.arbitrary
    p <- proveDlogGen
    b <- Gen.oneOf(TrueLeaf, FalseLeaf, p)
    tId <- Gen.listOfN(32, arbByte.arbitrary)
    boxId <- arbShort.arbitrary
    regNum <- Gen.chooseNum[Byte](0, 7)
    ar <- Gen.sequence(arGen(regNum))
  } yield ErgoBox(l, b, ar.asScala.toMap, Digest32 @@ tId.toArray, boxId)

  val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map{v => BoxConstant(v)}

  val smallIntGen: Gen[Int] = Gen.chooseNum(2, 16)
  val smallIntOptGen: Gen[Option[Int]] = for {
    int <- smallIntGen
    opt <- Gen.oneOf(Some(int), None)
  } yield opt


  def avlTreeDataGen: Gen[AvlTreeData] = for {
    digest <- Gen.listOfN(32, arbByte.arbitrary).map(_.toArray)
    keyLength <- smallIntGen
    vl <- smallIntOptGen
    mn <- arbOption[Int].arbitrary
    md <- arbOption[Int].arbitrary
  } yield AvlTreeData(ADDigest @@ digest, keyLength, vl, mn, md)

  def avlTreeConstantGen: Gen[AvlTreeConstant] = avlTreeDataGen.map{v => AvlTreeConstant(v)}
}
