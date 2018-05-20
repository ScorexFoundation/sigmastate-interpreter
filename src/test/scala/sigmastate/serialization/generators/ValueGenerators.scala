package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate.{AvlTreeData, SBoolean, SGroupElement, SType}
import sigmastate.Values._
import sigmastate.interpreter.GroupSettings
import sigmastate.utxo.ErgoBox
import sigmastate.utxo.ErgoBox._
import sigmastate.{AvlTreeData, SType, SByte}

import scala.collection.JavaConverters._

trait ValueGenerators {

  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbByteArrayConstant: Arbitrary[CollectionConstant[SByte.type]] = Arbitrary(byteArrayConstGen)
  implicit val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstGen)
  implicit val arbBoxConstant: Arbitrary[BoxConstant] = Arbitrary(boxConstantGen)
  implicit val arbAvlTreeConstant: Arbitrary[AvlTreeConstant] = Arbitrary(avlTreeConstantGen)
  implicit val arbBigIntConstant: Arbitrary[BigIntConstant] = Arbitrary(bigIntConstGen)

  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedIntGen)
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedBoxGen)

  implicit val arbProveDlog: Arbitrary[ProveDlog] = Arbitrary(proveDlogGen)
  implicit val arbProveDHT: Arbitrary[ProveDiffieHellmanTuple] = Arbitrary(proveDHTGen)
  implicit val arbRegisterIdentifier: Arbitrary[RegisterIdentifier] = Arbitrary(registerIdentifierGen)
  implicit val arbAvlTree: Arbitrary[TaggedAvlTree] = Arbitrary(taggedAvlTreeGen)
  implicit val arbBox: Arbitrary[ErgoBox] = Arbitrary(ergoBoxGen)



  val booleanGen: Gen[Value[SBoolean.type]] = Gen.oneOf(TrueLeaf, FalseLeaf)
  val intConstGen: Gen[IntConstant] = arbLong.arbitrary.map { v => IntConstant(v) }
  val bigIntConstGen: Gen[BigIntConstant] = arbBigInt.arbitrary.map { v => BigIntConstant(v.bigInteger) }
  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)
  val groupElementConstGen: Gen[GroupElementConstant] = for {
    _ <- Gen.const(1)
    el = GroupSettings.dlogGroup.createRandomGenerator()
  } yield GroupElementConstant(el)

  val taggedIntGen: Gen[TaggedInt] = arbByte.arbitrary.map { v => TaggedInt(v) }
  val taggedBoxGen: Gen[TaggedBox] = arbByte.arbitrary.map { v => TaggedBox(v) }

  val proveDlogGen: Gen[ProveDlog] = arbGroupElementConstant.arbitrary.map(v => ProveDlog(v))
  val proveDHTGen: Gen[ProveDiffieHellmanTuple] = for {
    gv: Value[SGroupElement.type] <- groupElementConstGen
    hv: Value[SGroupElement.type] <- groupElementConstGen
    uv: Value[SGroupElement.type] <- groupElementConstGen
    vv: Value[SGroupElement.type] <- groupElementConstGen
  } yield ProveDiffieHellmanTuple(gv, hv, uv, vv)

  val registerIdentifierGen: Gen[RegisterIdentifier] = Gen.oneOf(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9)

  val taggedAvlTreeGen: Gen[TaggedAvlTree] = arbByte.arbitrary.map { v => TaggedAvlTree(v) }

  def additionalRegistersGen(cnt: Byte): Seq[Gen[(NonMandatoryIdentifier, EvaluatedValue[SType])]] = {
    (0 until cnt)
      .map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI.toByte).asInstanceOf[NonMandatoryIdentifier])
      .map { r =>
        for {
          arr <- byteArrayConstGen
          v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
        }
        yield r -> v.asInstanceOf[EvaluatedValue[SType]]
      }
  }

  val ergoBoxGen: Gen[ErgoBox] = for {
    l <- arbLong.arbitrary
    p <- proveDlogGen
    b <- Gen.oneOf(TrueLeaf, FalseLeaf, p)
    tId <- Gen.listOfN(32, arbByte.arbitrary)
    boxId <- arbShort.arbitrary
    regNum <- Gen.chooseNum[Byte](0, 7)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
  } yield ErgoBox(l, b, ar.asScala.toMap, Digest32 @@ tId.toArray, boxId)

  val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map { v => BoxConstant(v) }

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

  def avlTreeConstantGen: Gen[AvlTreeConstant] = avlTreeDataGen.map { v => AvlTreeConstant(v) }
}
