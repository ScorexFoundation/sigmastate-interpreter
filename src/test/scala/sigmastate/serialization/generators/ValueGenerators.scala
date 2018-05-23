package sigmastate.serialization.generators

import org.ergoplatform
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTransaction}
import org.ergoplatform.ErgoBox._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.CryptoConstants

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait ValueGenerators extends TypeGenerators {

  implicit val arbByteConstants: Arbitrary[ByteConstant] = Arbitrary(byteConstGen)
  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbByteArrayConstant: Arbitrary[CollectionConstant[SByte.type]] = Arbitrary(byteArrayConstGen)
  implicit val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstGen)
  implicit val arbBoxConstant: Arbitrary[BoxConstant] = Arbitrary(boxConstantGen)
  implicit val arbAvlTreeConstant: Arbitrary[AvlTreeConstant] = Arbitrary(avlTreeConstantGen)
  implicit val arbBigIntConstant: Arbitrary[BigIntConstant] = Arbitrary(bigIntConstGen)

  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedVar[SInt.type])
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedVar[SBox.type])
  implicit val arbTaggedAvlTree: Arbitrary[TaggedAvlTree] = Arbitrary(taggedAvlTreeGen)

  implicit val arbProveDlog: Arbitrary[ProveDlog] = Arbitrary(proveDlogGen)
  implicit val arbProveDHT: Arbitrary[ProveDiffieHellmanTuple] = Arbitrary(proveDHTGen)
  implicit val arbRegisterIdentifier: Arbitrary[RegisterIdentifier] = Arbitrary(registerIdentifierGen)


  implicit val arbBigInteger   = Arbitrary(arbBigInt.arbitrary.map(_.bigInteger))
  implicit val arbGroupElement = Arbitrary(Gen.const(()).flatMap(_ => CryptoConstants.dlogGroup.createRandomGenerator()))
  implicit val arbBox          = Arbitrary(ergoBoxGen)
  implicit val arbAvlTreeData  = Arbitrary(avlTreeDataGen)
  implicit val arbBoxCandidate = Arbitrary(ergoBoxCandidateGen)

  val byteConstGen: Gen[ByteConstant] = arbByte.arbitrary.map { v => ByteConstant(v) }
  val booleanConstGen: Gen[Value[SBoolean.type]] = Gen.oneOf(TrueLeaf, FalseLeaf)
  val intConstGen: Gen[IntConstant] = arbLong.arbitrary.map { v => IntConstant(v) }
  val bigIntConstGen: Gen[BigIntConstant] = arbBigInt.arbitrary.map { v => BigIntConstant(v.bigInteger) }
  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)
  val groupElementConstGen: Gen[GroupElementConstant] = for {
    _ <- Gen.const(1)
    el = CryptoConstants.dlogGroup.createRandomGenerator()
  } yield GroupElementConstant(el)

  def taggedVar[T <: SType](implicit aT: Arbitrary[T]): Gen[TaggedVariable[T]] = for {
    t <- aT.arbitrary
    id <- arbByte.arbitrary
  } yield TaggedVariable(id, t)


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
  } yield ergoplatform.ErgoBox(l, b, ar.asScala.toMap, Digest32 @@ tId.toArray, boxId)

  val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    l <- arbLong.arbitrary
    p <- proveDlogGen
    b <- Gen.oneOf(TrueLeaf, FalseLeaf, p)
    regNum <- Gen.chooseNum[Byte](0, 7)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
  } yield new ErgoBoxCandidate(l, b, ar.asScala.toMap)

  val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map { v => BoxConstant(v) }

  val smallIntGen: Gen[Int] = Gen.chooseNum(2, 16)
  val smallIntOptGen: Gen[Option[Int]] = for {
    int <- smallIntGen
    opt <- Gen.oneOf(Some(int), None)
  } yield opt

  // todo implement (make sure empty seq is covered)
//  val inputGen: Gen[Input] = ???

  val ergoTransactionGen: Gen[ErgoTransaction] = for {
//    inputs <- Gen.listOf(inputGen)
    outputCandidates <- Gen.listOf(ergoBoxCandidateGen)
  } yield ErgoTransaction(IndexedSeq(), outputCandidates.toIndexedSeq)

  def avlTreeDataGen: Gen[AvlTreeData] = for {
    digest <- Gen.listOfN(32, arbByte.arbitrary).map(_.toArray)
    keyLength <- smallIntGen
    vl <- smallIntOptGen
    mn <- arbOption[Int].arbitrary
    md <- arbOption[Int].arbitrary
  } yield AvlTreeData(ADDigest @@ digest, keyLength, vl, mn, md)

  def avlTreeConstantGen: Gen[AvlTreeConstant] = avlTreeDataGen.map { v => AvlTreeConstant(v) }

  implicit def arrayGen[T: Gen: ClassTag]: Gen[Array[T]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbitrary[T])
  } yield bytes.toArray

  def wrappedTypeGen[T <: SType](tpe: T): Gen[T#WrappedType] = (tpe match {
    case SByte => arbByte
    case SInt => arbLong
    case SBoolean => arbBool
    case SBigInt => arbBigInteger
    case SAvlTree => arbAvlTreeData
    case SGroupElement => arbGroupElement
    case SBox => arbBox
  }).asInstanceOf[Arbitrary[T#WrappedType]].arbitrary
}
