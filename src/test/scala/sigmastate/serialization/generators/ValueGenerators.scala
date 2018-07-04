package sigmastate.serialization.generators

import org.ergoplatform
import org.ergoplatform._
import org.ergoplatform.ErgoBox._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.authds.{ADDigest, ADKey}
import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.{ContextExtension, CryptoConstants, ProverResult}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait ValueGenerators extends TypeGenerators {

  implicit val arbByteConstants: Arbitrary[ByteConstant] = Arbitrary(byteConstGen)
  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbLongConstants: Arbitrary[LongConstant] = Arbitrary(longConstGen)
  implicit val arbByteArrayConstant: Arbitrary[CollectionConstant[SByte.type]] = Arbitrary(byteArrayConstGen)
  implicit val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstGen)
  implicit val arbBoxConstant: Arbitrary[BoxConstant] = Arbitrary(boxConstantGen)
  implicit val arbAvlTreeConstant: Arbitrary[AvlTreeConstant] = Arbitrary(avlTreeConstantGen)
  implicit val arbBigIntConstant: Arbitrary[BigIntConstant] = Arbitrary(bigIntConstGen)

  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedVar[SInt.type])
  implicit val arbTaggedLong: Arbitrary[TaggedLong] = Arbitrary(taggedVar[SLong.type])
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
  implicit val arbTransaction  = Arbitrary(ergoTransactionGen)
  implicit val arbContextExtension = Arbitrary(contextExtensionGen)
  implicit val arbSerializedProverResult = Arbitrary(serializedProverResultGen)
  implicit val arbUnsignedInput = Arbitrary(unsignedInputGen)
  implicit val arbInput         = Arbitrary(inputGen)

  val byteConstGen: Gen[ByteConstant] = arbByte.arbitrary.map { v => ByteConstant(v) }
  val booleanConstGen: Gen[Value[SBoolean.type]] = Gen.oneOf(TrueLeaf, FalseLeaf)
  val intConstGen: Gen[IntConstant] = arbInt.arbitrary.map { v => IntConstant(v) }
  val longConstGen: Gen[LongConstant] = arbLong.arbitrary.map { v => LongConstant(v) }
  val bigIntConstGen: Gen[BigIntConstant] = arbBigInt.arbitrary.map { v => BigIntConstant(v.bigInteger) }
  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield ByteArrayConstant(bytes.toArray)
  val intArrayConstGen: Gen[CollectionConstant[SInt.type]] = for {
    length <- Gen.chooseNum(1, 100)
    ints <- Gen.listOfN(length, arbInt.arbitrary)
  } yield IntArrayConstant(ints.toArray)
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
  } yield ergoplatform.ErgoBox(l, b, ar.asScala.toMap, tId.toArray, boxId)

  val ergoBoxCandidateGen: Gen[ErgoBoxCandidate] = for {
    l <- arbLong.arbitrary
    p <- proveDlogGen
    b <- Gen.oneOf(TrueLeaf, FalseLeaf, p)
    regNum <- Gen.chooseNum[Byte](0, 7)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
  } yield new ErgoBoxCandidate(l, b, ar.asScala.toMap)

  val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map { v => BoxConstant(v) }

  val smallIntGe n: Gen[Int] = Gen.chooseNum(2, 16)
  val smallIntOptGen: Gen[Option[Int]] = for {
    int <- smallIntGen
    opt <- Gen.oneOf(Some(int), None)
  } yield opt

  val contextExtensionGen: Gen[ContextExtension] = for {
    values <- Gen.sequence(contextExtensionValuesGen(0, 3))
  } yield ContextExtension(values.asScala.toMap)

  val serializedProverResultGen: Gen[ProverResult] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
    contextExt <- contextExtensionGen
  } yield ProverResult(bytes.toArray, contextExt)

  val boxIdGen: Gen[BoxId] = for {
    bytes <- Gen.listOfN(BoxId.size, arbByte.arbitrary)
  } yield ADKey @@ bytes.toArray

  val unsignedInputGen: Gen[UnsignedInput] = for {
    boxId <- boxIdGen
  } yield new UnsignedInput(boxId)

  val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    proof <- serializedProverResultGen
  } yield Input(boxId, proof)

  val ergoTransactionGen: Gen[ErgoLikeTransaction] = for {
    inputs <- Gen.listOf(inputGen)
    outputCandidates <- Gen.listOf(ergoBoxCandidateGen)
  } yield ErgoLikeTransaction(inputs.toIndexedSeq, outputCandidates.toIndexedSeq)

  def contextExtensionValuesGen(min: Int, max: Int): Seq[Gen[(Byte, EvaluatedValue[SType])]] =
    (0 until Gen.chooseNum(min, max).sample.get)
      .map(_ + 1)
      .map { varId =>
        for {
          arr <- byteArrayConstGen
          v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
        }
          yield varId.toByte -> v.asInstanceOf[EvaluatedValue[SType]]
      }

  def avlTreeDataGen: Gen[AvlTreeData] = for {
    digest <- Gen.listOfN(32, arbByte.arbitrary).map(_.toArray)
    keyLength <- smallIntGen
    vl <- smallIntOptGen
    mn <- arbOption[Int].arbitrary
    md <- arbOption[Int].arbitrary
  } yield AvlTreeData(ADDigest @@ digest, keyLength, vl, mn, md)

  def avlTreeConstantGen: Gen[AvlTreeConstant] = avlTreeDataGen.map { v => AvlTreeConstant(v) }

  implicit def arrayGen[T: Arbitrary: ClassTag]: Gen[Array[T]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbitrary[T])
  } yield bytes.toArray

  def wrappedTypeGen[T <: SType](tpe: T): Gen[T#WrappedType] = (tpe match {
    case SBoolean => arbBool
    case SByte => arbByte
    case SShort => arbShort
    case SInt => arbInt
    case SLong => arbLong
    case SBigInt => arbBigInteger
    case SGroupElement => arbGroupElement
    case SBox => arbBox
    case SAvlTree => arbAvlTreeData
    case SAny => arbAnyVal
    case SUnit => arbUnit
  }).asInstanceOf[Arbitrary[T#WrappedType]].arbitrary
}
