package sigmastate.serialization.generators

import org.ergoplatform
import org.ergoplatform._
import org.ergoplatform.ErgoBox._
import org.ergoplatform.ErgoScriptPredef.{FalseProp, TrueProp}
import org.ergoplatform.validation.ValidationSpecification
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util._
import sigmastate._
import sigmastate.Values._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{ProverResult, ContextExtension, CryptoConstants}
import sigmastate.eval._
import sigmastate.eval.Extensions._
import special.collection.Coll
import special.sigma._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

trait ValueGenerators extends TypeGenerators with ValidationSpecification {

  import sigmastate.lang.TransformingSigmaBuilder._

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
  implicit val arbProveDHT: Arbitrary[ProveDHTuple] = Arbitrary(proveDHTGen)
  implicit val arbRegisterIdentifier: Arbitrary[RegisterId] = Arbitrary(registerIdentifierGen)


  implicit val arbBigInteger = Arbitrary(Arbitrary.arbBigInt.arbitrary.map(_.bigInteger))
  implicit val arbBigInt = Arbitrary(arbBigInteger.arbitrary.map(SigmaDsl.BigInt(_)))
  implicit val arbEcPointType = Arbitrary(Gen.const(()).flatMap(_ => CryptoConstants.dlogGroup.createRandomGenerator()))
  implicit val arbGroupElement = Arbitrary(arbEcPointType.arbitrary.map(SigmaDsl.GroupElement(_)))
  implicit val arbSigmaBoolean: Arbitrary[SigmaBoolean] = Arbitrary(Gen.oneOf(proveDHTGen, proveDHTGen))
  implicit val arbSigmaProp: Arbitrary[SigmaProp] = Arbitrary(sigmaPropGen)
  implicit val arbSigmaPropValue: Arbitrary[SigmaPropValue] = Arbitrary(sigmaPropValueGen)
  implicit val arbErgoBox = Arbitrary(ergoBoxGen)
  implicit val arbBox = Arbitrary(ergoBoxGen.map(SigmaDsl.Box))
  implicit val arbAvlTreeData = Arbitrary(avlTreeDataGen)
  implicit val arbAvlTree = Arbitrary(avlTreeGen)
  implicit val arbBoxCandidate = Arbitrary(ergoBoxCandidateGen(tokensGen.sample.get))
  implicit val arbTransaction = Arbitrary(ergoTransactionGen)
  implicit val arbContextExtension = Arbitrary(contextExtensionGen)
  implicit val arbSerializedProverResult = Arbitrary(serializedProverResultGen)
  implicit val arbUnsignedInput = Arbitrary(unsignedInputGen)
  implicit val arbInput = Arbitrary(inputGen)

  val byteConstGen: Gen[ByteConstant] =
    arbByte.arbitrary.map { v => mkConstant[SByte.type](v, SByte) }
  val booleanConstGen: Gen[Value[SBoolean.type]] = Gen.oneOf(TrueLeaf, FalseLeaf)
  val shortConstGen: Gen[ShortConstant] =
    arbShort.arbitrary.map { v => mkConstant[SShort.type](v, SShort) }
  val intConstGen: Gen[IntConstant] =
    arbInt.arbitrary.map { v => mkConstant[SInt.type](v, SInt) }
  val longConstGen: Gen[LongConstant] =
    arbLong.arbitrary.map { v => mkConstant[SLong.type](v, SLong) }
  val stringConstGen: Gen[StringConstant] =
    arbString.arbitrary.map { v => mkConstant[SString.type](v, SString) }
  val bigIntConstGen: Gen[BigIntConstant] =
    arbBigInt.arbitrary.map { v => mkConstant[SBigInt.type](v, SBigInt) }
  val byteArrayConstGen: Gen[CollectionConstant[SByte.type]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield mkCollectionConstant[SByte.type](bytes.toArray, SByte)
  val intArrayConstGen: Gen[CollectionConstant[SInt.type]] = for {
    length <- Gen.chooseNum(1, 100)
    ints <- Gen.listOfN(length, arbInt.arbitrary)
  } yield mkCollectionConstant[SInt.type](ints.toArray, SInt)

  val groupElementGen: Gen[EcPointType] = for {
    _ <- Gen.const(1)
  } yield CryptoConstants.dlogGroup.createRandomElement()

  val groupElementConstGen: Gen[GroupElementConstant] = for {
    p <- groupElementGen
  } yield mkConstant[SGroupElement.type](p, SGroupElement)

  def taggedVar[T <: SType](implicit aT: Arbitrary[T]): Gen[TaggedVariable[T]] = for {
    t <- aT.arbitrary
    id <- arbByte.arbitrary
  } yield mkTaggedVariable(id, t)


  val proveDlogGen: Gen[ProveDlog] = for { v <- groupElementGen } yield ProveDlog(v)
  val proveDHTGen: Gen[ProveDHTuple] = for {
    gv <- groupElementGen
    hv <- groupElementGen
    uv <- groupElementGen
    vv <- groupElementGen
  } yield ProveDHTuple(gv, hv, uv, vv)

  def sigmaTreeNodeGen: Gen[SigmaBoolean] = for {
    left <- sigmaBooleanGen
    right <- sigmaBooleanGen
    node <- Gen.oneOf(
      COR(Seq(left, right)),
      CAND(Seq(left, right))
    )
  } yield node

  val sigmaBooleanGen: Gen[SigmaBoolean] = Gen.oneOf(proveDlogGen, proveDHTGen, Gen.delay(sigmaTreeNodeGen))
  val sigmaPropGen: Gen[SigmaProp] = sigmaBooleanGen.map(SigmaDsl.SigmaProp)
  val sigmaPropValueGen: Gen[SigmaPropValue] =
    Gen.oneOf(proveDlogGen.map(SigmaPropConstant(_)), proveDHTGen.map(SigmaPropConstant(_)))

  val registerIdentifierGen: Gen[RegisterId] = Gen.oneOf(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9)

  val taggedAvlTreeGen: Gen[TaggedAvlTree] =
    arbByte.arbitrary.map { v => TaggedAvlTree(v).asInstanceOf[TaggedAvlTree] }

  def additionalRegistersGen(cnt: Byte): Seq[Gen[(NonMandatoryRegisterId, EvaluatedValue[SType])]] = {
    (0 until cnt)
      .map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI.toByte).asInstanceOf[NonMandatoryRegisterId])
      .map { r =>
        for {
          arr <- byteArrayConstGen
          v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
        } yield r -> v.asInstanceOf[EvaluatedValue[SType]]
      }
  }

  def additionalTokensGen(cnt: Byte): Seq[Gen[(Digest32, Long)]] =
    (0 until cnt).map { _ =>
      for {
        id <- Digest32 @@ boxIdGen
        amt <- Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue)
      } yield id -> amt
    }

  val smallIntGen: Gen[Int] = Gen.chooseNum(2, 16)
  val smallIntOptGen: Gen[Option[Int]] = for {
    int <- smallIntGen
    opt <- Gen.oneOf(Some(int), None)
  } yield opt
  val unsignedIntGen: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)
  val unsignedShortGen: Gen[Short] = Gen.chooseNum(0, Short.MaxValue).map(_.toShort)

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
    contextExt <- contextExtensionGen
  } yield new UnsignedInput(boxId, contextExt)

  val dataInputGen: Gen[DataInput] = for {
    boxId <- boxIdGen
  } yield DataInput(boxId)

  val inputGen: Gen[Input] = for {
    boxId <- boxIdGen
    proof <- serializedProverResultGen
  } yield Input(boxId, proof)

  def contextExtensionValuesGen(min: Int, max: Int): Seq[Gen[(Byte, EvaluatedValue[SType])]] =
    (0 until Gen.chooseNum(min, max).sample.get)
      .map(_ + 1)
      .map { varId =>
        for {
          arr <- byteArrayConstGen
          longConst <- longConstGen
          v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr, longConst)
        }
          yield varId.toByte -> v.asInstanceOf[EvaluatedValue[SType]]
      }

  def avlTreeFlagsGen: Gen[AvlTreeFlags] = for {
    insert <- arbBool.arbitrary
    update <- arbBool.arbitrary
    remove <- arbBool.arbitrary
  } yield AvlTreeFlags(insert, update, remove)

  def avlTreeDataGen: Gen[AvlTreeData] = for {
    digest <- Gen.listOfN(AvlTreeData.DigestSize, arbByte.arbitrary).map(_.toArray)
    flags <- avlTreeFlagsGen
    keyLength <- unsignedIntGen
    vl <- arbOption[Int](Arbitrary(unsignedIntGen)).arbitrary
  } yield AvlTreeData(ADDigest @@ digest, flags, keyLength, vl)

  def avlTreeGen: Gen[AvlTree] = avlTreeDataGen.map(SigmaDsl.avlTree)

  def avlTreeConstantGen: Gen[AvlTreeConstant] = avlTreeGen.map { v => AvlTreeConstant(v) }

  implicit def arrayGen[T: Arbitrary : ClassTag]: Gen[Array[T]] = for {
    length <- Gen.chooseNum(1, 100)
    bytes <- Gen.listOfN(length, arbitrary[T])
  } yield bytes.toArray

  def wrappedTypeGen[T <: SType](tpe: T): Gen[T#WrappedType] = (tpe match {
    case SBoolean => arbBool
    case SByte => arbByte
    case SShort => arbShort
    case SInt => arbInt
    case SLong => arbLong
    case SBigInt => arbBigInt
    case SGroupElement => arbGroupElement
    case SSigmaProp => arbSigmaProp
    case SBox => arbBox
    case SAvlTree => arbAvlTree
    case SAny => arbAnyVal
    case SUnit => arbUnit
  }).asInstanceOf[Arbitrary[T#WrappedType]].arbitrary

  def tupleGen(min: Int, max: Int): Gen[Tuple] = for {
    length <- Gen.chooseNum(min, max)
    values <- Gen.listOfN(length, Gen.oneOf(
      byteArrayConstGen,
      byteConstGen,
      shortConstGen,
      intConstGen,
      longConstGen,
      booleanConstGen,
      bigIntConstGen,
      groupElementConstGen,
      taggedVar[SInt.type],
      taggedVar[SLong.type],
      taggedVar[SBox.type],
      taggedVar(Arbitrary(sTupleGen(2, 10)))
    ))
  } yield mkTuple(values).asInstanceOf[Tuple]

  val ergoBoxGen: Gen[ErgoBox] = for {
    l <- arbLong.arbitrary
    p <- proveDlogGen
    b <- Gen.oneOf(TrueProp, FalseProp, ErgoTree.fromSigmaBoolean(p))
    tId <- Gen.listOfN(32, arbByte.arbitrary)
    boxId <- unsignedShortGen
    tokensCount <- Gen.chooseNum[Byte](0, ErgoBox.MaxTokens)
    tokens <- Gen.sequence(additionalTokensGen(tokensCount))
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
    creationHeight <- Gen.choose(-1, Int.MaxValue)
  } yield ergoplatform.ErgoBox(l, b, creationHeight, tokens.asScala, ar.asScala.toMap, tId.toArray.toModifierId, boxId)

  def ergoBoxCandidateGen(availableTokens: Seq[Digest32]): Gen[ErgoBoxCandidate] = for {
    l <- arbLong.arbitrary
    p <- proveDlogGen
    b <- Gen.oneOf(TrueProp, FalseProp, ErgoTree.fromSigmaBoolean(p))
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
    tokensCount <- Gen.chooseNum[Byte](0, ErgoBox.MaxTokens)
    tokens <- Gen.listOfN(tokensCount, Gen.oneOf(availableTokens))
    tokenAmounts <- Gen.listOfN(tokensCount, Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue))
    creationHeight <- Gen.chooseNum(0, 100000)
  } yield new ErgoBoxCandidate(l, b, creationHeight, tokens.toColl.zip(tokenAmounts.toColl), ar.asScala.toMap)

  val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map { v => BoxConstant(CostingBox(false, v)) }

  val tokenIdGen: Gen[Digest32] = for {
    bytes <- Gen.listOfN(TokenId.size, arbByte.arbitrary).map(_.toArray)
  } yield Digest32 @@ bytes

  val tokensGen: Gen[Seq[Digest32]] = for {
    count <- Gen.chooseNum(10, 50)
    tokens <- Gen.listOfN(count, tokenIdGen)
  } yield tokens

  val ergoTransactionGen: Gen[ErgoLikeTransaction] = for {
    inputs <- Gen.nonEmptyListOf(inputGen)
    tokens <- tokensGen
    dataInputs <- Gen.listOf(dataInputGen)
    outputsCount <- Gen.chooseNum(50, 200)
    outputCandidates <- Gen.listOfN(outputsCount, ergoBoxCandidateGen(tokens))
  } yield new ErgoLikeTransaction(inputs.toIndexedSeq, dataInputs.toIndexedSeq, outputCandidates.toIndexedSeq)

  // distinct list of elements from a given generator
  // with a maximum number of elements to discard
  // source https://gist.github.com/etorreborre/d0616e704ed85d7276eb12b025df8ab0
  def distinctListOfGen[T](gen: Gen[T], maxDiscarded: Int = 1000): Gen[List[T]] = {
    val seen: ListBuffer[T] = new ListBuffer[T]
    var discarded = 0

    Gen.sized { size =>
      if (size == seen.size) seen.toList
      else {
        while (seen.size <= size && discarded < maxDiscarded)
          gen.sample match {
            case Some(t) if !seen.contains(t) =>
              seen.+=:(t)
            case _ => discarded += 1
          }
        seen.toList
      }
    }
  }
}
