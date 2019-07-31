package sigmastate.serialization.generators

import org.ergoplatform.ErgoBox._
import org.ergoplatform.ErgoConstants.MaxPropositionBytes
import org.ergoplatform.ErgoScriptPredef.{FalseProp, TrueProp}
import org.ergoplatform.validation._
import org.ergoplatform._
import org.scalacheck.Arbitrary.{arbAnyVal, arbBool, arbByte, arbInt, arbLong, arbOption, arbShort, arbString, arbUnit, arbitrary}
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util.encode.{Base58, Base64}
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.{AvlTreeConstant, BigIntConstant, BlockValue, BoxConstant, ByteConstant, CollectionConstant, ConstantPlaceholder, ErgoTree, EvaluatedValue, FalseLeaf, FuncValue, GetVarInt, GroupElementConstant, IntConstant, LongConstant, ShortConstant, SigmaBoolean, SigmaPropConstant, SigmaPropValue, StringConstant, TaggedAvlTree, TaggedBox, TaggedInt, TaggedLong, TaggedVariable, TrueLeaf, Tuple, ValDef, ValUse, Value}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.eval.Extensions._
import sigmastate.eval.{CostingBox, SigmaDsl, _}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{ContextExtension, CryptoConstants, ProverResult}
import sigmastate.lang.TransformingSigmaBuilder.{mkAppend, mkAtLeast, mkBoolToSigmaProp, mkByteArrayToBigInt, mkByteArrayToLong, mkCollectionConstant, mkConstant, mkDeserializeContext, mkDeserializeRegister, mkDivide, mkDowncast, mkEQ, mkExists, mkExtractAmount, mkExtractBytes, mkExtractBytesWithNoRef, mkExtractCreationInfo, mkExtractId, mkExtractScriptBytes, mkFilter, mkFold, mkForAll, mkGE, mkGT, mkLE, mkLT, mkMapCollection, mkMax, mkMin, mkMinus, mkModulo, mkMultiply, mkNEQ, mkPlus, mkSigmaAnd, mkSigmaOr, mkSizeOf, mkSlice, mkTaggedVariable, mkTuple}
import sigmastate._
import sigmastate.utxo.{Append, ByIndex, DeserializeContext, DeserializeRegister, Exists, ExtractAmount, ExtractBytes, ExtractBytesWithNoRef, ExtractCreationInfo, ExtractId, ExtractRegisterAs, ExtractScriptBytes, Filter, Fold, ForAll, GetVar, MapCollection, OptionGet, OptionGetOrElse, OptionIsDefined, SizeOf, Slice, Transformer}
import special.collection.Coll
import special.sigma._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

trait ObjectGenerators extends TypeGenerators with ValidationSpecification with ConcreteCollectionGenerators {

  val ThresholdLimit = 10

  implicit lazy val statusArb: Arbitrary[RuleStatus] = Arbitrary(statusGen)
  implicit lazy val arbMapCollection: Arbitrary[MapCollection[SInt.type, SInt.type]] = Arbitrary(mapCollectionGen)
  implicit lazy val arbExists: Arbitrary[Exists[SInt.type]] = Arbitrary(existsGen)
  implicit lazy val arbForAll: Arbitrary[ForAll[SInt.type]] = Arbitrary(forAllGen)
  implicit lazy val arbFold: Arbitrary[Fold[SInt.type, SBoolean.type]] = Arbitrary(foldGen)
  implicit lazy val arbAppend: Arbitrary[Append[SInt.type]] = Arbitrary(appendGen)
  implicit lazy val arbSlice: Arbitrary[Slice[SInt.type]] = Arbitrary(sliceGen)
  implicit lazy val arbAtLeast: Arbitrary[AtLeast] = Arbitrary(atLeastGen)
  implicit lazy val arbFilter: Arbitrary[Filter[SInt.type]] = Arbitrary(filterGen)
  implicit lazy val arbSizeOf: Arbitrary[SizeOf[SInt.type]] = Arbitrary(sizeOfGen)
  implicit lazy val arbExtractAmount: Arbitrary[ExtractAmount] = Arbitrary(extractAmountGen)
  implicit lazy val arbExtractCreationInfo: Arbitrary[ExtractCreationInfo] = Arbitrary(extractCreationInfoGen)
  implicit lazy val arbExtractScriptBytes: Arbitrary[ExtractScriptBytes] = Arbitrary(extractScriptBytesGen)
  implicit lazy val arbExtractBytes: Arbitrary[ExtractBytes] = Arbitrary(extractBytesGen)
  implicit lazy val arbExtractBytesWithNoRef: Arbitrary[ExtractBytesWithNoRef] = Arbitrary(extractBytesWithNoRefGen)
  implicit lazy val arbExtractId: Arbitrary[ExtractId] = Arbitrary(extractIdGen)
  implicit lazy val arbExtractRegisterAs: Arbitrary[ExtractRegisterAs[SInt.type]] = Arbitrary(extractRegisterAsGen)
  implicit lazy val arbIntToByteArray: Arbitrary[LongToByteArray] = Arbitrary(longToByteArrayGen)
  implicit lazy val arbByteArrayToBigInt: Arbitrary[ByteArrayToBigInt] = Arbitrary(byteArrayToBigIntGen)
  implicit lazy val arbCalcBlake2b256: Arbitrary[CalcBlake2b256] = Arbitrary(calcBlake2b256Gen)
  implicit lazy val arbCalcSha256: Arbitrary[CalcSha256] = Arbitrary(calcSha256Gen)
  implicit lazy val arbByIndex: Arbitrary[ByIndex[SInt.type]] = Arbitrary(byIndexGen)
  implicit lazy val arbDeserializeContext: Arbitrary[DeserializeContext[SBoolean.type]] = Arbitrary(deserializeContextGen)
  implicit lazy val arbDeserializeRegister: Arbitrary[DeserializeRegister[SBoolean.type]] = Arbitrary(deserializeRegisterGen)
  implicit lazy val arbByteConstants: Arbitrary[ByteConstant] = Arbitrary(byteConstGen)
  implicit lazy val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit lazy val arbLongConstants: Arbitrary[LongConstant] = Arbitrary(longConstGen)
  implicit lazy val arbByteArrayConstant: Arbitrary[CollectionConstant[SByte.type]] = Arbitrary(byteArrayConstGen)
  implicit lazy val arbGroupElementConstant: Arbitrary[GroupElementConstant] = Arbitrary(groupElementConstGen)
  implicit lazy val arbBoxConstant: Arbitrary[BoxConstant] = Arbitrary(boxConstantGen)
  implicit lazy val arbAvlTreeConstant: Arbitrary[AvlTreeConstant] = Arbitrary(avlTreeConstantGen)
  implicit lazy val arbBigIntConstant: Arbitrary[BigIntConstant] = Arbitrary(bigIntConstGen)
  implicit lazy val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedVar[SInt.type])
  implicit lazy val arbTaggedLong: Arbitrary[TaggedLong] = Arbitrary(taggedVar[SLong.type])
  implicit lazy val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedVar[SBox.type])
  implicit lazy val arbTaggedAvlTree: Arbitrary[TaggedAvlTree] = Arbitrary(taggedAvlTreeGen)
  implicit lazy val arbProveDlog: Arbitrary[ProveDlog] = Arbitrary(proveDlogGen)
  implicit lazy val arbProveDHT: Arbitrary[ProveDHTuple] = Arbitrary(proveDHTGen)
  implicit lazy val arbRegisterIdentifier: Arbitrary[RegisterId] = Arbitrary(registerIdentifierGen)
  implicit lazy val arbBigInteger = Arbitrary(Arbitrary.arbBigInt.arbitrary.map(_.bigInteger))
  implicit lazy val arbBigInt = Arbitrary(arbBigInteger.arbitrary.map(SigmaDsl.BigInt(_)))
  implicit lazy val arbEcPointType = Arbitrary(Gen.const(()).flatMap(_ => CryptoConstants.dlogGroup.createRandomGenerator()))
  implicit lazy val arbGroupElement = Arbitrary(arbEcPointType.arbitrary.map(SigmaDsl.GroupElement(_)))
  implicit lazy val arbSigmaBoolean: Arbitrary[SigmaBoolean] = Arbitrary(Gen.oneOf(proveDHTGen, proveDHTGen))
  implicit lazy val arbSigmaProp: Arbitrary[SigmaProp] = Arbitrary(sigmaPropGen)
  implicit lazy val arbSigmaPropValue: Arbitrary[SigmaPropValue] = Arbitrary(sigmaPropValueGen)
  implicit lazy val arbErgoBox = Arbitrary(ergoBoxGen)
  implicit lazy val arbBox = Arbitrary(ergoBoxGen.map(SigmaDsl.Box))
  implicit lazy val arbAvlTreeData = Arbitrary(avlTreeDataGen)
  implicit lazy val arbAvlTree = Arbitrary(avlTreeGen)
  implicit lazy val arbBoxCandidate = Arbitrary(ergoBoxCandidateGen(tokensGen.sample.get))
  implicit lazy val arbTransaction = Arbitrary(ergoTransactionGen)
  implicit lazy val arbContextExtension = Arbitrary(contextExtensionGen)
  implicit lazy val arbSerializedProverResult = Arbitrary(serializedProverResultGen)
  implicit lazy val arbUnsignedInput = Arbitrary(unsignedInputGen)
  implicit lazy val arbInput = Arbitrary(inputGen)

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

  val heightGen: Gen[Int] = Gen.chooseNum(0, 1000000)

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


  val proveDlogGen: Gen[ProveDlog] = for {v <- groupElementGen} yield ProveDlog(v)
  val proveDHTGen: Gen[ProveDHTuple] = for {
    gv <- groupElementGen
    hv <- groupElementGen
    uv <- groupElementGen
    vv <- groupElementGen
  } yield ProveDHTuple(gv, hv, uv, vv)

  lazy val sigmaTreeNodeGen: Gen[SigmaBoolean] = for {
    itemsNum <- Gen.choose(2, ThresholdLimit)
    items <- if (itemsNum <= 2) {
      Gen.listOfN(itemsNum, sigmaBooleanGen)
    } else {
      Gen.listOfN(itemsNum, nonRecursiveSigmaBoolean)
    }
    threshold <- Gen.choose(1, itemsNum)
    node <- Gen.oneOf(
      CTHRESHOLD(threshold, items),
      COR(items),
      CAND(items)
    )
  } yield node

  val nonRecursiveSigmaBoolean: Gen[SigmaBoolean] = Gen.oneOf(proveDlogGen, proveDHTGen)

  val sigmaBooleanGen: Gen[SigmaBoolean] = Gen.oneOf(
    nonRecursiveSigmaBoolean,
    Gen.delay(sigmaTreeNodeGen)
  )

  val sigmaPropGen: Gen[SigmaProp] = sigmaBooleanGen.map(SigmaDsl.SigmaProp)
  val sigmaPropValueGen: Gen[SigmaPropValue] =
    Gen.oneOf(proveDlogGen.map(SigmaPropConstant(_)), proveDHTGen.map(SigmaPropConstant(_)))

  val registerIdentifierGen: Gen[RegisterId] = Gen.oneOf(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9)

  val taggedAvlTreeGen: Gen[TaggedAvlTree] =
    arbByte.arbitrary.map { v => TaggedAvlTree(v).asInstanceOf[TaggedAvlTree] }

  def additionalRegistersGen(cnt: Byte): Seq[Gen[(NonMandatoryRegisterId, EvaluatedValue[SType])]] = {
    (0 until cnt)
      .map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI).asInstanceOf[NonMandatoryRegisterId])
      .map { r =>
        for {
          arr <- byteArrayConstGen
          v <- Gen.oneOf(TrueLeaf, FalseLeaf, arr)
        } yield r -> v.asInstanceOf[EvaluatedValue[SType]]
      }
  }

  def additionalTokensGen(cnt: Int): Seq[Gen[(Digest32, Long)]] =
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

  val aDDigestGen: Gen[ADDigest] = Gen.listOfN(AvlTreeData.DigestSize, arbByte.arbitrary).map(ADDigest @@ _.toArray)

  def avlTreeDataGen: Gen[AvlTreeData] = for {
    digest <- aDDigestGen
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

  lazy val modifierIdGen: Gen[ModifierId] = Gen.listOfN(32, arbByte.arbitrary)
    .map(id => bytesToId(id.toArray))

  lazy val modifierIdBytesGen: Gen[ModifierIdBytes] = Gen.listOfN(32, arbByte.arbitrary)
    .map(id => ModifierIdBytes @@ bytesToId(id.toArray).toBytes.toColl)

  val ergoBoxGen: Gen[ErgoBox] = for {
    tId <- modifierIdGen
    boxId <- unsignedShortGen
    tokensCount <- Gen.chooseNum[Int](0, 20)
    tokens <- Gen.sequence(additionalTokensGen(tokensCount)).map(_.asScala.map(_._1))
    candidate <- ergoBoxCandidateGen(tokens)
  } yield candidate.toBox(tId, boxId)

  def ergoBoxCandidateGen(availableTokens: Seq[Digest32]): Gen[ErgoBoxCandidate] = for {
    l <- arbLong.arbitrary
    b <- ergoTreeGen.filter(t => t.bytes.length < MaxPropositionBytes.value)
    regNum <- Gen.chooseNum[Byte](0, ErgoBox.nonMandatoryRegistersCount)
    ar <- Gen.sequence(additionalRegistersGen(regNum))
    tokensCount <- Gen.chooseNum[Int](0, 20)
    tokens <- if(availableTokens.nonEmpty) {
      Gen.listOfN(tokensCount, Gen.oneOf(availableTokens))
    } else {
      Gen.oneOf(Seq(List[Digest32]()))
    }
    tokenAmounts <- Gen.listOfN(tokensCount, Gen.oneOf(1, 500, 20000, 10000000, Long.MaxValue))
    creationHeight <- heightGen
  } yield new ErgoBoxCandidate(l, b, creationHeight, tokens.toColl.zip(tokenAmounts.toColl), ar.asScala.toMap)

  val boxConstantGen: Gen[BoxConstant] = ergoBoxGen.map { v => BoxConstant(CostingBox(false, v)) }

  val digest32Gen: Gen[Digest32] = for {
    bytes <- Gen.listOfN(TokenId.size, arbByte.arbitrary).map(_.toArray)
  } yield Digest32 @@ bytes

  val tokenIdGen: Gen[Digest32] = digest32Gen

  val tokensGen: Gen[Seq[Digest32]] = for {
    count <- Gen.chooseNum(10, 50)
    tokens <- Gen.listOfN(count, tokenIdGen)
  } yield tokens

  val digest32CollGen: Gen[Digest32Coll] = digest32Gen.map(Digest32Coll @@ _.toColl)

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

  def byteArrayGen(length: Int): Gen[Array[Byte]] = for {
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield bytes.toArray

  def byteArrayGen(minLength: Int, maxLength: Int): Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(minLength, maxLength)
    bytes <- Gen.listOfN(length, arbByte.arbitrary)
  } yield bytes.toArray

  def byteCollGen(length: Int): Gen[Coll[Byte]] = byteArrayGen(length).map(_.toColl)

  val minerVotesGen: Gen[MinerVotes] = byteCollGen(MinerVotes.size).map(MinerVotes @@ _)

  val nonceBytesGen: Gen[NonceBytes] = byteCollGen(NonceBytes.size).map(NonceBytes @@ _)

  import ValidationRules._

  val numRules = currentSettings.size

  val replacedRuleIdGen = Gen.chooseNum((FirstRuleId + numRules).toShort, Short.MaxValue)

  val ruleIdGen = Gen.chooseNum(FirstRuleId, (FirstRuleId + numRules - 1).toShort)

  val statusGen: Gen[RuleStatus] = Gen.oneOf(
    Gen.oneOf(EnabledRule, DisabledRule),
    replacedRuleIdGen.map(id => ReplacedRule(id)),
    byteArrayGen(1, 10).map(xs => ChangedRule(xs))
  )

  val mapCollectionGen: Gen[MapCollection[SInt.type, SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    mapper <- funcValueGen
  } yield mkMapCollection(input, mapper).asInstanceOf[MapCollection[SInt.type, SInt.type]]

  val existsGen: Gen[Exists[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    condition <- funcValueGen
  } yield mkExists(input, condition).asInstanceOf[Exists[SInt.type]]

  val forAllGen: Gen[ForAll[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    condition <- funcValueGen
  } yield mkForAll(input, condition).asInstanceOf[ForAll[SInt.type]]

  val foldGen: Gen[Fold[SInt.type, SBoolean.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    foldOp <- funcValueGen
  } yield
    mkFold(input, TrueLeaf, foldOp).asInstanceOf[Fold[SInt.type, SBoolean.type]]

  val sliceGen: Gen[Slice[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    from <- intConstGen
    until <- intConstGen
  } yield mkSlice(col1, from, until).asInstanceOf[Slice[SInt.type]]

  val atLeastGen: Gen[AtLeast] = for {
    bound <- intConstGen
    input <- arbCCOfSigmaPropConstant.arbitrary
  } yield mkAtLeast(bound, input).asInstanceOf[AtLeast]

  val filterGen: Gen[Filter[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    condition <- funcValueGen
  } yield mkFilter(col1, condition).asInstanceOf[Filter[SInt.type]]

  val appendGen: Gen[Append[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    col2 <- arbCCOfIntConstant.arbitrary
  } yield mkAppend(col1, col2).asInstanceOf[Append[SInt.type]]

  val sizeOfGen: Gen[SizeOf[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
  } yield mkSizeOf(input).asInstanceOf[SizeOf[SInt.type]]

  val extractAmountGen: Gen[ExtractAmount] =
    arbTaggedBox.arbitrary.map { b => mkExtractAmount(b).asInstanceOf[ExtractAmount] }
  val extractScriptBytesGen: Gen[ExtractScriptBytes] =
    arbTaggedBox.arbitrary.map { b => mkExtractScriptBytes(b).asInstanceOf[ExtractScriptBytes] }
  val extractBytesGen: Gen[ExtractBytes] =
    arbTaggedBox.arbitrary.map { b => mkExtractBytes(b).asInstanceOf[ExtractBytes] }
  val extractBytesWithNoRefGen: Gen[ExtractBytesWithNoRef] =
    arbTaggedBox.arbitrary.map { b =>
      mkExtractBytesWithNoRef(b).asInstanceOf[ExtractBytesWithNoRef]
    }
  val extractIdGen: Gen[ExtractId] =
    arbTaggedBox.arbitrary.map { b => mkExtractId(b).asInstanceOf[ExtractId] }
  val extractRegisterAsGen: Gen[ExtractRegisterAs[SInt.type]] = for {
    input <- arbTaggedBox.arbitrary
    r <- arbRegisterIdentifier.arbitrary
  } yield ExtractRegisterAs(input, r)(SInt)
  val extractCreationInfoGen: Gen[ExtractCreationInfo] =
    arbTaggedBox.arbitrary.map { b => mkExtractCreationInfo(b).asInstanceOf[ExtractCreationInfo] }

  val deserializeContextGen: Gen[DeserializeContext[SBoolean.type]] =
    Arbitrary.arbitrary[Byte].map(b =>
      mkDeserializeContext(b, SBoolean).asInstanceOf[DeserializeContext[SBoolean.type]])

  val deserializeRegisterGen: Gen[DeserializeRegister[SBoolean.type]] = for {
    r <- arbRegisterIdentifier.arbitrary
    default <- booleanConstGen
    isDefined <- Arbitrary.arbitrary[Boolean]
    defaultOpt = if (isDefined) Some(default) else None
  } yield mkDeserializeRegister(r, SBoolean, defaultOpt)
    .asInstanceOf[DeserializeRegister[SBoolean.type]]

  val longToByteArrayGen: Gen[LongToByteArray] = arbLongConstants.arbitrary.map { v => LongToByteArray(v) }
  val byteArrayToBigIntGen: Gen[ByteArrayToBigInt] =
    arbByteArrayConstant.arbitrary.map { v =>
      mkByteArrayToBigInt(v).asInstanceOf[ByteArrayToBigInt]
    }
  val calcBlake2b256Gen: Gen[CalcBlake2b256] = arbByteArrayConstant.arbitrary.map { v => CalcBlake2b256(v) }
  val calcSha256Gen: Gen[CalcSha256] = arbByteArrayConstant.arbitrary.map { v => CalcSha256(v) }

  val byIndexGen: Gen[ByIndex[SInt.type]] = for {
    input <- Gen.oneOf(intConstCollectionGen, intArrayConstGen)
    index <- arbInt.arbitrary
    defaultValue <- arbOption(arbIntConstants).arbitrary
  } yield ByIndex(input, index, defaultValue)

  val booleanExprGen: Gen[Value[SBoolean.type]] =
    Gen.oneOf(
      Gen.oneOf(
        EQ(IntConstant(1), IntConstant(1)), // true
        EQ(IntConstant(1), IntConstant(2)) // false
      ),
      booleanConstGen
    )

  private type LogicalTransformerCons =
    Seq[Value[SBoolean.type]] => Transformer[SCollection[SBoolean.type], SBoolean.type]

  def logicalExprTreeNodeGen(nodeCons: Seq[LogicalTransformerCons]): Gen[Transformer[SCollection[SBoolean.type], SBoolean.type]] = for {
    left <- logicalExprTreeGen(nodeCons)
    right <- logicalExprTreeGen(nodeCons)
    node <- Gen.oneOf(nodeCons.map(cons => cons(Seq(left, right))))
  } yield node

  def logicalExprTreeGen(nodeCons: Seq[LogicalTransformerCons]): Gen[Value[SBoolean.type]] =
    Gen.oneOf(booleanExprGen, booleanConstGen, Gen.delay(logicalExprTreeNodeGen(nodeCons)))

  def numExprTreeNodeGen: Gen[Value[SNumericType]] = for {
    left <- numExprTreeGen
    right <- numExprTreeGen
    node <- Gen.oneOf(
      mkPlus(left, right),
      mkMinus(left, right),
      mkMultiply(left, right),
      mkDivide(left, right),
      mkModulo(left, right),
      mkMin(left, right),
      mkMax(left, right)
    )
  } yield node

  def numExprTreeGen: Gen[Value[SNumericType]] =
    Gen.oneOf(arbByteConstants.arbitrary,
      arbIntConstants.arbitrary,
      arbLongConstants.arbitrary,
      arbBigIntConstant.arbitrary,
      Gen.delay(numExprTreeNodeGen))

  def comparisonExprTreeNodeGen: Gen[Value[SBoolean.type]] = for {
    left <- numExprTreeNodeGen
    right <- numExprTreeNodeGen
    node <- Gen.oneOf(
      mkEQ(left, right),
      mkNEQ(left, right),
      mkLE(left, right),
      mkGE(left, right),
      mkLT(left, right),
      mkGT(left, right)
    )
  } yield node

  val downcastGen: Gen[Downcast[SNumericType, SNumericType]] = for {
    numVal <- Gen.oneOf(numExprTreeNodeGen, shortConstGen, intConstGen, longConstGen)
  } yield mkDowncast(numVal, SByte).asInstanceOf[Downcast[SNumericType, SNumericType]]

  val base58StringGen: Gen[String] = for {
    s <- Gen.someOf(Base58.Alphabet).suchThat(_.nonEmpty)
  } yield s.toString

  val base64StringGen: Gen[String] = for {
    s <- Gen.someOf(Base64.Alphabet).suchThat(_.length > 1)
  } yield s.toString

  def p2pkAddressGen(networkPrefix: Byte): Gen[P2PKAddress] = for {
    pd <- proveDlogGen
  } yield P2PKAddress(pd)(new ErgoAddressEncoder(networkPrefix))

  val getVarIntGen: Gen[GetVar[SInt.type]] = for {
    varId <- arbByte.arbitrary
  } yield GetVarInt(varId)

  val optionGetGen: Gen[OptionGet[SInt.type]] = for {
    getVar <- getVarIntGen
  } yield OptionGet(getVar)

  val optionGetOrElseGen: Gen[OptionGetOrElse[SInt.type]] = for {
    getVar <- getVarIntGen
  } yield OptionGetOrElse(getVar, IntConstant(1))

  val optionIsDefinedGen: Gen[OptionIsDefined[SInt.type]] = for {
    getVar <- getVarIntGen
  } yield OptionIsDefined(getVar)

  val valDefGen: Gen[ValDef] = for {
    id <- unsignedIntGen
    rhs <- booleanExprGen
  } yield ValDef(id, Nil, rhs)

  val funDefGen: Gen[ValDef] = for {
    id <- unsignedIntGen
    tpeArgs <- Gen.nonEmptyListOf(sTypeIdentGen)
    rhs <- booleanExprGen
  } yield ValDef(id, tpeArgs, rhs)

  val valOrFunDefGen: Gen[ValDef] = for {
    v <- Gen.oneOf(valDefGen, funDefGen)
  } yield v

  val valUseGen: Gen[ValUse[SType]] = for {
    id <- unsignedIntGen
    tpe <- predefTypeGen
  } yield ValUse(id, tpe)

  val blockValueGen: Gen[BlockValue] = for {
    items <- Gen.nonEmptyListOf(valDefGen)
  } yield BlockValue(items.toIndexedSeq,
    EQ(
      SizeOf(Tuple(items.toIndexedSeq.map(valDef => ValUse(valDef.id, valDef.tpe)))),
      IntConstant(items.length)))

  val constantPlaceholderGen: Gen[ConstantPlaceholder[SType]] = for {
    id <- unsignedIntGen
    tpe <- predefTypeGen
  } yield ConstantPlaceholder(id, tpe)

  val funcValueArgsGen: Gen[IndexedSeq[(Int, SType)]] = for {
    num <- Gen.chooseNum(1, 10)
    indices <- Gen.listOfN(num, unsignedIntGen)
    tpes <- Gen.listOfN(num, predefTypeGen)
  } yield indices.zip(tpes).toIndexedSeq

  val funcValueGen: Gen[FuncValue] = for {
    args <- funcValueArgsGen
    body <- logicalExprTreeNodeGen(Seq(AND.apply))
  } yield FuncValue(args, body)

  val sigmaAndGen: Gen[SigmaAnd] = for {
    num <- Gen.chooseNum(1, ThresholdLimit)
    items <- Gen.listOfN(num, sigmaPropValueGen)
  } yield mkSigmaAnd(items).asInstanceOf[SigmaAnd]

  val sigmaOrGen: Gen[SigmaOr] = for {
    num <- Gen.chooseNum(1, ThresholdLimit)
    items <- Gen.listOfN(num, sigmaPropValueGen)
  } yield mkSigmaOr(items).asInstanceOf[SigmaOr]

  val sigmaThresholdGen: Gen[CTHRESHOLD] = for {
    num <- Gen.chooseNum(1, ThresholdLimit)
    threshold <- Gen.choose(0, num)
    items: Seq[SigmaBoolean] <- Gen.listOfN(num, sigmaBooleanGen).map(_.toSeq)
  } yield CTHRESHOLD(threshold, items)


  val boolToSigmaPropGen: Gen[BoolToSigmaProp] = for {
    b <- booleanConstGen
  } yield mkBoolToSigmaProp(b).asInstanceOf[BoolToSigmaProp]

  val byteArrayToLongGen: Gen[ByteArrayToLong] =
    arbByteArrayConstant.arbitrary.map { v =>
      mkByteArrayToLong(v).asInstanceOf[ByteArrayToLong]
    }

  val ergoTreeGen: Gen[ErgoTree] = for {
    sigmaBoolean <- Gen.delay(sigmaBooleanGen)
    propWithConstants <- Gen.delay(logicalExprTreeNodeGen(Seq(AND.apply, OR.apply, XorOf.apply)).map(_.toSigmaProp))
    prop <- Gen.oneOf(propWithConstants, sigmaBoolean.toSigmaProp)
    treeBuilder <- Gen.oneOf(Seq[SigmaPropValue => ErgoTree](ErgoTree.withSegregation,
      ErgoTree.withoutSegregation))
  } yield treeBuilder(prop)

  def headerGen(stateRoot: AvlTree, parentId: ModifierIdBytes): Gen[Header] = for {
    id <- modifierIdBytesGen
    version <- arbByte.arbitrary
    adProofsRoot <- digest32CollGen
    transactionRoot <- digest32CollGen
    timestamp <- arbLong.arbitrary
    nBits <- arbLong.arbitrary
    height <- heightGen
    extensionRoot <- digest32CollGen
    minerPk <- groupElementGen
    powOnetimePk <- groupElementGen
    powNonce <- nonceBytesGen
    powDistance <- arbBigInt.arbitrary
    votes <- minerVotesGen
  } yield CHeader(id, version, parentId, adProofsRoot, stateRoot, transactionRoot, timestamp, nBits,
    height, extensionRoot, minerPk, powOnetimePk, powNonce, powDistance, votes)

  def headersGen(stateRoot: AvlTree): Gen[Seq[Header]] = for {
    size <- Gen.chooseNum(0, 10)
  } yield if (size == 0) Seq() else
    (0 to size)
    .foldLeft(List[Header](headerGen(stateRoot, modifierIdBytesGen.sample.get).sample.get)) { (h, _) =>
      h :+ headerGen(stateRoot, h.last.id).sample.get
    }.reverse

  def preHeaderGen(parentId: ModifierIdBytes): Gen[PreHeader] = for {
    version <- arbByte.arbitrary
    timestamp <- arbLong.arbitrary
    nBits <- arbLong.arbitrary
    height <- heightGen
    minerPk <- groupElementGen
    votes <- minerVotesGen
  } yield CPreHeader(version, parentId, timestamp, nBits, height, minerPk, votes)


  val ergoLikeContextGen: Gen[ErgoLikeContext] = for {
    stateRoot <- avlTreeGen
    headers <- headersGen(stateRoot)
    preHeader <- preHeaderGen(headers.headOption.map(_.id).getOrElse(modifierIdBytesGen.sample.get))
    tokens <- tokensGen
    dataBoxes <- Gen.nonEmptyListOf(ergoBoxGen)
    boxesToSpend <- Gen.nonEmptyListOf(ergoBoxGen)
    extension <- contextExtensionGen
    outputsCount <- Gen.chooseNum(50, 200)
    outputCandidates <- Gen.listOfN(outputsCount, ergoBoxCandidateGen(tokens))
    costLimit <- arbLong.arbitrary
    initCost <- arbLong.arbitrary
    avlTreeFlags <- avlTreeFlagsGen
  } yield new ErgoLikeContext(
    lastBlockUtxoRoot = AvlTreeData(ADDigest @@ stateRoot.digest.toArray, avlTreeFlags, unsignedIntGen.sample.get),
    headers = headers.toColl,
    preHeader = preHeader,
    dataBoxes = dataBoxes.toIndexedSeq,
    boxesToSpend = boxesToSpend.toIndexedSeq,
    spendingTransaction = new ErgoLikeTransaction(
      boxesToSpend.map(b => Input(b.id, serializedProverResultGen.sample.get)).toIndexedSeq,
      dataBoxes.map(b => DataInput(b.id)).toIndexedSeq,
      outputCandidates.toIndexedSeq),
    selfIndex = 0,
    extension = extension,
    validationSettings = ValidationRules.currentSettings,
    costLimit = costLimit,
    initCost = initCost
  )

}
