package sigmastate.serialization.generators

import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scorex.util.encode.{Base58, Base64}
import sigmastate.Values.{FalseLeaf, IntConstant, TrueLeaf, Value}
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.TransformingSigmaBuilder
import sigmastate.utxo._

trait TransformerGenerators {
  self: ValueGenerators with ConcreteCollectionGenerators =>

  import TransformingSigmaBuilder._

  implicit val arbMapCollection: Arbitrary[MapCollection[SInt.type, SInt.type]] = Arbitrary(mapCollectionGen)
  implicit val arbExists: Arbitrary[Exists[SInt.type]] = Arbitrary(existsGen)
  implicit val arbForAll: Arbitrary[ForAll[SInt.type]] = Arbitrary(forAllGen)
  implicit val arbFold: Arbitrary[Fold[SInt.type, SBoolean.type]] = Arbitrary(foldGen)
  implicit val arbAppend: Arbitrary[Append[SInt.type]] = Arbitrary(appendGen)
  implicit val arbSlice: Arbitrary[Slice[SInt.type]] = Arbitrary(sliceGen)
  implicit val arbAtLeast: Arbitrary[AtLeast] = Arbitrary(atLeastGen)
  implicit val arbWhere: Arbitrary[Where[SInt.type]] = Arbitrary(whereGen)
  implicit val sizeOf: Arbitrary[SizeOf[SInt.type]] = Arbitrary(sizeOfGen)
  implicit val arbExtractAmount: Arbitrary[ExtractAmount] = Arbitrary(extractAmountGen)
  implicit val arbExtractCreationInfo: Arbitrary[ExtractCreationInfo] = Arbitrary(extractCreationInfoGen)
  implicit val arbExtractScriptBytes: Arbitrary[ExtractScriptBytes] = Arbitrary(extractScriptBytesGen)
  implicit val arbExtractBytes: Arbitrary[ExtractBytes] = Arbitrary(extractBytesGen)
  implicit val arbExtractBytesWithNoRef: Arbitrary[ExtractBytesWithNoRef] = Arbitrary(extractBytesWithNoRefGen)
  implicit val arbExtractId: Arbitrary[ExtractId] = Arbitrary(extractIdGen)
  implicit val arbExtractRegisterAs: Arbitrary[ExtractRegisterAs[SInt.type]] = Arbitrary(extractRegisterAsGen)
  implicit val arbIntToByteArray: Arbitrary[LongToByteArray] = Arbitrary(longToByteArrayGen)
  implicit val arbByteArrayToBigInt: Arbitrary[ByteArrayToBigInt] = Arbitrary(byteArrayToBigIntGen)
  implicit val arbCalcBlake2b256: Arbitrary[CalcBlake2b256] = Arbitrary(calcBlake2b256Gen)
  implicit val arbCalcSha256: Arbitrary[CalcSha256] = Arbitrary(calcSha256Gen)
  implicit val arbByIndex: Arbitrary[ByIndex[SInt.type]] = Arbitrary(byIndexGen)
  implicit val arbDeserializeContext: Arbitrary[DeserializeContext[SBoolean.type]] = Arbitrary(deserializeContextGen)
  implicit val arbDeserializeRegister: Arbitrary[DeserializeRegister[SBoolean.type]] = Arbitrary(deserializeRegisterGen)

  val mapCollectionGen: Gen[MapCollection[SInt.type, SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    mapper <- arbIntConstants.arbitrary
  } yield mkMapCollection(input, idByte, mapper).asInstanceOf[MapCollection[SInt.type, SInt.type]]

  val existsGen: Gen[Exists[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    condition <- Gen.oneOf(TrueLeaf, FalseLeaf)
  } yield mkExists(input, idByte, condition).asInstanceOf[Exists[SInt.type]]

  val forAllGen: Gen[ForAll[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    condition <- Gen.oneOf(TrueLeaf, FalseLeaf)
  } yield mkForAll(input, idByte, condition).asInstanceOf[ForAll[SInt.type]]

  val foldGen: Gen[Fold[SInt.type, SBoolean.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
  } yield mkFold(input, 21, TrueLeaf, 22, AND(TaggedBoolean(21), GT(TaggedInt(21), IntConstant(1))))
    .asInstanceOf[Fold[SInt.type, SBoolean.type]]

  val sliceGen: Gen[Slice[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    from <- intConstGen
    until <- intConstGen
  } yield mkSlice(col1, from, until).asInstanceOf[Slice[SInt.type]]

  val atLeastGen: Gen[AtLeast] = for {
    bound <- intConstGen
    input <- arbCCOfBoolConstant.arbitrary
  } yield mkAtLeast(bound, input).asInstanceOf[AtLeast]

  val whereGen: Gen[Where[SInt.type]] = for {
    col1 <- arbCCOfIntConstant.arbitrary
    id <- Arbitrary.arbitrary[Byte]
    condition <- booleanConstGen
  } yield mkWhere(col1, id, condition).asInstanceOf[Where[SInt.type]]

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
    dvInt <- arbIntConstants.arbitrary
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
      mkByteArrayToBigInt(v).asInstanceOf[ByteArrayToBigInt] }
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
        EQ(IntConstant(1), IntConstant(2))  // false
      ),
      proveDlogGen,
      proveDHTGen
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
  } yield ValDef(id, Seq(), rhs)

  val funDefGen: Gen[ValDef] = for {
    id <- unsignedIntGen
    tpeArgs <- Gen.nonEmptyListOf(sTypeIdentGen)
    rhs <- booleanExprGen
  } yield ValDef(id, tpeArgs, rhs)

  val valOrFunDefGen: Gen[ValDef] = for {
    v <- Gen.oneOf(valDefGen, funDefGen)
  } yield v
}
