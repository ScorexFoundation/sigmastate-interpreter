package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import sigmastate.SInt
import sigmastate.Values.{FalseLeaf, TrueLeaf}
import sigmastate.utxo._

trait TransformerGenerators { self: ValueGeneratots with ConcreteCollectionGenerators =>

  implicit val arbMapCollection: Arbitrary[MapCollection[SInt.type, SInt.type ]] = Arbitrary(mapCollectionGen)
  implicit val arbExists: Arbitrary[Exists[SInt.type ]] = Arbitrary(existsGen)
  implicit val arbForAll: Arbitrary[ForAll[SInt.type ]] = Arbitrary(forAllGen)
  implicit val arbFold: Arbitrary[Fold[SInt.type]] = Arbitrary(foldGen)
  implicit val sizeOf: Arbitrary[SizeOf[SInt.type]] = Arbitrary(sizeOfGen)
  implicit val arbExtractAmount: Arbitrary[ExtractAmount] = Arbitrary(extractAmountGen)
  implicit val arbExtractScriptBytes: Arbitrary[ExtractScriptBytes] = Arbitrary(extractScriptBytesGen)
  implicit val arbExtractBytes: Arbitrary[ExtractBytes] = Arbitrary(extractBytesGen)
  implicit val arbExtractBytesWithNoRef: Arbitrary[ExtractBytesWithNoRef] = Arbitrary(extractBytesWithNoRefGen)
  implicit val arbExtractId: Arbitrary[ExtractId] = Arbitrary(extractIdGen)

  val mapCollectionGen: Gen[MapCollection[SInt.type, SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    mapper <- arbIntConstants.arbitrary
  } yield MapCollection(input, idByte, mapper)

  val existsGen: Gen[Exists[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    condition <- Gen.oneOf(TrueLeaf, FalseLeaf)
  } yield Exists(input, idByte, condition)

  val forAllGen: Gen[ForAll[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    condition <- Gen.oneOf(TrueLeaf, FalseLeaf)
  } yield ForAll(input, idByte, condition)

  val foldGen: Gen[Fold[SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
  } yield Fold.sum(input)

  val sizeOfGen: Gen[SizeOf[SInt.type ]] = for {
    input <- arbCCOfIntConstant.arbitrary
  } yield SizeOf(input)

  val extractAmountGen: Gen[ExtractAmount] = arbTaggedBox.arbitrary.map{b => ExtractAmount(b)}
  val extractScriptBytesGen: Gen[ExtractScriptBytes] = arbTaggedBox.arbitrary.map{b => ExtractScriptBytes(b)}
  val extractBytesGen: Gen[ExtractBytes] = arbTaggedBox.arbitrary.map{b => ExtractBytes(b)}
  val extractBytesWithNoRefGen: Gen[ExtractBytesWithNoRef] = arbTaggedBox.arbitrary.map{b => ExtractBytesWithNoRef(b)}
  val extractIdGen: Gen[ExtractId] = arbTaggedBox.arbitrary.map{b => ExtractId(b)}

}
