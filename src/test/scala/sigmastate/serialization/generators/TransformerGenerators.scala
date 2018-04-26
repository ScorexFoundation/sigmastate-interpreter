package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import sigmastate.SInt
import sigmastate.Values.{FalseLeaf, TrueLeaf}
import sigmastate.utxo.{Exists, Fold, ForAll, MapCollection}

trait TransformerGenerators { self: ValueGeneratots with ConcreteCollectionGenerators =>

  implicit val arbMapCollection: Arbitrary[MapCollection[SInt.type, SInt.type ]] = Arbitrary(mapCollectionGen)
  implicit val arbExists: Arbitrary[Exists[SInt.type ]] = Arbitrary(existsGen)
  implicit val arbForAll: Arbitrary[ForAll[SInt.type ]] = Arbitrary(forAllGen)
  implicit val arbFold: Arbitrary[Fold[SInt.type]] = Arbitrary(foldGen)

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

}
