package sigmastate.serialization.generators

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import sigmastate.SInt
import sigmastate.utxo.MapCollection

trait TransformerGenerators { self: ValueGeneratots with ConcreteCollectionGenerators =>

  implicit val arbMapCollection: Arbitrary[MapCollection[SInt.type, SInt.type ]] = Arbitrary(mapCollectionGen)

  val mapCollectionGen: Gen[MapCollection[SInt.type, SInt.type]] = for {
    input <- arbCCOfIntConstant.arbitrary
    idByte <- arbByte.arbitrary
    mapper <- arbIntConstants.arbitrary
  } yield MapCollection(input, idByte, mapper)

}
