package sigmastate.serialization

import org.scalacheck.Gen
import sigmastate.Values.{FalseLeaf, IntConstant, Tuple, Value}
import sigmastate._

class TupleSerializerSpecification extends TableSerializationSpecification {

  private def arbTypeValueGen: Gen[Value[SType]] =
    Gen.oneOf(
      byteArrayConstGen,
      byteConstGen,
      // todo add short
      intConstGen,
      longConstGen,
      booleanConstGen,
      bigIntConstGen,
      groupElementConstGen
    )

  private def arbTypeValueTupleGen(min: Int, max: Int): Gen[Tuple] = for {
    length <- Gen.chooseNum(min, max)
    values <- Gen.listOfN(length, arbTypeValueGen)
  } yield Tuple(values)

  property("Tuple: Serializer round trip ") {
    forAll(arbTypeValueTupleGen(1, 10)) { tuple: Tuple =>
      roundTripTest(tuple)
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (Tuple(IntConstant(1), FalseLeaf),
      Array[Byte](OpCodes.TupleCode, 2, 1, 0))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
