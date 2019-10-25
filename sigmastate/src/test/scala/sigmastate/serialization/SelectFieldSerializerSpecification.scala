package sigmastate.serialization

import org.scalacheck.Gen
import sigmastate.Values.{FalseLeaf, IntConstant, Tuple}
import sigmastate.serialization.OpCodes.{SelectFieldCode, TupleCode}
import sigmastate.utxo.SelectField

class SelectFieldSerializerSpecification extends TableSerializationSpecification {

  property("SelectField: Serializer round trip ") {
    forAll(tupleGen(2, 10)) { tuple: Tuple =>
      val index = Gen.chooseNum(1, tuple.length - 1).sample.get
      roundTripTest(SelectField(tuple, index.toByte))
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (SelectField(Tuple(IntConstant(1), FalseLeaf), 2),
      Array[Byte](SelectFieldCode, TupleCode, 2, 4, 2, 1, 0, 2))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
