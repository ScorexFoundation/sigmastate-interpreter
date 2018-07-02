package sigmastate.serialization

import org.scalacheck.Arbitrary.arbByte
import sigmastate.Values.{FalseLeaf, IntConstant, Tuple}
import sigmastate.serialization.OpCodes.{SelectFieldCode, TupleCode}
import sigmastate.utxo.SelectField

class SelectFieldSerializerSpecification extends TableSerializationSpecification {

  property("SelectField: Serializer round trip ") {
    forAll(tupleGen(2, 10), arbByte.arbitrary.suchThat(_ > 0)) { (tuple: Tuple, index: Byte) =>
      roundTripTest(SelectField(tuple, index))
    }
  }

  override def objects = Table(
    ("object", "bytes"),
    (SelectField(Tuple(IntConstant(1), FalseLeaf), 1),
      Array[Byte](SelectFieldCode, TupleCode, 2, 4, 2, 1, 0, 1))
  )

  tableRoundTripTest("Specific objects serializer round trip")
  tablePredefinedBytesTest("Specific objects deserialize from predefined bytes")

}
