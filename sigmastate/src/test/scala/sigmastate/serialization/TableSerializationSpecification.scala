package sigmastate.serialization

import org.scalatest.prop.TableFor2
import sigmastate.Values._
import sigmastate.SType

trait TableSerializationSpecification extends SerializationSpecification {
  def objects: TableFor2[_ <: Value[_ <: SType], Array[Byte]]

  def tableRoundTripTest(title: String) = property(title) {
    forAll(objects) { (relationObject, _) =>
      roundTripTest(relationObject)
    }
  }

  def tablePredefinedBytesTest(title: String) = property(title) {
    forAll(objects) { (value, bytes) =>
      predefinedBytesTest(value, bytes)
    }
  }
}
