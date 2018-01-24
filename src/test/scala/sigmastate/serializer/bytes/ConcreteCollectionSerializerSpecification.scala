package sigmastate.serializer.bytes

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import sigmastate._
import sigmastate.serializer.bytes._
import sigmastate.serializer.bytes.base._

class ConcreteCollectionSerializerSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  val s = new ConcreteCollectionSerializer[SCollection[SInt.type]]()

  property("ConcreteCollection serializer roundtrip") {
    val col = ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))
    val bytes = s.toBytes(col)
    s.parseBytes(bytes).get shouldBe col
  }
}
