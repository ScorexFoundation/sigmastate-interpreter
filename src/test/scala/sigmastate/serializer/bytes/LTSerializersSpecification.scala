package sigmastate.serializer.bytes

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import sigmastate.{Height, IntConstant, LT}

class LTSerializersSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  // todo params generator
  val serializers = Table(
    "objects",
    LT(IntConstant(10), IntConstant(20)),
    LT(Height, IntConstant(100))
  )

  property("LT serializer roundtrip") {
    forAll(serializers) { o: LT =>
      val s = new LTSerializer
      val bytes = s.toBytes(o)
      val parsed = s.parseBytes(bytes).get
      o shouldBe parsed
    }
  }
}
