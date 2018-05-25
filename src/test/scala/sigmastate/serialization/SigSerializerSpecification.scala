package sigmastate.serialization

import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.Values.{TrueLeaf, Value}
import sigmastate.{NoProof, SBoolean, SigSerializer, UncheckedTree}

class SigSerializerSpecification extends PropSpec
  with Matchers {

  private def roundTrip(uncheckedTree: UncheckedTree, exp: Value[SBoolean.type]): Assertion = {
    val bytes = SigSerializer.toBytes(uncheckedTree)
    val parsedUncheckedTree = SigSerializer.parse(exp, bytes)
    parsedUncheckedTree shouldEqual uncheckedTree
  }

  property("SigSerializer no proof round trip") {
    roundTrip(NoProof, TrueLeaf)
  }

}
