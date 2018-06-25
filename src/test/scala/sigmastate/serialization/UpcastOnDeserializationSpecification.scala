package sigmastate.serialization

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.Values.{SValue, Value}
import sigmastate._
import sigmastate.lang.SigmaTyper
import sigmastate.serialization.generators.ValueGenerators

class UpcastOnDeserializationSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  private def numExprTreeNodeGen: Gen[Value[SNumericType]] = for {
    left <- exprTreeGen
    right <- exprTreeGen
    node <- Gen.oneOf(
      Plus(left, right),
      Minus(left, right)
    )
  } yield node

  private def exprTreeGen: Gen[SValue] =
    Gen.oneOf(arbByteConstants.arbitrary,
      arbIntConstants.arbitrary,
      arbLongConstants.arbitrary,
      Gen.delay(numExprTreeNodeGen))

  private def roundTrip(tree: SValue): Assertion = {
    val typedTree = (new SigmaTyper).typecheck(tree)
    val bytes = ValueSerializer.serialize(typedTree)
    val parsedTree = ValueSerializer.deserialize(bytes)
    parsedTree shouldEqual typedTree
  }

  property("Upcast deserialization round trip") {
    forAll(exprTreeGen) { expr =>
      roundTrip(expr)
    }
  }
}
