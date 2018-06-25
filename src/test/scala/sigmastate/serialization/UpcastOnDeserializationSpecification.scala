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
    left <- numExprTreeGen
    right <- numExprTreeGen
    node <- Gen.oneOf(
      Plus(left, right),
      Minus(left, right),
      Multiply(left, right),
      Divide(left, right),
      Modulo(left, right)
    )
  } yield node

  private def numExprTreeGen: Gen[Value[SNumericType]] =
    Gen.oneOf(arbByteConstants.arbitrary,
      arbIntConstants.arbitrary,
      arbLongConstants.arbitrary,
      arbBigIntConstant.arbitrary,
      Gen.delay(numExprTreeNodeGen))

  private def comparisonExprTreeNodeGen: Gen[Value[SBoolean.type]] = for {
    left <- numExprTreeNodeGen
    right <- numExprTreeNodeGen
    node <- Gen.oneOf(
      EQ(left, right),
      NEQ(left, right),
      LE(left, right),
      GE(left, right),
      LT(left, right),
      GT(left, right)
    )
  } yield node

  private def roundTrip(tree: SValue): Assertion = {
    // add Upcast nodes
    val typedTree = (new SigmaTyper).typecheck(tree)
    val bytes = ValueSerializer.serialize(typedTree)
    val parsedTree = ValueSerializer.deserialize(bytes)
    parsedTree shouldEqual typedTree
  }

  property("Upcast deserialization round trip") {
    forAll(comparisonExprTreeNodeGen, minSuccessful(500)) { expr =>
      roundTrip(expr)
    }
  }
}
