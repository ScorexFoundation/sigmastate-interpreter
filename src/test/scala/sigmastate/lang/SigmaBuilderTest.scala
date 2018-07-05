package sigmastate.lang

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.exceptions.ConstraintFailed
import sigmastate.serialization.OpCodes

class SigmaBuilderTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  property("StdSigmaBuilder construct nodes") {
    StdSigmaBuilder.EQ(TrueLeaf, FalseLeaf) shouldEqual EQ(TrueLeaf, FalseLeaf)
    StdSigmaBuilder.NEQ(TrueLeaf, FalseLeaf) shouldEqual NEQ(TrueLeaf, FalseLeaf)

    StdSigmaBuilder.GT(IntConstant(1), IntConstant(1)) shouldEqual GT(IntConstant(1), IntConstant(1))
    StdSigmaBuilder.GE(IntConstant(1), IntConstant(1)) shouldEqual GE(IntConstant(1), IntConstant(1))
    StdSigmaBuilder.LT(IntConstant(1), IntConstant(1)) shouldEqual LT(IntConstant(1), IntConstant(1))
    StdSigmaBuilder.LE(IntConstant(1), IntConstant(1)) shouldEqual LE(IntConstant(1), IntConstant(1))

    StdSigmaBuilder.Plus(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.PlusCode)
    StdSigmaBuilder.Minus(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.MinusCode)
    StdSigmaBuilder.Multiply(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.MultiplyCode)
    StdSigmaBuilder.Divide(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.DivisionCode)
    StdSigmaBuilder.Modulo(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.ModuloCode)
  }

  property("TransformingSigmaBuilder apply upcast") {
    TransformingSigmaBuilder.EQ(LongConstant(1), IntConstant(1)) shouldEqual
      EQ(LongConstant(1), Upcast(IntConstant(1), SLong))
    TransformingSigmaBuilder.NEQ(LongConstant(1), IntConstant(1)) shouldEqual
      NEQ(LongConstant(1), Upcast(IntConstant(1), SLong))

    TransformingSigmaBuilder.GT(LongConstant(1), IntConstant(1)) shouldEqual
      GT(LongConstant(1), Upcast(IntConstant(1), SLong))
    TransformingSigmaBuilder.GE(LongConstant(1), IntConstant(1)) shouldEqual
      GE(LongConstant(1), Upcast(IntConstant(1), SLong))
    TransformingSigmaBuilder.LT(LongConstant(1), IntConstant(1)) shouldEqual
      LT(LongConstant(1), Upcast(IntConstant(1), SLong))
    TransformingSigmaBuilder.LE(LongConstant(1), IntConstant(1)) shouldEqual
      LE(LongConstant(1), Upcast(IntConstant(1), SLong))

    TransformingSigmaBuilder.Plus(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.PlusCode)
    TransformingSigmaBuilder.Minus(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.MinusCode)
    TransformingSigmaBuilder.Multiply(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.MultiplyCode)
    TransformingSigmaBuilder.Divide(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.DivisionCode)
    TransformingSigmaBuilder.Modulo(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.ModuloCode)
  }

  property("CheckingSigmaBuilder failing constraint") {
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.EQ(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.NEQ(LongConstant(1), IntConstant(1))

    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.GT(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.GE(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.LT(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.LE(LongConstant(1), IntConstant(1))

    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.Plus(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.Minus(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.Multiply(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.Divide(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy CheckingSigmaBuilder.Modulo(LongConstant(1), IntConstant(1))
  }
}
