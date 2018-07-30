package sigmastate.lang

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.exceptions.{ArithException, ConstraintFailed}
import sigmastate.serialization.OpCodes

class SigmaBuilderTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  property("StdSigmaBuilder construct nodes") {
    import StdSigmaBuilder._
    mkEQ(TrueLeaf, FalseLeaf) shouldEqual EQ(TrueLeaf, FalseLeaf)
    mkNEQ(TrueLeaf, FalseLeaf) shouldEqual NEQ(TrueLeaf, FalseLeaf)

    mkGT(IntConstant(1), IntConstant(1)) shouldEqual GT(IntConstant(1), IntConstant(1))
    mkGE(IntConstant(1), IntConstant(1)) shouldEqual GE(IntConstant(1), IntConstant(1))
    mkLT(IntConstant(1), IntConstant(1)) shouldEqual LT(IntConstant(1), IntConstant(1))
    mkLE(IntConstant(1), IntConstant(1)) shouldEqual LE(IntConstant(1), IntConstant(1))

    mkPlus(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.PlusCode)
    mkMinus(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.MinusCode)
    mkMultiply(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.MultiplyCode)
    mkDivide(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.DivisionCode)
    mkModulo(IntConstant(1), IntConstant(1)) shouldEqual
      ArithOp(IntConstant(1), IntConstant(1), OpCodes.ModuloCode)
  }

  property("TransformingSigmaBuilder apply upcast") {
    import TransformingSigmaBuilder._
    mkEQ(LongConstant(1), IntConstant(1)) shouldEqual EQ(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkNEQ(LongConstant(1), IntConstant(1)) shouldEqual NEQ(LongConstant(1), Upcast(IntConstant(1), SLong))

    mkGT(LongConstant(1), IntConstant(1)) shouldEqual GT(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkGE(LongConstant(1), IntConstant(1)) shouldEqual GE(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkLT(LongConstant(1), IntConstant(1)) shouldEqual LT(LongConstant(1), Upcast(IntConstant(1), SLong))
    mkLE(LongConstant(1), IntConstant(1)) shouldEqual LE(LongConstant(1), Upcast(IntConstant(1), SLong))

    mkPlus(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.PlusCode)
    mkMinus(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.MinusCode)
    mkMultiply(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.MultiplyCode)
    mkDivide(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.DivisionCode)
    mkModulo(LongConstant(1), IntConstant(1)) shouldEqual
      ArithOp(LongConstant(1), Upcast(IntConstant(1), SLong), OpCodes.ModuloCode)
  }

  property("CheckingSigmaBuilder failing constraint") {
    import CheckingSigmaBuilder._
    an[ConstraintFailed] should be thrownBy mkEQ(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkNEQ(LongConstant(1), IntConstant(1))

    an[ConstraintFailed] should be thrownBy mkGT(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkGE(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkLT(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkLE(LongConstant(1), IntConstant(1))

    an[ConstraintFailed] should be thrownBy mkPlus(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkMinus(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkMultiply(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkDivide(LongConstant(1), IntConstant(1))
    an[ConstraintFailed] should be thrownBy mkModulo(LongConstant(1), IntConstant(1))

    an[ArithException] should be thrownBy mkIntToByte(IntConstant(129))
  }
}
