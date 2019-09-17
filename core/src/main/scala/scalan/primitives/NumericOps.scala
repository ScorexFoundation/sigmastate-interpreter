package scalan.primitives

import scalan.{ExactNumeric, Base, Scalan, ExactIntegral}

trait NumericOps extends Base { self: Scalan =>
  implicit class NumericOpsCls[T](x: Ref[T])(implicit val n: ExactNumeric[T]) {
    def +(y: Ref[T]): Ref[T] = NumericPlus(n)(x.elem).apply(x, y)
    def -(y: Ref[T]): Ref[T] = NumericMinus(n)(x.elem).apply(x, y)
    def *(y: Ref[T]): Ref[T] = NumericTimes(n)(x.elem).apply(x, y)
    def unary_- : Ref[T] = NumericNegate(n)(x.elem).apply(x)
    def abs: Ref[T] = Abs(n)(x.elem).apply(x)
    def toFloat: Ref[Float] = NumericToFloat(n).apply(x)
    def toDouble: Ref[Double] = NumericToDouble(n).apply(x)
    def toInt: Ref[Int] = NumericToInt(n).apply(x)
    def toLong: Ref[Long] = NumericToLong(n).apply(x)
  }

  implicit class IntegralOpsCls[T](x: Ref[T])(implicit i: ExactIntegral[T]) {
    def div(y: Ref[T]): Ref[T] = IntegralDivide(i)(x.elem).apply(x, y)
    def mod(y: Ref[T]): Ref[T] = IntegralMod(i)(x.elem).apply(x, y)
    // avoid / due to conflicts
    def /!(y: Ref[T]): Ref[T] = div(y)
    def %(y: Ref[T]): Ref[T] = mod(y)
  }

  def numeric[T:ExactNumeric]: ExactNumeric[T] = implicitly[ExactNumeric[T]]
  def integral[T:ExactIntegral]: ExactIntegral[T] = implicitly[ExactIntegral[T]]

  case class NumericPlus[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("+", n.plus)

  case class NumericMinus[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("-", n.minus)

  case class NumericTimes[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("*", n.times)

  class DivOp[T: Elem](opName: String, applySeq: (T, T) => T, n: ExactIntegral[T]) extends EndoBinOp[T](opName, applySeq) {
    override def shouldPropagate(lhs: T, rhs: T) = rhs != n.zero
  }

  case class NumericNegate[T: Elem](n: ExactNumeric[T]) extends UnOp[T, T]("-", n.negate)

  case class NumericToDouble[T](n: ExactNumeric[T]) extends UnOp[T,Double]("ToDouble", n.toDouble)

  case class NumericToFloat[T](n: ExactNumeric[T]) extends UnOp[T, Float]("ToFloat", n.toFloat)

  case class NumericToInt[T](n: ExactNumeric[T]) extends UnOp[T,Int]("ToInt", n.toInt)

  case class NumericToLong[T](n: ExactNumeric[T]) extends UnOp[T,Long]("ToLong", n.toLong)

  case class Abs[T: Elem](n: ExactNumeric[T]) extends UnOp[T, T]("Abs", n.abs)

  case class IntegralDivide[T](i: ExactIntegral[T])(implicit elem: Elem[T]) extends DivOp[T]("/", i.quot, i)

  case class IntegralMod[T](i: ExactIntegral[T])(implicit elem: Elem[T]) extends DivOp[T]("%", i.rem, i)

  @inline final def isZero[T](x: T, n: ExactNumeric[T]) = x == n.zero
  @inline final def isOne[T](x: T, n: ExactNumeric[T]) = x == n.fromInt(1)
}
