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

  case class NumericPlus[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("+") {
    override def applySeq(x: T, y: T): T = n.plus(x, y)
  }

  case class NumericMinus[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("-") {
    override def applySeq(x: T, y: T): T = n.minus(x, y)
  }

  case class NumericTimes[T: Elem](n: ExactNumeric[T]) extends EndoBinOp[T]("*") {
    override def applySeq(x: T, y: T): T = n.times(x, y)
  }

  abstract class DivOp[T: Elem](opName: String, n: ExactIntegral[T]) extends EndoBinOp[T](opName) {
    override def shouldPropagate(lhs: T, rhs: T) = rhs != n.zero
  }

  case class NumericNegate[T: Elem](n: ExactNumeric[T]) extends UnOp[T, T]("-") {
    override def applySeq(x: T): T = n.negate(x)
  }

  case class NumericToDouble[T](n: ExactNumeric[T]) extends UnOp[T,Double]("ToDouble") {
    override def applySeq(x: T): Double = n.toDouble(x)
  }

  case class NumericToFloat[T](n: ExactNumeric[T]) extends UnOp[T, Float]("ToFloat") {
    override def applySeq(x: T): Float = n.toFloat(x)
  }

  case class NumericToInt[T](n: ExactNumeric[T]) extends UnOp[T,Int]("ToInt") {
    override def applySeq(x: T): Int = n.toInt(x)
  }

  case class NumericToLong[T](n: ExactNumeric[T]) extends UnOp[T,Long]("ToLong") {
    override def applySeq(x: T): Long = n.toLong(x)
  }

  case class Abs[T: Elem](n: ExactNumeric[T]) extends UnOp[T, T]("Abs") {
    override def applySeq(x: T): T = n.abs(x)
  }

  case class IntegralDivide[T](i: ExactIntegral[T])(implicit elem: Elem[T]) extends DivOp[T]("/", i) {
    override def applySeq(x: T, y: T): T = i.quot(x, y)
  }

  case class IntegralMod[T](i: ExactIntegral[T])(implicit elem: Elem[T]) extends DivOp[T]("%", i) {
    override def applySeq(x: T, y: T): T = i.rem(x, y)
  }

  @inline final def isZero[T](x: T, n: ExactNumeric[T]) = x == n.zero
  @inline final def isOne[T](x: T, n: ExactNumeric[T]) = x == n.fromInt(1)
}
