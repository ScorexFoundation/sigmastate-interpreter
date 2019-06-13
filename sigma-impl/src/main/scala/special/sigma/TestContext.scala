package special.sigma

import scalan.RType

case class TestValue[A](value: A, tVal: RType[Any]) extends AnyValue {
  def tA: RType[A] = tVal.asInstanceOf[RType[A]]
  override def toString = s"Value($value)"
}

