package special.sigma

import scalan.OverloadHack.Overloaded1
import scalan.RType

case class TestValue[A](value: A, tVal: RType[Any]) extends AnyValue {
  def tA: RType[A] = tVal.asInstanceOf[RType[A]]
  override def toString = s"Value($value)"
}

object TestValue {
  def apply[A](value: A, t: RType[A])(implicit o: Overloaded1): TestValue[A] =
    new TestValue(value, t.asInstanceOf[RType[Any]])
}
