package special.sigma

import scalan.OverloadHack.Overloaded1
import scalan.RType

// TODO refactor: move to sigmastate package and rename to CAnyValue

/** Default implementation of AnyValue interface. */
case class TestValue[A](value: A, tVal: RType[Any]) extends AnyValue {
  def tA: RType[A] = tVal.asInstanceOf[RType[A]]
  override def toString = s"TestValue($value)"
}

object TestValue {
  def apply[A](value: A, t: RType[A])(implicit o: Overloaded1): TestValue[A] =
    new TestValue(value, t.asInstanceOf[RType[Any]])
}
