package special.sigma

import scalan.RType

case class TestValue[A](val value: A)(implicit val tA: RType[A]) extends AnyValue {
  def dataSize = SigmaPredef.dataSize(value)
  override def toString = s"Value($value)"
}

