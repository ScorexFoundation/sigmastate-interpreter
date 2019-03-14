package special.sigma

import org.bouncycastle.math.ec.ECPoint
import special.collection.Coll

abstract class TestGroupElement(private[sigma] val value: ECPoint) extends GroupElement {
  val dsl: TestSigmaDslBuilder = new TestSigmaDslBuilder

  override def toString: String = s"GroupElement(${Extensions.showECPoint(value)})"

  override def isInfinity: Boolean = value.isInfinity

  override def exp(k: BigInt): GroupElement = dsl.GroupElement(value.multiply(k.value))

  override def multiply(that: GroupElement): GroupElement = dsl.GroupElement(value.add(that.value))

  override def negate: GroupElement = dsl.GroupElement(value.negate())

}
