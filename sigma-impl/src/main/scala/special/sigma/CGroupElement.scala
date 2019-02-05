package special.sigma

import org.bouncycastle.math.ec.ECPoint
import special.collection.Coll

class CGroupElement(private[sigma] val value: ECPoint) extends GroupElement {
  val dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  @inline implicit def adaptBigInt(x: GroupElement) = x.asInstanceOf[CGroupElement]

  override def isInfinity: Boolean = value.isInfinity

  override def multiply(k: BigInt): GroupElement = new CGroupElement(value.multiply(k.value))

  override def add(that: GroupElement): GroupElement = new CGroupElement(value.add(that.value))

  override def negate: GroupElement = new CGroupElement(value.negate())

  override def getEncoded(compressed: Boolean): Coll[Byte] = dsl.Colls.fromArray(value.getEncoded(compressed))
}
