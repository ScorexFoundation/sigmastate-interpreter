package special.sigma

import org.bouncycastle.crypto.ec.CustomNamedCurves
import scalan.{NeverInline, OverloadId}
import special.collection.Builder.DefaultCollBuilder
import special.collection.{Coll, Builder}

/**NOTE: this should extend SigmaProp because semantically it subclass of SigmaProp
  * and DefaultSigma is used just to mixin implementations. */

case class MockSigma(val _isValid: Boolean) extends SigmaProp {

  def propBytes: Coll[Byte] = DefaultCollBuilder.fromItems(if(isValid) 1 else 0)
  def isValid: Boolean = _isValid
  def &&(other: SigmaProp): SigmaProp = MockSigma(isValid && other.isValid)

  @NeverInline
  @OverloadId("and_bool")
  def &&(other: Boolean): SigmaProp = MockSigma(isValid && other)

  @NeverInline
  @OverloadId("or_sigma")
  def ||(other: SigmaProp): SigmaProp = MockSigma(isValid || other.isValid)

  @NeverInline
  @OverloadId("or_bool")
  def ||(other: Boolean): SigmaProp = MockSigma(isValid || other)
  
}

case class MockProveDlog(var isValid: Boolean, val propBytes: Coll[Byte]) extends SigmaProp {
  val curve = CustomNamedCurves.getByName("curve25519")
  def value = curve.getG
  def setValid(v: Boolean) = { isValid = v }
  def builder = new TestSigmaDslBuilder
  @NeverInline
  @OverloadId("and_sigma")
  def &&(other: SigmaProp): SigmaProp = MockSigma(isValid && other.isValid)

  @NeverInline
  @OverloadId("and_bool")
  def &&(other: Boolean): SigmaProp = MockSigma(isValid && other)

  @NeverInline
  @OverloadId("or_sigma")
  def ||(other: SigmaProp): SigmaProp = MockSigma(isValid || other.isValid)

  @NeverInline
  @OverloadId("or_bool")
  def ||(other: Boolean): SigmaProp = MockSigma(isValid || other)
}

