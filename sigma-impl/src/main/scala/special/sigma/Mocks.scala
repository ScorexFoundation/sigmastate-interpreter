package special.sigma

import org.bouncycastle.crypto.ec.CustomNamedCurves
import special.collection.Coll

class MockProveDlog(var isValid: Boolean, val propBytes: Coll[Byte]) extends DefaultSigma {
  val curve = CustomNamedCurves.getByName("curve25519")
  def value = curve.getG
  def setValid(v: Boolean) = { isValid = v }
}

