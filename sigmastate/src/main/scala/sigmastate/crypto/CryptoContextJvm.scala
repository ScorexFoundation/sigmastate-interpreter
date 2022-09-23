package sigmastate.crypto

import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.math.ec.ECPoint

import java.math.BigInteger

class CryptoContextJvm(x9params: X9ECParameters) extends CryptoContext {
  lazy val curve = x9params.getCurve

  override def getModulus: BigInteger = curve.getField.getCharacteristic
  override def getOrder: BigInteger = x9params.getN
  override def getGenerator: Ecp = {
    Platform.Ecp(x9params.getG)
  }
  override def validatePoint(x: BigInteger, y: BigInteger): Ecp = {
    Platform.Ecp(curve.validatePoint(x, y))
  }

  override def getInfinity(): Ecp = {
    Platform.Ecp(curve.getInfinity)
  }

  override def decodePoint(encoded: Array[Byte]): Ecp = {
    Platform.Ecp(curve.decodePoint(encoded))
  }
}
