package sigmastate.crypto

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}
import sigmastate.crypto

import java.math.BigInteger

/** JVM specific implementation of crypto methods*/
object Platform {
  def getXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getXCoord)
  def getYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getYCoord)
  def getAffineXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineXCoord)
  def getAffineYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineYCoord)

  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = p.value.getEncoded

  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = p.value.testBitZero()

  def normalizePoint(p: Ecp): Ecp = Ecp(p.value.normalize())

  def showPoint(p: Ecp): String = {
    val rawX = p.value.getRawXCoord.toString.substring(0, 6)
    val rawY = p.value.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }

  def addPoint(p1: Ecp, p2: Ecp): Ecp = Ecp(p1.value.add(p2.value))

  def multiplyPoint(p: Ecp, n: BigInteger): Ecp = Ecp(p.value.multiply(n))

  def isInfinityPoint(p: Ecp): Boolean = p.value.isInfinity

  def negatePoint(p: Ecp): Ecp = Ecp(p.value.negate())

  /** Opaque point type. */
  case class Ecp(private[crypto] val value: ECPoint)

  case class ECFieldElem(value: ECFieldElement)

  def createContext(): CryptoContext = new CryptoContextJvm(CustomNamedCurves.getByName("secp256k1"))

}
