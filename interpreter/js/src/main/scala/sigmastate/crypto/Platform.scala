package sigmastate.crypto

import java.math.BigInteger

/** JVM specific implementation of crypto methods*/
object Platform {
  /** Description of elliptic curve of point `p` which belongs to the curve.
    *
    * @param p the elliptic curve point
    */
  def getCurve(p: Ecp): Curve = ???

  def getXCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getXCoord)
  def getYCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getYCoord)
  def getAffineXCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getAffineXCoord)
  def getAffineYCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getAffineYCoord)

  def encodeFieldElem(p: ECFieldElem): Array[Byte] = ??? //p.value.getEncoded

  /** Byte representation of the given point.
    *
    * @param p point to encode
    * @param compressed if true, generates a compressed point encoding
    */
  def encodePoint(p: Ecp, compressed: Boolean): Array[Byte] = ???

  def signOf(p: ECFieldElem): Boolean = ??? //p.value.testBitZero()

  def normalizePoint(p: Ecp): Ecp = ??? //Ecp(p.value.normalize())

  def showPoint(p: Ecp): String = ??? /*{
    val rawX = p.value.getRawXCoord.toString.substring(0, 6)
    val rawY = p.value.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }*/

  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     */
    ??? //Ecp(p1.value.add(p2.value))
  }

  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     * Therefore, exponentiate point is multiply.
     */
    ??? //Ecp(p.value.multiply(n))
  }

  def isInfinityPoint(p: Ecp): Boolean = ??? //p.value.isInfinity

  def negatePoint(p: Ecp): Ecp = ??? //Ecp(p.value.negate())

  class Curve
  class ECPoint
  class ECFieldElement
  /** Opaque point type. */
  case class Ecp(private[crypto] val value: ECPoint)

  case class ECFieldElem(value: ECFieldElement)

  def createContext(): CryptoContext = ??? //new CryptoContextJvm(CustomNamedCurves.getByName("secp256k1"))

}
