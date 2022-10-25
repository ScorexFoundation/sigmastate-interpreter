package sigmastate.crypto

import java.math.BigInteger
import scala.scalajs.js

/** JVM specific implementation of crypto methods*/
object Platform {
  def getXCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getXCoord)
  def getYCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getYCoord)
  def getAffineXCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getAffineXCoord)
  def getAffineYCoord(p: Ecp): ECFieldElem = ??? //ECFieldElem(p.value.getAffineYCoord)

  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = ??? //p.value.getEncoded

  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = ??? //p.value.testBitZero()

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

  class ECFieldElement

  /** Opaque point type. */
  @js.native
  trait Ecp extends js.Object {
    def x: js.BigInt = js.native
    def y: js.BigInt = js.native
    def toHex(b: Boolean): String = js.native
  }

  case class ECFieldElem(value: ECFieldElement)

  def createContext(): CryptoContext = ??? //new CryptoContextJvm(CustomNamedCurves.getByName("secp256k1"))

}
