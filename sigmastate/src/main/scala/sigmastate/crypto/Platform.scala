package sigmastate.crypto

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}

import java.math.BigInteger

/** JVM specific implementation of crypto methods*/
object Platform {
  /** Returns the x-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalize() to get a point where the coordinates have their
    * affine values, or use getAffineXCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the x-coordinate of this point
    */
  def getXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getXCoord)

  /** Returns the y-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalize() to get a point where the coordinates have their
    * affine values, or use getAffineYCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the y-coordinate of this point
    */
  def getYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getYCoord)

  /** Returns the affine x-coordinate after checking that this point is normalized.
    *
    * @return The affine x-coordinate of this point
    * @throws IllegalStateException if the point is not normalized
    */
  def getAffineXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineXCoord)

  /** Returns the affine y-coordinate after checking that this point is normalized
    *
    * @return The affine y-coordinate of this point
    * @throws IllegalStateException if the point is not normalized
    */
  def getAffineYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineYCoord)

  /** Returns byte representation of the given field element. */
  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = p.value.getEncoded

  /** Returns the value of bit 0 in BigInteger representation of this point. */
  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = p.value.testBitZero()

  /** * Normalization ensures that any projective coordinate is 1, and therefore that the x, y
    * coordinates reflect those of the equivalent point in an affine coordinate system.
    *
    * @return a new ECPoint instance representing the same point, but with normalized coordinates
    */
  def normalizePoint(p: Ecp): Ecp = Ecp(p.value.normalize())

  /** Return simplified string representation of the point (used only for debugging) */
  def showPoint(p: Ecp): String = {
    val rawX = p.value.getRawXCoord.toString.substring(0, 6)
    val rawY = p.value.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }

  /** Multiply two points.
    * @param p1 first point
    * @param p2 second point
    * @return group multiplication (p1 * p2)
    */
  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     */
    Ecp(p1.value.add(p2.value))
  }

  /** Exponentiate a point.
    * @param p point to exponentiate
    * @param n exponent
    * @return p to the power of n (p^n)
    */
  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     * Therefore, exponentiate point is multiply.
     */
    Ecp(p.value.multiply(n))
  }

  /** Check if a point is infinity. */
  def isInfinityPoint(p: Ecp): Boolean = p.value.isInfinity

  /** Negate a point. */
  def negatePoint(p: Ecp): Ecp = Ecp(p.value.negate())

  /** Wrapper for point type. */
  case class Ecp(private[crypto] val value: ECPoint)

  /** Wrapper for field element type. */
  case class ECFieldElem(value: ECFieldElement)

  /** Create a new context for cryptographic operations. */
  def createContext(): CryptoContext = new CryptoContextJvm(CustomNamedCurves.getByName("secp256k1"))

}
