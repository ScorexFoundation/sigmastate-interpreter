package sigmastate.crypto

import java.math.BigInteger

/** A facade for cryptographic operations. The concrete implementation is delegated to the
  * Platform object, which is resolved by the compiler to either JVM or JS implementation.
  * Cross-platform code should use this facade instead of the Platform object directly.
  */
object CryptoFacade {
  /** Create a new context for cryptographic operations. */
  def createCryptoContext(): CryptoContext = Platform.createContext()

  /** * Normalization ensures that any projective coordinate is 1, and therefore that the x, y
    * coordinates reflect those of the equivalent point in an affine coordinate system.
    *
    * @return a new ECPoint instance representing the same point, but with normalized coordinates
    */
  def normalizePoint(p: Ecp): Ecp = Platform.normalizePoint(p)

  /** Negate a point. */
  def negatePoint(p: Ecp): Ecp = Platform.negatePoint(p)

  /** Check if a point is infinity. */
  def isInfinityPoint(p: Ecp): Boolean = Platform.isInfinityPoint(p)

  /** Exponentiate a point.
    *
    * @param p point to exponentiate
    * @param n exponent
    * @return p to the power of n (`p^n`)
    */
  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = Platform.exponentiatePoint(p, n)

  /** Multiply two points.
    *
    * @param p1 first point
    * @param p2 second point
    * @return group multiplication (p1 * p2)
    */
  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = Platform.multiplyPoints(p1, p2)

  /** Return simplified string representation of the point (used only for debugging) */
  def showPoint(p: Ecp): String = Platform.showPoint(p)

  /** Returns the sign of the field element. */
  def signOf(p: ECFieldElem): Boolean = Platform.signOf(p)

  /** Returns byte representation of the given field element. */
  def encodeFieldElem(p: ECFieldElem): Array[Byte] = Platform.encodeFieldElem(p)

  /** Returns byte representation of the given point.
    *
    * @param p          point to encode
    * @param compressed if true, generates a compressed point encoding
    */
  def encodePoint(p: Ecp, compressed: Boolean): Array[Byte] = Platform.encodePoint(p, compressed)

  /** Returns a [[Curve]] instance describing the elliptic curve of the point p
    *
    * @param p the elliptic curve point
    */
  def getCurve(p: Ecp): Curve = Platform.getCurve(p)

  /** Returns the x-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalize() to get a point where the coordinates have their
    * affine values, or use getAffineXCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the x-coordinate of this point
    */
  def getXCoord(p: Ecp): ECFieldElem = Platform.getXCoord(p)

  /** Returns the y-coordinate.
    *
    * Caution: depending on the curve's coordinate system, this may not be the same value as in an
    * affine coordinate system; use normalize() to get a point where the coordinates have their
    * affine values, or use getAffineYCoord() if you expect the point to already have been
    * normalized.
    *
    * @return the y-coordinate of this point
    */
  def getYCoord(p: Ecp): ECFieldElem = Platform.getYCoord(p)

  /** Returns the affine x-coordinate after checking that this point is normalized.
    *
    * @return The affine x-coordinate of this point
    * @throws IllegalStateException if the point is not normalized
    */
  def getAffineXCoord(p: Ecp): ECFieldElem = Platform.getAffineXCoord(p)

  /** Returns the affine y-coordinate after checking that this point is normalized
    *
    * @return The affine y-coordinate of this point
    * @throws IllegalStateException if the point is not normalized
    */
  def getAffineYCoord(p: Ecp): ECFieldElem = Platform.getAffineYCoord(p)
}
