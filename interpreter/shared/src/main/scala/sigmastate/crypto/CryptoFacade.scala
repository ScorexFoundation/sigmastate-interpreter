package sigmastate.crypto

import java.math.BigInteger

/** A facade for cryptographic operations. The concrete implementation is delegated to the
  * Platform object, which is resolved by the compiler to either JVM or JS implementation.
  * Cross-platform code should use this facade instead of the Platform object directly.
  */
object CryptoFacade {
  /** Default encoding used for Strings. */
  val Encoding = "UTF-8"

  /** part of the protocol, do not change */
  val SecretKeyLength = 32

  /** Used as the key parameter of hashHmacSHA512 */
  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  /** Number of iteration specified in BIP39 standard. */
  val Pbkdf2Iterations = 2048

  /** The size of the key in bits. */
  val Pbkdf2KeyLength = 512

  /** Create a new context for cryptographic operations. */
  def createCryptoContext(): CryptoContext = Platform.createContext()

  /** Normalization ensures that any projective coordinate is 1, and therefore that the x, y
    * coordinates reflect those of the equivalent element in an affine coordinate system.
    *
    * @return a new ECPoint instance representing the same element, but with normalized coordinates
    */
  def normalize(e: Ecp): Ecp = Platform.normalize(e)

  /** Negates the given element by negating its y coordinate. */
  def inverse(e: Ecp): Ecp = Platform.inverse(e)

  /** Check if this is identity element. */
  def isIdentity(e: Ecp): Boolean = Platform.isIdentity(e)

  /** Exponentiate an element.
    *
    * @param e element to exponentiate
    * @param n exponent
    * @return p to the power of n (`p^n`) i.e. `p + p + ... + p` (n times)
    */
  def exponentiateElement(e: Ecp, n: BigInteger): Ecp = Platform.exponentiateElement(e, n)

  /** Multiply two points.
    *
    * @param e1 first element
    * @param e2 second element
    * @return group multiplication (e1 * e2)
    */
  def multiplyElements(e1: Ecp, e2: Ecp): Ecp = Platform.multiplyElements(e1, e2)

  /** Return simplified string representation of element (used only for debugging) */
  def showElement(e: Ecp): String = Platform.showElement(e)

  /** Returns the sign of the field element. */
  def signOf(p: ECFieldElem): Boolean = Platform.signOf(p)

  /** Returns byte representation of the given field element. */
  def encodeFieldElem(p: ECFieldElem): Array[Byte] = Platform.encodeFieldElem(p)

  /** Byte representation of the given point.
    *
    * ASN.1, short for Abstract Syntax Notation One, is a standard and notation that
    * describes data structures for representing, encoding, transmitting, and decoding
    * data.
    *
    * The ASN.1 encoding of EC point according to this standard can be one of two forms:
    *
    * Compressed form: This is a shorter form where only the x coordinate and a bit of
    * information about the y coordinate are stored. The full y coordinate can be
    * recalculated from this information. The compressed form begins with a byte value of
    * 02 or 03 (depending on the y coordinate), followed by the x coordinate.
    *
    * Uncompressed form: This is a longer form where both the x and y coordinates are
    * stored. The uncompressed form begins with a byte value of 04, followed by the x
    * coordinate, and then the y coordinate.
    *
    * NOTE, this encoding is not used in ErgoTree and not part of consensus, it is used for
    * extended keys (in the wallet) to ensure BIP32 compatibility.
    *
    * @param p          point to encode
    * @param compressed if true, generates a compressed point encoding
    */
  def getASN1Encoding(p: Ecp, compressed: Boolean): Array[Byte] = Platform.getASN1Encoding(p, compressed)

  /** A [[Curve]] instance describing the elliptic curve of the point p
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

  /** Create source of secure randomness. */
  def createSecureRandom(): SecureRandom = Platform.createSecureRandom()

  /** Computes HMAC-SHA512 hash of the given data using the specified key.
    *
    * @param key  the secret key used for hashing
    * @param data the input data to be hashed
    * @return a HMAC-SHA512 hash of the input data
    */
  def hashHmacSHA512(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    Platform.hashHmacSHA512(key, data)

  /** Generates PBKDF2 key from a mnemonic and passphrase using SHA512 digest. */
  def generatePbkdf2Key(normalizedMnemonic: String, normalizedPass: String): Array[Byte] =
    Platform.generatePbkdf2Key(normalizedMnemonic, normalizedPass)

  /** Normalize a sequence of char values using NFKD normalization form. */
  def normalizeChars(chars: Array[Char]): String = Platform.normalizeChars(chars)
}
