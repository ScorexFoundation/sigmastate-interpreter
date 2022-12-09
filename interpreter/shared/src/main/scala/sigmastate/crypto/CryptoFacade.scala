package sigmastate.crypto

import java.math.BigInteger
import scala.util.Random

object CryptoFacade {
  val Encoding = "UTF-8"

  /** part of the protocol, do not change */
  val SecretKeyLength = 32

  /** Used as the key parameter of hashHmacSHA512 */
  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  val Pbkdf2Algorithm = "PBKDF2WithHmacSHA512"

  val Pbkdf2Iterations = 2048 // number of iteration specified in BIP39 standard.

  val Pbkdf2KeyLength = 512

  def createCryptoContext(): CryptoContext = Platform.createContext()
  def normalizePoint(p: Ecp): Ecp = Platform.normalizePoint(p)
  def negatePoint(p: Ecp): Ecp = Platform.negatePoint(p)
  def isInfinityPoint(p: Ecp): Boolean = Platform.isInfinityPoint(p)
  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = Platform.exponentiatePoint(p, n)
  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = Platform.multiplyPoints(p1, p2)
  def showPoint(p: Ecp): String = Platform.showPoint(p)
  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = Platform.testBitZeroOfFieldElem(p)
  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = Platform.getEncodedOfFieldElem(p)
  def getEncodedPoint(p: Ecp, compressed: Boolean): Array[Byte] = Platform.getEncodedPoint(p, compressed)
  def getXCoord(p: Ecp): ECFieldElem = Platform.getXCoord(p)
  def getYCoord(p: Ecp): ECFieldElem = Platform.getYCoord(p)
  def getAffineXCoord(p: Ecp): ECFieldElem = Platform.getAffineXCoord(p)
  def getAffineYCoord(p: Ecp): ECFieldElem = Platform.getAffineYCoord(p)
  def createSecureRandom(): Random = Platform.createSecureRandom()
  def hashHmacSHA512(key: Array[Byte], data: Array[Byte]): Array[Byte] = Platform.hashHmacSHA512(key, data)
  def generatePbkdf2Key(normalizedMnemonic: String, normalizedPass: String): Array[Byte] = Platform.generatePbkdf2Key(normalizedMnemonic, normalizedPass)
  /** Normalize a sequence of char values using NFKD normalization form. */
  def normalizeChars(chars: Array[Char]): String = Platform.normalizeChars(chars)
}
