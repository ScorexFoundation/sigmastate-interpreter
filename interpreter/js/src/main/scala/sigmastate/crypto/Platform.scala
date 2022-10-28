package sigmastate.crypto

import scorex.util.encode.Base16
import sigmastate.crypto

import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Random

/** JVM specific implementation of crypto methods*/
object Platform {
  def getXCoord(p: Ecp): ECFieldElem = CryptoFacadeJs.getXCoord(p)
  def getYCoord(p: Ecp): ECFieldElem = CryptoFacadeJs.getYCoord(p)
  def getAffineXCoord(p: Ecp): ECFieldElem = CryptoFacadeJs.getAffineXCoord(p)
  def getAffineYCoord(p: Ecp): ECFieldElem = CryptoFacadeJs.getAffineYCoord(p)

  def Uint8ArrayToBytes(jsShorts: Uint8Array): Array[Byte] = {
    jsShorts.toArray[Short].map(x => x.toByte)
  }

  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = {
    Uint8ArrayToBytes(CryptoFacadeJs.getEncodedOfFieldElem(p))
  }

  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = CryptoFacadeJs.testBitZeroOfFieldElem(p)

  def normalizePoint(p: Ecp): Ecp = CryptoFacadeJs.normalizePoint(p)

  def showPoint(p: Ecp): String = CryptoFacadeJs.showPoint(p)

  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = CryptoFacadeJs.addPoint(p1, p2)

  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    val scalar = Convert.bigIntegerToBigInt(n)
    CryptoFacadeJs.multiplyPoint(p, scalar)
  }

  def isInfinityPoint(p: Ecp): Boolean = CryptoFacadeJs.isInfinityPoint(p)

  def negatePoint(p: Ecp): Ecp = CryptoFacadeJs.negatePoint(p)

  /** Opaque point type. */
  @js.native
  trait Ecp extends js.Object {
    def x: js.BigInt = js.native
    def y: js.BigInt = js.native
    def toHex(b: Boolean): String = js.native
  }

  type ECFieldElem = js.BigInt

  object Convert {
    def bigIntToBigInteger(jsValue: js.BigInt): BigInteger = {
      new BigInteger(jsValue.toString(10), 10)
    }

    def bigIntegerToBigInt(value: BigInteger): js.BigInt = {
      js.BigInt(value.toString(10))
    }
  }

  def createContext(): CryptoContext = new CryptoContext {
    val ctx = new CryptoContextJs

    override def getModulus: BigInteger = Convert.bigIntToBigInteger(ctx.getModulus())

    override def getOrder: BigInteger = Convert.bigIntToBigInteger(ctx.getOrder())

    override def validatePoint(x: BigInteger, y: BigInteger): crypto.Ecp = {
      ctx.validatePoint(Convert.bigIntegerToBigInt(x), Convert.bigIntegerToBigInt(y))
    }

    override def getInfinity(): crypto.Ecp = ctx.getInfinity()

    override def decodePoint(encoded: Array[Byte]): crypto.Ecp =
      ctx.decodePoint(Base16.encode(encoded))

    override def getGenerator: crypto.Ecp = ctx.getGenerator()
  }

  def createSecureRandom(): Random = new Random()

}
