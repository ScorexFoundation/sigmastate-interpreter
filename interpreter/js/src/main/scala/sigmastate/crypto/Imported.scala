package sigmastate.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("sigmajs-crypto-facade", "CryptoContext")
class CryptoContextJs() extends js.Object {
  def getModulus(): js.BigInt = js.native
  def getOrder(): js.BigInt = js.native
  def validatePoint(x: js.BigInt, y: js.BigInt): Platform.Ecp = js.native
  def getInfinity(): Platform.Ecp = js.native
  def decodePoint(encoded: String): Platform.Ecp = js.native
  def getGenerator(): Platform.Ecp = js.native
}

@js.native
@JSImport("sigmajs-crypto-facade", "CryptoFacade")
object CryptoFacadeJs extends js.Object {
  def normalizePoint(point: Platform.Ecp): Platform.Ecp = js.native

  def createCryptoContext(): CryptoContextJs = js.native

  def negatePoint(point: Platform.Ecp): Platform.Ecp = js.native 

  def isInfinityPoint(point: Platform.Ecp): Boolean = js.native 

  def multiplyPoint(point: Platform.Ecp, scalar: js.BigInt): Platform.Ecp = js.native

  def addPoint(point1: Platform.Ecp, point2: Platform.Ecp): Platform.Ecp = js.native

  def showPoint(point: Platform.Ecp): String = js.native

  def testBitZeroOfFieldElem(element: js.BigInt): Boolean = js.native

  def getEncodedOfFieldElem(element: js.BigInt): Uint8Array = js.native

  def getXCoord(point: Platform.Ecp): js.BigInt = js.native

  def getYCoord(point: Platform.Ecp): js.BigInt = js.native

  def getAffineXCoord(point: Platform.Ecp): js.BigInt = js.native

  def getAffineYCoord(point: Platform.Ecp): js.BigInt = js.native
}

@js.native
@JSImport("@noble/secp256k1", "Point")
object Point extends js.Any {
  def fromHex(hex: String): Platform.Ecp = js.native
  def ZERO: Platform.Ecp = js.native
}

@js.native
@JSImport("@noble/secp256k1", "utils")
object utils extends js.Any {
  def bytesToHex(bytes: Uint8Array): String = js.native
  def hexToBytes(hex: String): Uint8Array = js.native
}