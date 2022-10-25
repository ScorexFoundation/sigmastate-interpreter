package sigmastate.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

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

