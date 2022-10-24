package sigmastate.crypto

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("sigmajs-crypto-facade", "CryptoContext")
class CryptoContextJs() extends js.Object {
  def getModulus(): js.BigInt = js.native

  def getOrder(): js.BigInt = js.native

  def validatePoint(x: js.BigInt, y: js.BigInt): Ecp = js.native

//  def getInfinity(): Ecp = js.native
//
  def decodePoint(encoded: js.Array[Byte]): Ecp = js.native
//
//  def getGenerator: Ecp = js.native
}

