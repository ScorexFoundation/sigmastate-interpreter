package org.ergoplatform.sdk.js

import sigmastate.crypto.{CryptoFacade, CryptoFacadeJs, Ecp, Platform}
import sigmastate.eval.Extensions.ArrayByteOps

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[special.sigma.GroupElement]] available from JS. */
@JSExportTopLevel("GroupElement")
class GroupElement(val point: Ecp) extends js.Object {
  /** Returns the point encoded as hex string (ASN.1 encoding).
    * @see CryptoFacade.getASN1Encoding
    */
  def toHex: String = {
    CryptoFacade.getASN1Encoding(point, true).toHex
  }
}

object GroupElement {
  /** Creates a new [[GroupElement]] from the given hex string (ASN.1 encoding)
    * representation of the underlying [[sigmastate.crypto.Platform.Point]].
    */
  def fromHex(hex: String): GroupElement = {
    val point = CryptoFacadeJs.createCryptoContext().decodePoint(hex)
    new GroupElement(new Platform.Ecp(point))
  }
}