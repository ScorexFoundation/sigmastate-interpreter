package sigma.js

import sigma.data.{ProveDlog, SigmaBoolean}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.SigmaProp]] available from JS. */
@JSExportTopLevel("SigmaProp")
class SigmaProp(val sigmaBoolean: SigmaBoolean) extends js.Object {
}

@JSExportTopLevel("SigmaPropObj")
object SigmaProp extends js.Object {
  /** Creates a new [[SigmaProp]] from the given hex string of public key.
    * @param pointHex  hex representation of elliptic curve point (ASN.1 encoded)
    * @see CryptoFacade.getASN1Encoding, GroupElement.fromPointHex, Point
    */
  def fromPointHex(pointHex: String): SigmaProp = {
    val point = GroupElement.fromPointHex(pointHex).point
    new SigmaProp(ProveDlog(point))
  }
}
