package sigmastate.js

import scorex.util.encode.Base16
import sigmastate.Values
import sigmastate.serialization.ErgoTreeSerializer

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** An exported JavaScript class wrapping the Scala `Values.ErgoTree` type. */
@JSExportTopLevel("ErgoTree")
class ErgoTree(tree: Values.ErgoTree) extends js.Object {
  /** Serializes the ErgoTree instance to an array of bytes. */
  def toBytes(): Array[Byte] = {
    val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    bytes
  }

  /** Serializes the ErgoTree instance to a hexadecimal string. */
  def toHex(): String = {
    Base16.encode(toBytes())
  }
}

/** An exported JavaScript object providing utility methods for working with ErgoTree instances. */
@JSExportTopLevel("ErgoTrees")
object ErgoTree extends js.Object {

  /** Deserializes an ErgoTree instance from a hexadecimal string.
    *
    * @param hex a hexadecimal string representing the serialized ErgoTree
    */
  def fromHex(hex: String): ErgoTree = {
    val bytes = Base16.decode(hex).get
    fromBytes(bytes)
  }

  /** Deserializes an ErgoTree instance from an array of bytes.
    *
    * @param bytes an array of bytes representing the serialized ErgoTree
    */
  def fromBytes(bytes: Array[Byte]): ErgoTree = {
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    new ErgoTree(tree)
  }
}
