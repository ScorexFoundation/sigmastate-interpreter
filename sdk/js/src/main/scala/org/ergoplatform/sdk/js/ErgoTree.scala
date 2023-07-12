package org.ergoplatform.sdk.js

import scorex.util.encode.Base16
import sigmastate.Values
import sigmastate.serialization.ErgoTreeSerializer

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.annotation.JSExportTopLevel

/** An exported JavaScript class wrapping the Scala `Values.ErgoTree` type. */
@JSExportTopLevel("ErgoTree")
class ErgoTree(tree: Values.ErgoTree) extends js.Object {
  /** The first byte of serialized byte array which determines interpretation of the rest of the array. */
  def header(): Byte = tree.header

  def version(): Byte = tree.version

  def isConstantSegregation(): Boolean = tree.isConstantSegregation

  def hasSize(): Boolean = tree.hasSize

  /** Serializes the ErgoTree instance to an array of bytes. */
  def bytes(): Array[Byte] = tree.bytes

  /** Serializes the ErgoTree instance to a hexadecimal string. */
  def toHex(): String = tree.bytesHex

  /** Returns segregated constants of this tree as [[js.Array]]. */
  def constants(): js.Array[Value] = {
    val constants = tree.constants
    val values = constants.map(Isos.isoValueToConstant.from)
    values.toJSArray
  }
}

/** An exported JavaScript object providing utility methods for working with ErgoTree instances. */
@JSExportTopLevel("ErgoTreeObj")
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
