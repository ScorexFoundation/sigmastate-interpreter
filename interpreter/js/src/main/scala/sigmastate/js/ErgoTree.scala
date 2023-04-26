package sigmastate.js

import scorex.util.encode.Base16
import sigmastate.Values
import sigmastate.serialization.ErgoTreeSerializer

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ErgoTree")
class ErgoTree(tree: Values.ErgoTree) extends js.Object {
  def toBytes(): Array[Byte] = {
    val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    bytes
  }
  def toHex(): String = {
    Base16.encode(toBytes())
  }
}

@JSExportTopLevel("ErgoTrees")
object ErgoTree extends js.Object {

  def fromHex(hex: String): ErgoTree = {
    val bytes = Base16.decode(hex).get
    fromBytes(bytes)
  }

  def fromBytes(bytes: Array[Byte]): ErgoTree = {
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    new ErgoTree(tree)
  }
}
