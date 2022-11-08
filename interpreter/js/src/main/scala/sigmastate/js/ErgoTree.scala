package sigmastate.js

import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.serialization.js.ErgoTreeSerializer

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ErgoTree")
object ErgoTree {

  @JSExport
  def fromHex(hex: String): ErgoTree = {
    val bytes = Base16.decode(hex).get
    ErgoTreeSerializer.deserializeErgoTree(bytes)
  }

  @JSExport
  def toHex(tree: ErgoTree): String = {
    val bytes = ErgoTreeSerializer.serializeErgoTree(tree)
    Base16.encode(bytes)
  }

}
