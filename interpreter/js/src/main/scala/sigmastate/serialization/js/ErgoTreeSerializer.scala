package sigmastate.serialization.js

import sigmastate.Values.ErgoTree
import sigmastate.serialization.ErgoTreeSerializer.{DefaultSerializer => TreeSerializer}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ErgoTreeSerializer")
object ErgoTreeSerializer {

  @JSExport
  def serializeErgoTree(ergoTree: ErgoTree): Array[Byte] =
    TreeSerializer.serializeErgoTree(ergoTree)

  @JSExport
  def deserializeErgoTree(bytes: Array[Byte]): ErgoTree =
    TreeSerializer.deserializeErgoTree(bytes)
}
