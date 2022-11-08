package sigmastate.serialization.js

import sigmastate.SType
import sigmastate.js.Values.Value

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ValueSerializer")
object ValueSerializer {
  @JSExport
  def deserialize(bytes: Array[Byte]): Value[SType] =
    sigmastate.serialization.ValueSerializer.deserialize(bytes)
}
