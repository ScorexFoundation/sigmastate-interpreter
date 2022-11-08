package sigmastate.js

import sigmastate.SType

import scala.scalajs.js.annotation.JSExport

object Values {
  @JSExport
  type Value[+S <: SType] = sigmastate.Values.Value[S]
}
