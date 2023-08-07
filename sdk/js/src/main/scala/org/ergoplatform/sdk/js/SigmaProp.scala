package org.ergoplatform.sdk.js

import sigmastate.Values.SigmaBoolean
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[special.sigma.SigmaProp]] available from JS. */
@JSExportTopLevel("SigmaProp")
class SigmaProp(val sigmaBoolean: SigmaBoolean) extends js.Object
