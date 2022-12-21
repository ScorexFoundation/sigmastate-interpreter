package org.ergoplatform.sdk.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("BlockchainStateContext")
class BlockchainStateContext(
    val sigmaLastHeaders: js.Array[Header],
    val previousStateDigest: String,
    val sigmaPreHeader: PreHeader
)
