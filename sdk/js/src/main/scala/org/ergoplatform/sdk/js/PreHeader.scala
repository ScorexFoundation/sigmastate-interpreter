package org.ergoplatform.sdk.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("BlockchainStateContext")
class PreHeader(
    val version: Byte,
    val parentId: String,
    val timestamp: js.BigInt,
    val nBits: js.BigInt,
    val height: Int,
    val minerPk: String,
    val votes: String
)
