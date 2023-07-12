package org.ergoplatform.sdk.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[special.sigma.Header]] available from JS. */
@JSExportTopLevel("Header")
class Header(
    val id: String,
    val version: Byte,
    val parentId: String,
    val ADProofsRoot: String,
    val stateRoot: AvlTree,
    val transactionsRoot: String,
    val timestamp: js.BigInt,
    val nBits: js.BigInt,
    val height: Int,
    val extensionRoot: String,
    val minerPk: String,
    val powOnetimePk: String,
    val powNonce: String,
    val powDistance: js.BigInt,
    val votes: String
) extends js.Object
