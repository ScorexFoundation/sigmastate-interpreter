package org.ergoplatform.sdk.js

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[special.sigma.AvlTree]] available from JS. */
@JSExportTopLevel("AvlTree")
class AvlTree(
    val digest: String,
    val insertAllowed: Boolean,
    val updateAllowed: Boolean,
    val removeAllowed: Boolean,
    val keyLength: Int,
    val valueLengthOpt: UndefOr[Int]
) extends js.Object
