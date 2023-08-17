package org.ergoplatform.sdk.js

import org.ergoplatform.sdk
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sdk.ReducedTransaction]] available from JS. */
@JSExportTopLevel("ReducedTransaction")
class ReducedTransaction(private[js] val _tx: sdk.ReducedTransaction) {
  /** Serialized bytes of this transaction in hex format. */
  def toHex: String = _tx.toHex
}

@JSExportTopLevel("ReducedTransactionObj")
object ReducedTransaction {
  /** Creates a [[ReducedTransaction]] from serialized bytes in hex format. */
  def fromHex(hex: String): ReducedTransaction = {
    val tx = sdk.ReducedTransaction.fromHex(hex)
    new ReducedTransaction(tx)
  }
}