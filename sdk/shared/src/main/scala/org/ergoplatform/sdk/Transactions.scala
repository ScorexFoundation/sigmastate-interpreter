package org.ergoplatform.sdk

import org.ergoplatform.{ErgoBox, ErgoLikeTransaction, UnsignedErgoLikeTransaction}

/** Represents a transaction data chat can be reduced to [[ReducedTransaction]].
  *
  * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
  *
  * @param unsignedTx   original unsigned transaction to be reduced (holds messageToSign)
  * @param boxesToSpend input boxes of the transaction
  * @param dataInputs   data inputs of the transaction
  * @param tokensToBurn requested tokens to be burnt in the transaction, if empty no burning allowed
  */
case class UnreducedTransaction(
    unsignedTx: UnsignedErgoLikeTransaction,
    boxesToSpend: IndexedSeq[ExtendedInputBox],
    dataInputs: IndexedSeq[ErgoBox],
    tokensToBurn: IndexedSeq[ErgoToken]
) {
  require(unsignedTx.inputs.length == boxesToSpend.length, "Not enough boxes to spend")
  require(unsignedTx.dataInputs.length == dataInputs.length, "Not enough data boxes")
  checkSameElements(
    "unsignedTx.inputs", unsignedTx.inputs.map(_.boxId),
    "boxesToSpend", boxesToSpend.map(_.box.id),
    "boxesToSpend should have the same box ids as unsignedTx.inputs")
  checkSameElements(
    "unsignedTx.dataInputs", unsignedTx.dataInputs.map(_.boxId),
    "dataInputs", dataInputs.map(_.id),
    "dataInputs should have the same box ids as unsignedTx.dataInputs")
  
  private def checkSameElements[A](xsName: String, xs: Seq[A], ysName: String, ys: Seq[A], msg: => String) = {
    require(xs == ys, {
      val xsOnly = xs.diff(ys)
      val ysOnly = ys.diff(xs)
      s"""$msg:
        | only in $xsName: $xsOnly
        | only in $ysName: $ysOnly
        |""".stripMargin
    })
  }
}

/** Represents results for transaction reduction by [[ReducingInterpreter]]. */
case class ReducedTransaction(ergoTx: ReducedErgoLikeTransaction, cost: Int)

/** Represents results for transaction signing by a prover like [[Prover]]. */
case class SignedTransaction(ergoTx: ErgoLikeTransaction, cost: Int)


