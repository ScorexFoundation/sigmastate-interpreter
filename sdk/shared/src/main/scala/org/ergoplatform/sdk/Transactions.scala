package org.ergoplatform.sdk

import org.ergoplatform.{ErgoBox, ErgoLikeTransaction, UnsignedErgoLikeTransaction}

case class UnsignedTransaction(
    ergoTx: UnsignedErgoLikeTransaction,
    boxesToSpend: IndexedSeq[ExtendedInputBox],
    dataInputs: IndexedSeq[ErgoBox],
    tokensToBurn: IndexedSeq[ErgoToken]
)

case class SignedTransaction(ergoTx: ErgoLikeTransaction, cost: Int)

case class ReducedTransaction(ergoTx: ReducedErgoLikeTransaction, cost: Int)

