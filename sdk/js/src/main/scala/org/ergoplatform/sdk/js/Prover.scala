package org.ergoplatform.sdk.js

import typings.fleetSdkCommon.transactionsMod

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Prover")
class Prover {
  def reduceTransaction(
      unsignedTx: transactionsMod.UnsignedTransaction
//      boxesToSpend: IndexedSeq[ExtendedInputBox],
//      dataBoxes: IndexedSeq[ErgoBox],
//      stateContext: ErgoLikeStateContext,
//      baseCost: Int,
//      tokensToBurn: IndexedSeq[ErgoToken]
  ): (ReducedTransaction, Int) = {
    val tx = Isos.isoUnsignedTransaction.to(unsignedTx)
    (new ReducedTransaction, 0)
  }
}

@JSExportTopLevel("Prover")
class ReducedTransaction

@JSExportTopLevel("ProverFactory")
object Prover {
}
