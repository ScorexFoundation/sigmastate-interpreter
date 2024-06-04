package org.ergoplatform.sdk

import org.ergoplatform.sdk.TransactionHintsBag.MapOfBags
import sigmastate.interpreter.HintsBag

/** Holds public and secret hints for each input of a transaction. */
case class TransactionHintsBag(
  secretHints: MapOfBags,
  publicHints: MapOfBags
) {
  private def updateHints(mapOfBags: MapOfBags, idx: Int, bag: HintsBag): MapOfBags = {
    mapOfBags.updated(idx, mapOfBags.getOrElse(idx, HintsBag.empty) ++ bag)
  }

  /** Adding hints for a input index */
  def addHintsForInput(idx: Int, hints: HintsBag): TransactionHintsBag = {
    val secretBag = HintsBag(hints.proofs)
    val publicBag = HintsBag(hints.commitments)
    TransactionHintsBag(
      secretHints = updateHints(this.secretHints, idx, secretBag),
      publicHints = updateHints(this.publicHints, idx, publicBag)
    )
  }

  /** Returns hints for a given input of transaction */
  def hintsForInput(inputIdx: Int): HintsBag = {
    val secretBag = secretHints.getOrElse(inputIdx, HintsBag.empty)
    val publicBag = publicHints.getOrElse(inputIdx, HintsBag.empty)
    HintsBag(secretBag.hints ++ publicBag.hints)
  }
}

object TransactionHintsBag {
  /** Holds public and secret hints for each input of a transaction. */
  type MapOfBags = Map[Int, HintsBag]

  def empty: TransactionHintsBag = new TransactionHintsBag(Map.empty, Map.empty)
}