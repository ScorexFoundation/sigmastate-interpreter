package org.ergoplatform.sdk.js

import org.ergoplatform.sdk.js.TransactionHintsBag.MapOfBags
import org.scalablytyped.runtime.NumberDictionary
import sigma.data.Iso
import sigma.interpreter.js.ProverHints
import org.ergoplatform.sdk

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[org.ergoplatform.sdk.TransactionHintsBag]] available from JS.
  * Holds public and secret hints for each input of a transaction.
  */
@JSExportTopLevel("TransactionHintsBag")
class TransactionHintsBag(
  val secretHints: MapOfBags,
  val publicHints: MapOfBags
) extends js.Object

object TransactionHintsBag {
  type MapOfBags = NumberDictionary[ProverHints]

  val isoMapOfBags: Iso[MapOfBags, sdk.TransactionHintsBag.MapOfBags] = Isos.isoNumberDictionary(ProverHints.isoProverHints)

  val isoToSdk: Iso[TransactionHintsBag, sdk.TransactionHintsBag] = new Iso[TransactionHintsBag, sdk.TransactionHintsBag] {
    override def to(p: TransactionHintsBag): sdk.TransactionHintsBag =
      sdk.TransactionHintsBag(
        isoMapOfBags.to(p.secretHints),
        isoMapOfBags.to(p.publicHints)
      )

    override def from(p: sdk.TransactionHintsBag): TransactionHintsBag = new TransactionHintsBag(
      isoMapOfBags.from(p.secretHints),
      isoMapOfBags.from(p.publicHints)
    )
  }
}
