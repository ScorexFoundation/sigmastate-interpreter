package sigmastate.utxo

import sigmastate.SigmaStateTree

object BoxHelpers {
  def boxWithMetadata(value: Int, proposition: SigmaStateTree, creationHeight: Int = 0) =
    BowWithMetadata(SigmaStateBox(value, proposition), BoxMetadata(creationHeight))
}