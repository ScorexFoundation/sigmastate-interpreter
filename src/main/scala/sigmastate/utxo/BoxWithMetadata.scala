package sigmastate.utxo

import sigmastate.utxo.UtxoContext.Height

case class BoxMetadata(creationHeight: Height)

class BoxWithMetadata(val box: SigmaStateBox, val metadata: BoxMetadata)

object BoxWithMetadata{
  def apply(box: SigmaStateBox, metadata: BoxMetadata): BoxWithMetadata = new BoxWithMetadata(box, metadata)
}
