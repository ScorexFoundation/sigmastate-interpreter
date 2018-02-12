package sigmastate.utxo

import scorex.crypto.hash.Blake2b256
import sigmastate.{SigmaStateTree, TrueLeaf}

object BoxHelpers {
  def boxWithMetadata(value: Int, proposition: SigmaStateTree, creationHeight: Int = 0) =
    BoxWithMetadata(SigmaStateBox(value, proposition), BoxMetadata(creationHeight))

  val fakeSelf: BoxWithMetadata = boxWithMetadata(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")
}