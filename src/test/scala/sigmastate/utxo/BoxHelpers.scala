package sigmastate.utxo

import scorex.crypto.hash.Blake2b256
import sigmastate.utxo.SigmaStateBox.NonMandatoryIdentifier
import sigmastate.{SBoolean, SType, TrueLeaf, Value}

object BoxHelpers {
  def createBox(value: Int,
                proposition: Value[SBoolean.type],
                additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map())
    = SigmaStateBox(value, proposition, additionalRegisters)

  val fakeSelf: SigmaStateBox = createBox(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")
}