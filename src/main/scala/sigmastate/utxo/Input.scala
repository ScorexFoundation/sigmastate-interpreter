package sigmastate.utxo

import scorex.crypto.authds.ADKey
import sigmastate.UncheckedTree
import sigmastate.interpreter.ProverResult

trait InputTemplate {
  val boxId: ADKey
}

case class UnsignedInput(override val boxId: ADKey) extends InputTemplate

case class Input(override val boxId: ADKey, spendingProof: ProverResult[UncheckedTree])
  extends InputTemplate {
  def bytes: Array[Byte] = Array()
}