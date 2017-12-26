package sigmastate.utxo

import sigmastate.NotReadyValueBox
import sigmastate.utxo.CostTable.Cost

//todo: replace with ByIndex on Collection
case class TxOutput(outIndex: Int) extends NotReadyValueBox {
  override val cost: Int = Cost.TxOutputDeclaration
}