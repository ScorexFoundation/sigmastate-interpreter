package sigmastate.utxo

import sigmastate.NotReadyValueBoxLeaf
import sigmastate.utxo.CostTable.Cost

//todo: replace with ByIndex on Collection
case class TxOutput(outIndex: Int) extends NotReadyValueBoxLeaf {
  override val cost: Int = Cost.TxOutputDeclaration
}