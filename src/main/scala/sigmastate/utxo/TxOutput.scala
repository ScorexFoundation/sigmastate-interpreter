package sigmastate.utxo

import sigmastate.{NotReadyValueBoxLeaf, Relation, Value}
import sigmastate.utxo.CostTable.Cost

//todo: replace with ByIndex on Collection
case class TxOutput(outIndex: Int, relation: Relation[Value, Value]*) extends NotReadyValueBoxLeaf {
  override val cost: Int = relation.length + Cost.TxOutputDeclaration
}
