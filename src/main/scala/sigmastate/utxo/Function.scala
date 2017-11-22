package sigmastate.utxo

import sigmastate.{Relation, StateTree, Value}
import sigmastate.utxo.CostTable.Cost


trait Function extends StateTree

//todo: replace with ByIndex on Collection
//todo: derive from BoxLeaf
case class TxOutput(outIndex: Int, relation: Relation[Value, Value]*) extends Function {
  override val cost: Int = relation.length + Cost.TxOutputDeclaration
}
