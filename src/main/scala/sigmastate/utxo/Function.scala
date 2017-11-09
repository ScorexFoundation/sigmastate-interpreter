package sigmastate.utxo

import sigmastate.{Relation, StateTree}
import sigmastate.utxo.CostTable.Cost



trait Function extends StateTree

case class TxHasOutput(relation: Relation*) extends Function {
  override val cost: Int = relation.length + Cost.TxHasOutputDeclaration
}

case class TxOutput(outIndex: Int, relation: Relation*) extends Function {
  override val cost: Int = relation.length + Cost.TxOutputDeclaration
}
