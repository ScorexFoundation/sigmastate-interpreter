package sigmastate.utxo

import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.Interpreter


class ErgoInterpreter(override val maxCost: Int = CostTable.ScriptLimit) extends Interpreter {
  override type CTX = ErgoContext

  override def specificTransformations(context: ErgoContext): PartialFunction[Value[_ <: SType], Value[_ <: SType]] = {
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.outputs.map(BoxConstant.apply))

    case Self => BoxConstant(context.self)

    case Height => IntConstant(context.currentHeight)

    case LastBlockUtxoRootHash => AvlTreeConstant(context.lastBlockUtxoRoot)

    case t: TaggedVariable[_] if context.extension.values.contains(t.id) =>
      context.extension.values(t.id)
  }
}