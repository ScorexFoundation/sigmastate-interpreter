package sigmastate.utxo

import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.Interpreter


class ErgoInterpreter(override val maxCost: Int = CostTable.ScriptLimit) extends Interpreter {
  override type CTX = ErgoContext

  override def specificTransformations(context: ErgoContext, tree: SValue): SValue = tree match {
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.outputs.map(BoxConstant.apply))

    case Self => BoxConstant(context.self)

    case Height => IntConstant(context.currentHeight)

    case LastBlockUtxoRootHash => AvlTreeConstant(context.lastBlockUtxoRoot)

    case t: TaggedVariable[_] =>
      if (context.extension.values.contains(t.id))
        context.extension.values(t.id)
      else
        null
//        Interpreter.error(s"Tagged variable with id=${t.id} not found in context ${context.extension.values}")

    case d: Deserialize[_] if d.transformationReady =>
      d.function(d.input.asInstanceOf[EvaluatedValue[SByteArray.type]])

    case _ =>
      super.specificTransformations(context, tree)
  }
}