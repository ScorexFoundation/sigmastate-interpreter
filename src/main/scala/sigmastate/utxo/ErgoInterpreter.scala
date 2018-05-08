package sigmastate.utxo

import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.Interpreter
import sigmastate.serialization.ValueSerializer


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

    case d: DeserializeContext[_] =>
      if (context.extension.values.contains(d.id))
        context.extension.values(d.id) match {
          case eba: EvaluatedValue[SByteArray.type] => ValueSerializer.deserialize(eba.value)
          case _ => null
        }
      else
        null

    case d: DeserializeRegister[_] =>
      context.self.get(d.reg).map { v =>
        v match {
          case eba: EvaluatedValue[SByteArray.type] => ValueSerializer.deserialize(eba.value)
          case _ => null
        }
      }.orElse(d.default).orNull

    case _ =>
      super.specificTransformations(context, tree)
  }
}