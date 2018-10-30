package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.interpreter.Interpreter
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo._


class ErgoLikeInterpreter(override val maxCost: Long = CostTable.ScriptLimit) extends Interpreter {

  override type CTX <: ErgoLikeContext

  override def evaluateNode(context: CTX, tree: SValue): SValue = tree match {
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.outputs.map(BoxConstant.apply))

    case Self => BoxConstant(context.self)

    case Height => LongConstant(context.currentHeight)

    case MinerPubkey => ByteArrayConstant(context.minerPubkey)

    case LastBlockUtxoRootHash => AvlTreeConstant(context.lastBlockUtxoRoot)

    case _ =>
      super.evaluateNode(context, tree)
  }

  override def substDeserialize(context: CTX, node: SValue): Option[SValue] = node match {
      case d: DeserializeRegister[_] =>
        context.self.get(d.reg).flatMap { v =>
          v match {
            case eba: EvaluatedValue[SByteArray]@unchecked => Some(ValueSerializer.deserialize(eba.value))
            case _ => None
          }
        }.orElse(d.default)
      case _ => super.substDeserialize(context, node)
    }
}