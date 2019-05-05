package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo._


class ErgoLikeInterpreter(override val maxCost: Long = CostTable.ScriptLimit)(implicit val IR: IRContext) extends Interpreter {

  override type CTX <: ErgoLikeContext

  override def substDeserialize(context: CTX, node: SValue): Option[SValue] = node match {
      case d: DeserializeRegister[_] =>
        context.self.get(d.reg).flatMap { v =>
          v match {
            case eba: EvaluatedValue[SByteArray]@unchecked =>
              val outVal = ValueSerializer.deserialize(eba.value.toArray)(context.validationSettings)
              if (outVal.tpe != d.tpe)
                sys.error(s"Failed deserialization of $d: expected deserialized value to have type ${d.tpe}; got ${outVal.tpe}")
              else
                Some(outVal)
            case _ => None
          }
        }.orElse(d.default)
      case _ => super.substDeserialize(context, node)
    }
}