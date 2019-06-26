package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter
import sigmastate.utxo._


class ErgoLikeInterpreter(implicit val IR: IRContext) extends Interpreter {

  override type CTX <: ErgoLikeContext

  override def substDeserialize(context: CTX, updateContext: CTX => Unit, node: SValue): Option[SValue] = node match {
      case d: DeserializeRegister[_] =>
        context.self.get(d.reg).flatMap { v =>
          v match {
            case eba: EvaluatedValue[SByteArray]@unchecked =>
              val (ctx1, outVal) = deserializeMeasured(context, eba.value.toArray)
              updateContext(ctx1)

              if (outVal.tpe != d.tpe)
                sys.error(s"Failed deserialization of $d: expected deserialized value to have type ${d.tpe}; got ${outVal.tpe}")
              else
                Some(outVal)
            case _ => None
          }
        }.orElse(d.default)
      case _ => super.substDeserialize(context, updateContext, node)
    }
}