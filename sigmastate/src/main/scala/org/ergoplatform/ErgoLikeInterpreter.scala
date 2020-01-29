package org.ergoplatform

import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.eval.IRContext
import sigmastate.interpreter.{Interpreter, ErgoTreeEvaluator}
import sigmastate.interpreter.Interpreter.{ScriptEnv, ReductionResult}
import sigmastate.utxo._

import scala.util.Try


class ErgoLikeInterpreter(implicit val IR: IRContext) extends Interpreter {

  override type CTX <: ErgoLikeContext

  override def substDeserialize(context: CTX, updateContext: CTX => Unit, node: SValue): Option[SValue] = node match {
      case d: DeserializeRegister[_] =>
        context.boxesToSpend(context.selfIndex).get(d.reg).flatMap { v =>
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

  def reduceToCrypto2(context: CTX, env: ScriptEnv, exp: SigmaPropValue): Try[ReductionResult] = Try {
    val processor = new ErgoTreeEvaluator(env)
    processor.eval(context, ErgoTree.withoutSegregation(exp))
  }

}