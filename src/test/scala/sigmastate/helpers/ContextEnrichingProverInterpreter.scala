package sigmastate.helpers

import sigmastate.SType
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.interpreter.{ContextExtension, CostedProverResult, HintsBag, ProverInterpreter}

import scala.util.Try

/**
  * Proving interpreter that keeps dictionary with possible variables and
  * automatically enrich context when needed
  *
  * Note: context is included into message (under hash function), thus changed context
  * also changes message. This trait may be useful for tests, that sign fake messages,
  * or for transactions which inputs does not require signatures.
  */
trait ContextEnrichingProverInterpreter extends ProverInterpreter {

  def contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = Map()

  val knownExtensions = ContextExtension(contextExtenders)

  /**
    * Replace context.extension to knownExtensions and prove script in different context.
    */
  override def prove(env: ScriptEnv, exp: ErgoTree, context: CTX, message: Array[Byte], hintsBag: HintsBag): Try[CostedProverResult] = {
    val enrichedContext = context.withExtension(knownExtensions).asInstanceOf[CTX]
    super.prove(env, exp, enrichedContext, message)
  }
}
