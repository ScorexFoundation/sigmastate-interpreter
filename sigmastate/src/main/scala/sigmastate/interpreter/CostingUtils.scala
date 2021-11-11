package sigmastate.interpreter

import sigmastate.interpreter.Interpreter.ReductionResult

object CostingUtils {
  /** Checks that a jitRes is equal to res and also checks costs.
    * Reports either error or warning to the console.
    *
    * @param ergoTreeHex used to compose the error message
    * @param es          evaluation settings currently used by the interpreter
    */
  def checkResults(
        ergoTreeHex: String,
        aotRes: ReductionResult, jitRes: ReductionResult,
        logger: String => Unit)(implicit es: EvalSettings): Unit = {
    val aotValue = aotRes.value
    val jitValue = jitRes.value
    if (aotValue != jitValue) {
      val msg =
        s"""Wrong JIT result: -----------------------------------------
          |ErgoTree: $ergoTreeHex
          |AOT result: $aotValue
          |JIT result: $jitValue
          |------------------------------------------------------------""".stripMargin
      if (es.isTestRun) {
        Interpreter.error(msg)
      }
      else if (es.isLogEnabled) {
        logger(msg)
      }
    }
    checkCosts(ergoTreeHex, aotRes.cost, jitRes.cost, logger)
  }

  /** Checks that newCost doesn't exceed oldCost.
    * If it exceeds, then:
    * - in test context - throws an error
    * - in non-test context - sends a warning to the logger.
    *
    * @param ergoTreeHex used to compose the error message
    * @param es          evaluation settings currently used by the interpreter
    */
  def checkCosts(
        ergoTreeHex: String,
        aotCost: Long, jitCost: Long,
        logger: String => Unit)(implicit es: EvalSettings): Unit = {
    if (aotCost < jitCost) {
      val msg =
        s"""Wrong JIT cost: -----------------------------------------
          |ErgoTree: $ergoTreeHex
          |AOT cost: $aotCost
          |JIT cost: $jitCost
          |------------------------------------------------------------""".stripMargin
      if (es.isTestRun) {
        Interpreter.error(msg)
      }
      else if (es.isLogEnabled) {
        logger(msg)
      }
    }
  }
}
