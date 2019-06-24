package sigmastate.utxo

import sigmastate.Values.Constant
import sigmastate.serialization.OpCodes.OpCode

object ComplexityTable {
  val MinimalComplexity = 100
  val OpCodeComplexity: Map[OpCode, Int] = Seq(
    Constant.opCode -> MinimalComplexity,
  ).toMap
}
