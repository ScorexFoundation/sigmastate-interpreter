package sigmastate.utxo

import sigmastate.Values.Constant
import sigmastate.serialization.OpCodes.OpCode

object ComplexityTable {
  val OpCodeComplexity: Map[OpCode, Int] = Seq(
    Constant.opCode -> 84,
  ).toMap
}
