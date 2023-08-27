package sigmastate.serialization.generators

import org.scalacheck.Gen
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueCodes.OpCode

trait OpcodesGen {

  val arithmeticCodeGen: Gen[OpCode] = Gen.oneOf(MinusCode, PlusCode, MultiplyCode, DivisionCode, ModuloCode, MinCode, MaxCode)


}
