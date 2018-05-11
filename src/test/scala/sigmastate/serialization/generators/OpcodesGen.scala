package sigmastate.serialization.generators

import org.scalacheck.Gen
import sigmastate.serialization.OpCodes._

trait OpcodesGen {

  val aritmeticCodeGen: Gen[OpCode] = Gen.oneOf(MinusCode, PlusCode, MultiplyCode, DivisionCode, ModuloCode)


}
