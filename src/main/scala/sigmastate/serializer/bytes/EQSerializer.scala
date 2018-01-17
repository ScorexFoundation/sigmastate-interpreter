package sigmastate.serializer.bytes

import sigmastate.{EQ, SType}

object EQSerializer {
  val OpCode: Short = 5
}
class EQSerializer[T1 <: SType, T2 <: SType] extends TwoOperandOperationSerializer[T1, T2, EQ[T1, T2]](EQSerializer.OpCode, EQ.apply)
