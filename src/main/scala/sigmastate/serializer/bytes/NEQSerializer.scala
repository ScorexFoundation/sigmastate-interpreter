package sigmastate.serializer.bytes

import sigmastate.{NEQ, SType}

object NEQSerializer {
  val OpCode: Short = 6
}
class NEQSerializer[T1 <: SType, T2 <: SType] extends TwoOperandOperationSerializer[T1, T2, NEQ[T1, T2]](EQSerializer.OpCode, NEQ.apply)
