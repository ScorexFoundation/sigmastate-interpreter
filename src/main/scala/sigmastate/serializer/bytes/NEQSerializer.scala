package sigmastate.serializer.bytes

import scorex.core.serialization.Serializer
import sigmastate.{NEQ, SType, Value}

object NEQSerializer {
  private val OpCode: Short = 6
}
class NEQSerializer[LT <: SType, RT <: SType](implicit leftSerializer: Serializer[Value[LT]],
                                              rightSerializer: Serializer[Value[RT]]) extends TwoOperandOperationSerializer[LT, RT, NEQ[LT, RT]](NEQSerializer.OpCode, NEQ.apply)
