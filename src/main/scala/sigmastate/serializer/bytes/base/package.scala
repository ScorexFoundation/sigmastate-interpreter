package sigmastate.serializer.bytes

import scorex.core.serialization.Serializer
import sigmastate.{SInt, Value}

package object base {
  implicit val sIntSerializer: Serializer[Value[SInt.type]] = new SIntSerializer
}
