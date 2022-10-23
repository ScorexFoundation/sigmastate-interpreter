package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Value, ValueCompanion}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class CaseObjectSerialization[V <: Value[SType]](override val opDesc: ValueCompanion, obj: V)
  extends ValueSerializer[V] {

  override def serialize(obj: V, w: SigmaByteWriter): Unit = ()

  override def parse(r: SigmaByteReader): V = obj
}
