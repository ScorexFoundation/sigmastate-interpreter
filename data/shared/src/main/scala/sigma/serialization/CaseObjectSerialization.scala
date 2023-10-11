package sigma.serialization

import sigma.ast.SType
import sigma.ast.{Value, ValueCompanion}

case class CaseObjectSerialization[V <: Value[SType]](override val opDesc: ValueCompanion, obj: V)
  extends ValueSerializer[V] {

  override def serialize(obj: V, w: SigmaByteWriter): Unit = ()

  override def parse(r: SigmaByteReader): V = obj
}
