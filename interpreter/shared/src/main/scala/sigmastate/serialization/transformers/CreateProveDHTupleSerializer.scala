package sigmastate.serialization.transformers

import sigma.ast.SGroupElement
import sigma.ast.global.SigmaPropValue
import sigmastate.CreateProveDHTuple
import sigma.ast.Value
import sigmastate.lang.Terms._
import sigmastate.serialization._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class CreateProveDHTupleSerializer(cons: (Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type],
    Value[SGroupElement.type]) => SigmaPropValue)
    extends ValueSerializer[CreateProveDHTuple] {
  import sigma.ast.Operations.CreateProveDHTupleInfo._
  override def opDesc = CreateProveDHTuple

  override def serialize(obj: CreateProveDHTuple, w: SigmaByteWriter): Unit = {
    w.putValue(obj.gv, gArg)
    w.putValue(obj.hv, hArg)
    w.putValue(obj.uv, uArg)
    w.putValue(obj.vv, vArg)
  }

  override def parse(r: SigmaByteReader) = {
    val gv = r.getValue().asValue[SGroupElement.type]
    val hv = r.getValue().asValue[SGroupElement.type]
    val uv = r.getValue().asValue[SGroupElement.type]
    val vv = r.getValue().asValue[SGroupElement.type]
    cons(gv, hv, uv, vv)
  }
}
