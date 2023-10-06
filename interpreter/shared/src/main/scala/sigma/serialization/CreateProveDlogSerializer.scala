package sigma.serialization

import sigma.ast.{CreateProveDlog, SGroupElement}
import sigma.serialization.CoreByteWriter._
import sigma.ast.Value
import sigma.ast.defs._
import sigmastate.lang.Terms.ValueOps
import SigmaByteWriter._

case class CreateProveDlogSerializer(cons: Value[SGroupElement.type] => SigmaPropValue)
    extends ValueSerializer[CreateProveDlog] {
  import sigma.ast.Operations.CreateProveDlogInfo._

  override def opDesc = CreateProveDlog

  val valueInfo: DataInfo[SValue] = valueArg

  override def serialize(obj: CreateProveDlog, w: SigmaByteWriter): Unit = {
    w.putValue(obj.value, valueInfo)
  }

  override def parse(r: SigmaByteReader) = {
    val v = r.getValue().asValue[SGroupElement.type]
    cons(v)
  }
}
