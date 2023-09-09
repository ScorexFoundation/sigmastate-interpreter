package sigmastate.serialization

import sigma.ast.SGroupElement
import sigma.serialization.CoreByteWriter.DataInfo
import sigmastate.crypto.DLogProtocol.ProveDlog
import sigmastate.CreateProveDlog
import sigmastate.Values.{SValue, SigmaPropValue, Value}
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ProveDlogSerializer(cons: EcPointType => ProveDlog)
  extends SigmaSerializer[ProveDlog, ProveDlog] {

  override def serialize(obj: ProveDlog, w: SigmaByteWriter): Unit =
    GroupElementSerializer.serialize(obj.value, w)

  override def parse(r: SigmaByteReader) = {
    val res = GroupElementSerializer.parse(r)
    cons(res)
  }
}

case class CreateProveDlogSerializer(cons: Value[SGroupElement.type] => SigmaPropValue)
    extends ValueSerializer[CreateProveDlog] {
  import sigmastate.Operations.CreateProveDlogInfo._
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


