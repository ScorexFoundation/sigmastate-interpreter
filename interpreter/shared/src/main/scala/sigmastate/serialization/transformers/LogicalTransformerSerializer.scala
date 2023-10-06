package sigmastate.serialization.transformers

import sigma.ast.global.SValue
import sigma.ast.{LogicalTransformerCompanion, SBoolean, SCollection}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer

case class LogicalTransformerSerializer[I <: SCollection[SBoolean.type], O <: SBoolean.type]
(opDesc: LogicalTransformerCompanion,
 cons: Value[SCollection[SBoolean.type]] => Value[SBoolean.type])
  extends ValueSerializer[Transformer[I, O]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)

  override def serialize(obj: Transformer[I, O], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)

  override def parse(r: SigmaByteReader): Value[SBoolean.type] =
    cons(r.getValue().asCollection[SBoolean.type])
}
