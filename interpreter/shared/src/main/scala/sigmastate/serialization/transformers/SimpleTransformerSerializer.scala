package sigmastate.serialization.transformers

import sigma.ast.SType
import sigma.serialization.CoreByteWriter.DataInfo
import sigmastate.Values.{SValue, Value}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.{SimpleTransformerCompanion, Transformer}

case class SimpleTransformerSerializer[I <: SType, O <: SType]
(opDesc: SimpleTransformerCompanion,
 cons: Value[I] => Value[O]) extends ValueSerializer[Transformer[I, O]] {
  val inputInfo: DataInfo[SValue] = opDesc.argInfos(0)
  
  override def serialize(obj: Transformer[I, O], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)

  override def parse(r: SigmaByteReader): Value[O] =
    cons(r.getValue().asValue[I])
}
