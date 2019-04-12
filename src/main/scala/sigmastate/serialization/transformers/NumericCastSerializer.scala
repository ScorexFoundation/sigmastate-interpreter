package sigmastate.serialization.transformers

import sigmastate.Values.{Value, ValueCompanion}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer

case class NumericCastSerializer(opDesc: ValueCompanion,
                                 cons: (Value[SNumericType], SNumericType) => Value[SNumericType])
  extends ValueSerializer[Transformer[SNumericType, SNumericType]] {

  override def serialize(obj: Transformer[SNumericType, SNumericType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .putType(obj.tpe)

  override def parse(r: SigmaByteReader): Value[SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    cons(input, tpe)
  }
}
