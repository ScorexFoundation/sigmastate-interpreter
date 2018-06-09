package sigmastate.serialization.transformers

import sigmastate.Values.{ConcreteCollection, Value}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Consumed, Position}
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.utxo.Transformer
import sigmastate.{SBoolean, SCollection}

case class LogicalTransformerSerializer[I <: SCollection[SBoolean.type], O <: SBoolean.type]
(code: OpCode,
 cons: Seq[Value[SBoolean.type]] => Transformer[I, O])
  extends ValueSerializer[Transformer[I, O]] {

  override val opCode: OpCode = code

  override def parseBody(bytes: Array[Byte], pos: Position): (Transformer[I, O], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val size = r.getShort()
    val values =  (1 to size).map(_ => r.getValue().asInstanceOf[Value[SBoolean.type]])
    cons(values) -> r.consumed
  }

  override def serializeBody(obj: Transformer[I, O]): Array[Byte] = {
    obj.input.asInstanceOf[Value[SCollection[SBoolean.type]]] match {
      case ConcreteCollection(items, SBoolean) =>
        val ccSize = items.size
        require(ccSize <= Short.MaxValue, s"max collection size is Short.MaxValue = ${Short.MaxValue}")
        val size = ccSize.toShort
        val w = Serializer.startWriter()
          .putShort(size)
        for (item <- items) {
          w.putValue(item)
        }
        w.toBytes
    }
  }
}
