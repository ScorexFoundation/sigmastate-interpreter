package sigmastate.serialization

import sigmastate.SCollection._
import sigmastate.SOption.SIntOption
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms.ValueOps

case class CreateAvlTreeSerializer(
    cons: (ByteValue, Value[SByteArray], IntValue, Value[SIntOption]) => AvlTreeValue
  )
  extends ValueSerializer[CreateAvlTree]
{
  override val opCode: OpCode = OpCodes.AvlTreeCode

  override def serialize(obj: CreateAvlTree, w: SigmaByteWriter): Unit = {
    w.putValue(obj.operationFlags)
    w.putValue(obj.digest)
    w.putValue(obj.keyLength)
    w.putValue(obj.valueLengthOpt)
  }

  override def parse(r: SigmaByteReader) = {
    val flags = r.getValue().asByteValue
    val digest = r.getValue().asByteArray
    val keyLength = r.getValue().asIntValue
    val valueLength = r.getValue().asOption[SInt.type]
    cons(flags, digest, keyLength, valueLength)
  }
}
