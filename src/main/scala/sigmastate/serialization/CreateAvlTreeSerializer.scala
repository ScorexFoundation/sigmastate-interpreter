package sigmastate.serialization

import sigmastate.SCollection._
import sigmastate.SOption.SIntOption
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms.ValueOps

case class CreateAvlTreeSerializer(
    cons: (ByteValue, Value[SByteArray], IntValue, Value[SIntOption]) => AvlTreeValue
  )
  extends ValueSerializer[CreateAvlTree]
{
  import sigmastate.Operations.CreateAvlTreeInfo._
  override def opDesc = CreateAvlTree

  override def serialize(obj: CreateAvlTree, w: SigmaByteWriter): Unit = {
    w.putValue(obj.operationFlags, operationFlagsArg)
    w.putValue(obj.digest, digestArg)
    w.putValue(obj.keyLength, keyLengthArg)
    w.putValue(obj.valueLengthOpt, valueLengthOptArg)
  }

  override def parse(r: SigmaByteReader) = {
    val flags = r.getValue().asByteValue
    val digest = r.getValue().asByteArray
    val keyLength = r.getValue().asIntValue
    val valueLength = r.getValue().asOption[SInt.type]
    cons(flags, digest, keyLength, valueLength)
  }
}
