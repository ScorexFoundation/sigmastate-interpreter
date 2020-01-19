package sigmastate.serialization

import sigmastate.SCollection._
import sigmastate.SOption.SIntOption
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms.ValueOps
import sigmastate.utils.SigmaByteWriter.DataInfo

case class CreateAvlTreeSerializer(
    cons: (ByteValue, Value[SByteArray], IntValue, Value[SIntOption]) => AvlTreeValue
  )
  extends ValueSerializer[CreateAvlTree]
{
  import sigmastate.Operations.CreateAvlTreeInfo._
  override def opDesc = CreateAvlTree
  val operationFlagsInfo: DataInfo[SValue] = operationFlagsArg
  val digestInfo: DataInfo[SValue] = digestArg
  val keyLengthInfo: DataInfo[SValue] = keyLengthArg
  val valueLengthOptInfo: DataInfo[SValue] = valueLengthOptArg

  override def serialize(obj: CreateAvlTree, w: SigmaByteWriter): Unit = {
    w.putValue(obj.operationFlags, operationFlagsInfo)
    w.putValue(obj.digest, digestInfo)
    w.putValue(obj.keyLength, keyLengthInfo)
    w.putValue(obj.valueLengthOpt, valueLengthOptInfo)
  }

  override def parse(r: SigmaByteReader) = {
    val flags = r.getValue().asByteValue
    val digest = r.getValue().asByteArray
    val keyLength = r.getValue().asIntValue
    val valueLength = r.getValue().asOption[SInt.type]
    cons(flags, digest, keyLength, valueLength)
  }
}
