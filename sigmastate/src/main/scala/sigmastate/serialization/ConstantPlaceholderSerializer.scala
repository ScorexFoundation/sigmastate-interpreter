package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ConstantPlaceholderSerializer(cons: (Int, SType) => Value[SType])
  extends ValueSerializer[ConstantPlaceholder[SType]] {
  override def opDesc = ConstantPlaceholder

  override def serialize(obj: ConstantPlaceholder[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.id)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val constant = r.constantStore.get(id)  // TODO HF: move this under if branch
    if (r.resolvePlaceholdersToConstants)
      constant
    else
      cons(id, constant.tpe)
  }
}

