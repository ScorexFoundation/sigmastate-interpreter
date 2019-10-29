package sigmastate.serialization.transformers

import sigmastate.Values.{Value, SValue}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Fold
import sigmastate.{SCollection, SType, SFunc}

case class FoldSerializer(cons: (Value[SCollection[SType]], Value[SType], Value[SFunc]) => Value[SType])
  extends ValueSerializer[Fold[SType, SType]] {
  override def opDesc = Fold
  import sigmastate.Operations.FoldInfo._
  val thisInfo: DataInfo[SValue] = thisArg
  val zeroInfo: DataInfo[SValue] = zeroArg
  val opInfo: DataInfo[SValue] = opArg

  override def serialize(obj: Fold[SType, SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, thisInfo)
      .putValue(obj.zero, zeroInfo)
      .putValue(obj.foldOp, opInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input  = r.getValue().asCollection[SType]
    val zero   = r.getValue()
    val foldOp = r.getValue().asFunc
    cons(input, zero, foldOp)
  }
}
