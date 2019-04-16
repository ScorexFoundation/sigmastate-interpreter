package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Fold
import sigmastate.{SCollection, SType, SFunc}

case class FoldSerializer(cons: (Value[SCollection[SType]], Value[SType], Value[SFunc]) => Value[SType])
  extends ValueSerializer[Fold[SType, SType]] {
  override def opDesc = Fold

  override def serialize(obj: Fold[SType, SType], w: SigmaByteWriter): Unit = {
    import Fold._
    w.putValue(obj.input, inputArg)
      .putValue(obj.zero, zeroArg)
      .putValue(obj.foldOp, foldOpArg)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input  = r.getValue().asCollection[SType]
    val zero   = r.getValue()
    val foldOp = r.getValue().asFunc
    cons(input, zero, foldOp)
  }
}
