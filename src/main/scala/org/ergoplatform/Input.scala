package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}


class UnsignedInput(val boxId: BoxId) {
  require(boxId.size == BoxId.size, s"incorrect boxId size, expected: $BoxId.size, got: ${boxId.size}")

  // todo check whether it is correct to compare inputs (Input use the same equals) by boxId only?
  override def equals(obj: Any): Boolean = obj match {
    case x: UnsignedInput => util.Arrays.equals(boxId, x.boxId)
    case _ => false
  }
}

case class Input(override val boxId: BoxId, spendingProof: ProverResult)
  extends UnsignedInput(boxId) {
}

object Input {
  object serializer extends SigmaSerializer[Input, Input] {

    override def serialize(obj: Input, w: SigmaByteWriter): Unit = {
      w.putBytes(obj.boxId)
      ProverResult.serializer.serialize(obj.spendingProof, w)
    }

    override def parse(r: SigmaByteReader): Input = {
      val boxId = r.getBytes(BoxId.size)
      val spendingProof = ProverResult.serializer.parse(r)
      Input(ADKey @@ boxId, spendingProof)
    }
  }
}
