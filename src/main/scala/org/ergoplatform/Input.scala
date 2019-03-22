package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.Serializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}


class UnsignedInput(val boxId: BoxId) {
  require(boxId.size == BoxId.size, s"incorrect boxId size, expected: $BoxId.size, got: ${boxId.size}")

  override def equals(obj: Any): Boolean = obj match {
    case x: UnsignedInput => util.Arrays.equals(boxId, x.boxId)
    case _ => false
  }
}

object UnsignedInput {
  object serializer extends Serializer[UnsignedInput, UnsignedInput] {

    @inline
    override def serializeBody(obj: UnsignedInput, w: SigmaByteWriter): Unit = {
      SerializeLog.logPrintf(true, true, false, "UnsignedInput")

      SerializeLog.logPrintf(true, true, false, "boxId")
      w.putBytes(obj.boxId)
      SerializeLog.logPrintf(false, true, false, "boxId")

      SerializeLog.logPrintf(false, true, false, "UnsignedInput")
    }

    @inline
    override def parseBody(r: SigmaByteReader): UnsignedInput =
      new UnsignedInput(ADKey @@ r.getBytes(BoxId.size))
  }
}

case class Input(override val boxId: BoxId, spendingProof: ProverResult)
  extends UnsignedInput(boxId) {
}

object Input {
  object serializer extends Serializer[Input, Input] {

    override def serializeBody(obj: Input, w: SigmaByteWriter): Unit = {
      SerializeLog.logPrintf(true, true, false, "Input")

      SerializeLog.logPrintf(true, true, false, "boxId")
      w.putBytes(obj.boxId)
      SerializeLog.logPrintf(false, true, false, "boxId")

      SerializeLog.logPrintf(true, true, false, "spendingProof")
      ProverResult.serializer.serializeBody(obj.spendingProof, w)
      SerializeLog.logPrintf(false, true, false, "spendingProof")

      SerializeLog.logPrintf(false, true, false, "Input")
    }

    override def parseBody(r: SigmaByteReader): Input = {
      val boxId = r.getBytes(BoxId.size)
      val spendingProof = ProverResult.serializer.parseBody(r)
      Input(ADKey @@ boxId, spendingProof)
    }
  }
}
