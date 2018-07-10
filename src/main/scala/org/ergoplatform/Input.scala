package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.Serializer
import sigmastate.utils.{ByteReader, ByteWriter}


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
    override def serializeBody(obj: UnsignedInput, w: ByteWriter): Unit =
      w.putBytes(obj.boxId)

    @inline
    override def parseBody(r: ByteReader): UnsignedInput =
      new UnsignedInput(ADKey @@ r.getBytes(BoxId.size))
  }
}

case class Input(override val boxId: BoxId, spendingProof: ProverResult)
  extends UnsignedInput(boxId) {
}

object Input {
  object serializer extends Serializer[Input, Input] {

    override def serializeBody(obj: Input, w: ByteWriter): Unit = {
      w.putBytes(obj.boxId)
      ProverResult.serializer.serializeBody(obj.spendingProof, w)
    }

    override def parseBody(r: ByteReader): Input = {
      val boxId = r.getBytes(BoxId.size)
      val spendingProof = ProverResult.serializer.parseBody(r)
      Input(ADKey @@ boxId, spendingProof)
    }
  }
}
