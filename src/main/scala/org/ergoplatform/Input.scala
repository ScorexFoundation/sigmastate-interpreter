package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import sigmastate.interpreter.SerializedProverResult
import sigmastate.serialization.Serializer
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.Try


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
    override def toBytes(input: UnsignedInput): Array[Byte] = serializeBody(input)

    override def parseBytes(bytes: Array[Byte]): Try[UnsignedInput] =
      Try(parseBody(bytes, 0)._1)

    override def parseBody(bytes: Array[Byte], pos: Position): (UnsignedInput, Consumed) = {
      new UnsignedInput(ADKey @@ bytes.slice(pos, pos + BoxId.size)) -> BoxId.size
    }

    @inline
    override def serializeBody(input: UnsignedInput): Array[Byte] = input.boxId
  }
}

case class Input(override val boxId: BoxId, spendingProof: SerializedProverResult)
  extends UnsignedInput(boxId) {
}

object Input {
  object serializer extends Serializer[Input, Input] {
    override def toBytes(input: Input): Array[Byte] = {
      input.boxId ++ SerializedProverResult.serializer.toBytes(input.spendingProof)
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (Input, Consumed) = {
      val boxId = bytes.slice(pos, pos + BoxId.size)
      val (spendingProof, consumed) = SerializedProverResult.serializer.parseBody(bytes, pos + BoxId.size)
      Input(ADKey @@ boxId, spendingProof) -> (consumed + BoxId.size)
    }
  }
}
