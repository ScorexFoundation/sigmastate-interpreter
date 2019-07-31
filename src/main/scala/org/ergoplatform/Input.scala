package org.ergoplatform

import java.util

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

/**
  * Inputs, that are used to enrich script context, but won't be spent by the transaction
  *
  * @param boxId - id of a box to add into context (should be in UTXO)
  */
case class DataInput(boxId: BoxId) {
  override def toString: String = s"DataInput(${Algos.encode(boxId)})"
}

object DataInput extends JsonCodecs {

  implicit val jsonEncoder: Encoder[DataInput] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
    )
  }

  implicit val jsonDecoder: Decoder[DataInput] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
    } yield DataInput(boxId)
  }

}

/**
  * Inputs of formed, but unsigned transaction
  *
  * @param boxId     - id of a box to spent
  * @param extension - user-defined variables to be put into context
  */
class UnsignedInput(val boxId: BoxId, val extension: ContextExtension) {

  def this(boxId: BoxId) = this(boxId, ContextExtension.empty)

  require(boxId.size == BoxId.size, s"incorrect boxId size, expected: $BoxId.size, got: ${boxId.size}")

  // todo check whether it is correct to compare inputs (Input use the same equals) by boxId only?
  override def equals(obj: Any): Boolean = obj match {
    case x: UnsignedInput => util.Arrays.equals(boxId, x.boxId)
    case _ => false
  }

  /**
    * Input, that should be signed by prover and verified by verifier.
    * Contains all the input data except of signature itself.
    */
  def inputToSign: Input = Input(boxId: BoxId, ProverResult(Array[Byte](), extension))
}

/**
  * Fully signed transaction input
  *
  * @param boxId         - id of a box to spent
  * @param spendingProof - proof of spending correctness
  */
case class Input(override val boxId: BoxId, spendingProof: ProverResult)
  extends UnsignedInput(boxId, spendingProof.extension) {
  override def toString: String = s"Input(${Algos.encode(boxId)},$spendingProof)"
}

object Input extends JsonCodecs {

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

  implicit val jsonEncoder: Encoder[Input] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "spendingProof" -> input.spendingProof.asJson
    )
  }

  implicit val jsonDecoder: Decoder[Input] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      proof <- cursor.downField("spendingProof").as[ProverResult]
    } yield Input(boxId, proof)
  }

}
