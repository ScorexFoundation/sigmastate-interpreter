package org.ergoplatform

import io.circe._
import io.circe.syntax._
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.{AvlTreeData, SType}
import sigmastate.Values.EvaluatedValue
import sigmastate.eval.{CAvlTree, WrapperOf}
import sigmastate.serialization.ValueSerializer
import special.collection.Coll
import special.sigma.{Header, PreHeader}

trait JsonCodecs {
  //TODO: remove in ergo

  implicit val sigmaBigIntEncoder: Encoder[special.sigma.BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.toString).asJson
  }

  implicit val arrayBytesEncoder: Encoder[Array[Byte]] = Algos.encode(_).asJson
  implicit val collBytesEncoder: Encoder[Coll[Byte]] = Algos.encode(_).asJson
//  implicit val byteSeqEncoder: Encoder[IndexedSeq[Byte]] = { in => Algos.encode(in.toArray).asJson }

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson

  implicit val digest32Encoder: Encoder[Digest32] = _.array.asJson

  implicit val modifierIdEncoder: Encoder[ModifierId] = _.asJson

  implicit val headerEncoder: Encoder[Header] = { h: Header =>
    Map(
      "id" -> h.id.asJson,
      "version" -> h.version.asJson,
      "parentId" -> h.parentId.asJson,
      "adProofsRoot" -> h.ADProofsRoot.asJson,
      // TODO: fix
      "stateRoot" -> h.stateRoot.asInstanceOf[WrapperOf[AvlTreeData]].wrappedValue.asJson,
      "transactionsRoot" -> h.transactionsRoot.asJson,
      "timestamp" -> h.timestamp.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "extensionRoot" -> h.extensionRoot.asJson,
      "minerPk" -> h.minerPk.getEncoded.asJson,
      "powOnetimePk" -> h.powOnetimePk.getEncoded.asJson,
      "powNonce" -> h.powNonce.asJson,
      "powDistance" -> h.powDistance.asJson,
      "votes" -> h.votes.asJson
    ).asJson
  }

  implicit val preHeaderEncoder: Encoder[PreHeader] = { v: PreHeader =>
    Map(
      "version" -> v.version.asJson,
      "parentId" -> v.parentId.asJson,
      "timestamp" -> v.timestamp.asJson,
      "nBits" -> v.nBits.asJson,
      "height" -> v.height.asJson,
      "minerPk" -> v.minerPk.getEncoded.asJson,
      "votes" -> v.votes.asJson
    ).asJson
  }

  implicit val evaluatedValueEncoder: Encoder[EvaluatedValue[SType]] = { value =>
    ValueSerializer.serialize(value).asJson
  }
}
