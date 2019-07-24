package org.ergoplatform

import io.circe._
import io.circe.syntax._
import org.ergoplatform.settings.Algos
import sigmastate.AvlTreeData
import sigmastate.eval.{CAvlTree, WrapperOf}
import special.sigma.Header

trait JsonCodecs {

  implicit val sigmaBigIntEncoder: Encoder[special.sigma.BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.toString).asJson
  }

  implicit val headerEncoder: Encoder[Header] = { h: Header =>
    Map(
      "id" -> Algos.encode(h.id).asJson,
      "version" -> h.version.asJson,
      "parentId" -> Algos.encode(h.parentId).asJson,
      "adProofsRoot" -> Algos.encode(h.ADProofsRoot).asJson,
      // TODO: fix
      "stateRoot" -> h.stateRoot.asInstanceOf[WrapperOf[AvlTreeData]].wrappedValue.asJson,
      "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
      "timestamp" -> h.timestamp.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "extensionRoot" -> Algos.encode(h.extensionRoot).asJson,
      "minerPk" -> Algos.encode(h.minerPk.getEncoded).asJson,
      "powOnetimePk" -> Algos.encode(h.powOnetimePk.getEncoded).asJson,
      "powNonce" -> Algos.encode(h.powNonce).asJson,
      "powDistance" -> h.powDistance.asJson,
      "votes" -> Algos.encode(h.votes).asJson
    ).asJson
  }

}
