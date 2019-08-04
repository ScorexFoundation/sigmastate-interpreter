package org.ergoplatform

import io.circe._
import io.circe.syntax._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.SerializationSpecification

class JsonSerializationSpec extends SigmaTestingCommons with SerializationSpecification with JsonCodecs {

  property("ErgoLikeContext should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeContextGen) { ctx =>
      val ctxJson: Json = ctx.asJson
      val ctxDecoded: ErgoLikeContext = ctxJson.as[ErgoLikeContext].toTry.get
      ctxDecoded shouldEqual ctx
    }
  }

}
