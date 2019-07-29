package org.ergoplatform

import io.circe._
import io.circe.syntax._
import sigmastate.helpers.SigmaTestingCommons

class JsonSerializationSpec extends SigmaTestingCommons {

  property("ErgoFullBlock should be encoded into JSON and decoded back correctly") {

    // TODO: implement ErgoLikeContext generator
    val ctx: ErgoLikeContext = ???

    val ctxJson: Json = ctx.asJson
    val ctxDecoded: ErgoLikeContext = ctxJson.as[ErgoLikeContext].toTry.get

    // TODO: implement ErgoLikeContext.equals
    ctxDecoded shouldEqual ctx
  }

}
