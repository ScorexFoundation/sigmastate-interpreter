package sigmastate.eval

import scalan.BaseCtxTests

trait ExampleContracts extends ErgoScriptTestkit { self: BaseCtxTests =>

  val envCF = Map(
    "timeout" -> timeout,
    "minToRaise" -> minToRaise,
    "backerPubKeyId" -> backerPubKeyId,
    "projectPubKeyId" -> projectPubKeyId
  )

  val crowdFundingScript =
    """{
     | let projectPubKey = getVar[SigmaProp](projectPubKeyId)
     | let c1 = HEIGHT >= timeout && getVar[SigmaProp](backerPubKeyId)
     | let c2 = allOf(Array(
     |   HEIGHT < timeout,
     |   projectPubKey,
     |   OUTPUTS.exists(fun (out: Box) = {
     |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
     |   })
     | ))
     | c1 || c2
     | }
    """.stripMargin

  val demurragePeriod = 100L
  val demurrageCost = 2L
  val regScriptId = 1.toByte
  val envDem = Map(
    "demurragePeriod" -> demurragePeriod,
    "demurrageCost" -> demurrageCost,
    "regScriptId" -> regScriptId,
  )

  val demurrageScript =
    """{
     | let c2 = allOf(Array(
     |   HEIGHT >= SELF.R4[Long].value + demurragePeriod,
     |   OUTPUTS.exists(fun (out: Box) = {
     |     out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
     |   })
     | ))
     | getVar[SigmaProp](regScriptId) || c2
     | }
    """.stripMargin

}
