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
     | //let projectPubKey = proveDlog(project)  //getVar[SigmaProp](projectPubKeyId)
     | //let backerPubKey = proveDlog(backer)  //getVar[SigmaProp](backerPubKeyId)
     | val c1 = HEIGHT >= timeout && backerPubKey
     | val c2 = allOf(Array(
     |   HEIGHT < timeout,
     |   projectPubKey,
     |   OUTPUTS.exists { (out: Box) =>
     |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
     |   }
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
     | val c2 = allOf(Array(
     |   HEIGHT >= SELF.R4[Long].get + demurragePeriod,
     |   OUTPUTS.exists { (out: Box) =>
     |     out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
     |   }
     | ))
     | //getVar[SigmaProp](regScriptId) || c2
     | regScript || c2
     | }
    """.stripMargin

}
