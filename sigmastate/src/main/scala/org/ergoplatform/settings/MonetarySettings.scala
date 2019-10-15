package org.ergoplatform.settings

import org.ergoplatform.ErgoScriptPredef
import org.ergoplatform.mining.emission.EmissionRules
import sigmastate.Values.ErgoTree

/**
  * Configuration file for monetary settings of Ergo chain
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class MonetarySettings(fixedRatePeriod: Int = 30 * 2 * 24 * 365,
                            epochLength: Int = 90 * 24 * 30,
                            fixedRate: Long = 75L * EmissionRules.CoinsInOneErgo,
                            oneEpochReduction: Long = 3L * EmissionRules.CoinsInOneErgo,
                            minerRewardDelay: Int = 720,
                            foundersInitialReward: Long = 75L * EmissionRules.CoinsInOneErgo / 10) {

  val feeProposition: ErgoTree = ErgoScriptPredef.feeProposition(minerRewardDelay)
  val feePropositionBytes: Array[Byte] = feeProposition.bytes
  val emissionBoxProposition: ErgoTree = ErgoScriptPredef.emissionBoxProp(this)
  val foundersBoxProposition: ErgoTree = ErgoScriptPredef.foundationScript(this)

}
