package org.ergoplatform.settings

import org.ergoplatform.ErgoScriptPredef
import org.ergoplatform.mining.emission.EmissionRules
import sigmastate.Values.ErgoTree
import sigmastate.verified.MonetarySettingsErgoLaunch

/**
  * Configuration file for monetary settings of Ergo chain
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class MonetarySettings(fixedRatePeriod: Int = MonetarySettingsErgoLaunch.fixedRatePeriod,
                            epochLength: Int = MonetarySettingsErgoLaunch.epochLength,
                            fixedRate: Long = MonetarySettingsErgoLaunch.fixedRate,
                            oneEpochReduction: Long = MonetarySettingsErgoLaunch.oneEpochReduction,
                            minerRewardDelay: Int = MonetarySettingsErgoLaunch.minerRewardDelay,
                            foundersInitialReward: Long = MonetarySettingsErgoLaunch.foundersInitialReward)
  extends sigmastate.verified.MonetarySettings {

  val feeProposition: ErgoTree = ErgoScriptPredef.feeProposition(minerRewardDelay)
  val feePropositionBytes: Array[Byte] = feeProposition.bytes
  val emissionBoxProposition: ErgoTree = ErgoScriptPredef.emissionBoxProp(this)
  val foundersBoxProposition: ErgoTree = ErgoScriptPredef.foundationScript(this)

}
