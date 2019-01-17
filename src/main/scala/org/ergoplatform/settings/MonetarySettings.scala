package org.ergoplatform.settings

import org.ergoplatform.mining.emission.EmissionRules
import scorex.crypto.authds.ADDigest
import scorex.util.encode.Base16

import scala.util.Success

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
                            foundersInitialReward: Long = 75L * EmissionRules.CoinsInOneErgo / 10)