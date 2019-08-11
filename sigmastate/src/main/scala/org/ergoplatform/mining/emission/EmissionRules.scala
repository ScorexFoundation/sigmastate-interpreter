package org.ergoplatform.mining.emission

import org.ergoplatform.settings.MonetarySettings
import sigmastate.verified.{EmissionRulesVerified, MonetarySettingsErgoLaunch}

import scala.annotation.tailrec

/**
  * Ergo coin emission curve.
  *
  * Mainnet properties:
  * 1000000000 nanoErgs (minimal non-divisible parts) in one Erg
  * a block is coming every 2 minutes
  * fixed rate 75 coins during first 2 years
  * reward reduction for 3 coins every 3 month after that
  * 19710000 coins after the first year
  * 97739925 coins total
  *
  * @param settings - network settings
  */
class EmissionRules(val settings: MonetarySettings) {

  lazy val (coinsTotal, blocksTotal) = EmissionRulesVerified.totalCoinsAndBlocks(settings)

  val foundersCoinsTotal: Long = remainingFoundationRewardAtHeight(0)
  val minersCoinsTotal: Long = coinsTotal - foundersCoinsTotal

  /**
    * Returns number of coins issued at height `h` and before that
    */
  def issuedCoinsAfterHeight(h: Int): Long = EmissionRulesVerified.issuedCoinsAfterHeight(h, settings)

  /**
    * Number not issued yet coins after height `h`
    */
  def remainingCoinsAfterHeight(h: Int): Long = coinsTotal - issuedCoinsAfterHeight(h)

  /**
    * Number of coins to be issued at height `h`
    */
  def emissionAtHeight(h: Int): Long = EmissionRulesVerified.emissionAtHeight(h, settings)

  /**
    * Returns number of coins issued at height `h` in favour of a miner
    */
  def minersRewardAtHeight(h: Int): Long = EmissionRulesVerified.minersRewardAtHeight(h, settings)

  /**
    * Returns number of coins which should be kept in the foundation box at height `h`
    */
  def remainingFoundationRewardAtHeight(h: Int): Long = EmissionRulesVerified.remainingFoundationRewardAtHeight(h, settings)

  /**
    * Returns number of coins issued at height `h` in favour of the foundation
    */
  def foundationRewardAtHeight(h: Int): Long = EmissionRulesVerified.foundationRewardAtHeight(h, settings)

}

object EmissionRules {

  val CoinsInOneErgo: Long = EmissionRulesVerified.CoinsInOneErgo

}

