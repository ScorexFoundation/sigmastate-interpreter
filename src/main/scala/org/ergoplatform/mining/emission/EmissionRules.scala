package org.ergoplatform.mining.emission

import org.ergoplatform.settings.MonetarySettings

import scala.annotation.tailrec

/**
  * Ergo coin emission curve.
  *
  * Mainnet properties:
  * 1000000000 parts of one coin
  * block every 2 minutes
  * fixed rate 75 coins during first 2 years
  * reward reduction for 3 coins every 3 month after that
  * 19710000 coins after the first year
  * 97739925 coins total
  *
  * @param settings - network settings
  */
class EmissionRules(val settings: MonetarySettings) {

  lazy val (coinsTotal, blocksTotal) = {
    @tailrec
    def loop(height: Int, acc: Long): (Long, Int) = {
      val currentRate = emissionAtHeight(height)
      if (currentRate > 0) {
        loop(height + 1, acc + currentRate)
      } else {
        (acc, height - 1)
      }
    }

    loop(1, 0)
  }

  val foundersCoinsTotal: Long = remainingFoundationRewardAtHeight(0)
  val minersCoinsTotal: Long = coinsTotal - foundersCoinsTotal

  /**
    * Emission rules.
    * Return number of coins, issued at height `h` and all previous heights
    */
  def issuedCoinsAfterHeight(h: Long): Long = {
    if (h < settings.fixedRatePeriod) {
      settings.fixedRate * h
    } else {
      val fixedRateIssue: Long = settings.fixedRate * (settings.fixedRatePeriod - 1)
      val epoch = (h - settings.fixedRatePeriod) / settings.epochLength
      val fullEpochsIssued: Long = (1 to epoch.toInt).map { e =>
        Math.max(settings.fixedRate - settings.oneEpochReduction * e, 0) * settings.epochLength
      }.sum
      val heightInThisEpoch = (h - settings.fixedRatePeriod) % settings.epochLength + 1
      val rateThisEpoch = Math.max(settings.fixedRate - settings.oneEpochReduction * (epoch + 1), 0)
      val thisEpochIssued = heightInThisEpoch * rateThisEpoch

      fullEpochsIssued + fixedRateIssue + thisEpochIssued
    }
  }

  /**
    * Number not issued yet coins, after height `h`
    */
  def remainingCoinsAfterHeight(h: Long): Long = coinsTotal - issuedCoinsAfterHeight(h)

  /**
    * Number of coins to be issued at height `h`
    */
  def emissionAtHeight(h: Long): Long = {
    if (h < settings.fixedRatePeriod) {
      settings.fixedRate
    } else {
      val epoch = 1 + (h - settings.fixedRatePeriod) / settings.epochLength
      Math.max(settings.fixedRate - settings.oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")

  /**
    * Return number of coins, issued at height `h` in favour of a miner
    */
  def minersRewardAtHeight(h: Long): Long = {
    if (h < settings.fixedRatePeriod + 2 * settings.epochLength) {
      settings.fixedRate - settings.foundersInitialReward
    } else {
      val epoch = 1 + (h - settings.fixedRatePeriod) / settings.epochLength
      Math.max(settings.fixedRate - settings.oneEpochReduction * epoch, 0)
    }
  }

  /**
    * Return number of coins, that should be kept in the foundation box at height `h`
    */
  def remainingFoundationRewardAtHeight(h: Long): Long = {
    val foundersInitialReward = settings.foundersInitialReward
    val oneEpochReduction = settings.oneEpochReduction
    val epochLength = settings.epochLength
    val fixedRatePeriod = settings.fixedRatePeriod
    val full15reward = (foundersInitialReward - 2 * oneEpochReduction) * epochLength
    val full45reward = (foundersInitialReward - oneEpochReduction) * epochLength

    if (h < fixedRatePeriod) {
      full15reward + full45reward + (fixedRatePeriod - h - 1) * foundersInitialReward
    } else if (h < fixedRatePeriod + epochLength) {
      full15reward + (foundersInitialReward - oneEpochReduction) * (fixedRatePeriod + epochLength - h - 1)
    } else if (h < fixedRatePeriod + (2 * epochLength)) {
      (foundersInitialReward - 2 * oneEpochReduction) * (fixedRatePeriod + (2 * epochLength) - h - 1)
    } else {
      0
    }
  }

  /**
    * Return number of coins, issued at height `h` in favour of the foundation
    */
  def foundationRewardAtHeight(h: Long): Long = {
    if (h < settings.fixedRatePeriod) {
      settings.foundersInitialReward
    } else if (h < settings.fixedRatePeriod + settings.epochLength) {
      settings.foundersInitialReward - settings.oneEpochReduction
    } else if (h < settings.fixedRatePeriod + (2 * settings.epochLength)) {
      settings.foundersInitialReward - 2 * settings.oneEpochReduction
    } else {
      0
    }
  }

}

object EmissionRules {

  val CoinsInOneErgo: Long = 1000000000

}

