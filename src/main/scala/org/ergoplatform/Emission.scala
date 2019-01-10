package org.ergoplatform

object Emission {

  val CoinsInOneErgo: Long = 1000000000

  /**
    * Emission rules.
    * Return number of coins, issued at height `h` and all previous heights
    */
  def issuedCoinsAtHeight(h: Int,
                          fixedRatePeriod: Int,
                          fixedRate: Long,
                          epochLength: Int,
                          oneEpochReduction: Long): Long = {
    if (h < fixedRatePeriod) {
      fixedRate * h
    } else {
      val fixedRateIssue: Long = fixedRate * (fixedRatePeriod - 1)
      val epoch = (h - fixedRatePeriod) / epochLength
      val fullEpochsIssued: Long = (1 to epoch.toInt).map { e =>
        Math.max(fixedRate - oneEpochReduction * e, 0) * epochLength
      }.sum
      val heightInThisEpoch = (h - fixedRatePeriod) % epochLength + 1
      val rateThisEpoch = Math.max(fixedRate - oneEpochReduction * (epoch + 1), 0)
      val thisEpochIssued = heightInThisEpoch * rateThisEpoch

      fullEpochsIssued + fixedRateIssue + thisEpochIssued
    }
  }

  /**
    * Return number of coins, issued at height `h` in favour of a miner
    */
  def minersRewardAtHeight(h: Long,
                           fixedRatePeriod: Int,
                           fixedRate: Long,
                           epochLength: Int,
                           oneEpochReduction: Long,
                           foundersInitialReward: Long): Long = {
    if (h < fixedRatePeriod + 2 * epochLength) {
      fixedRate - foundersInitialReward
    } else {
      val epoch = 1 + (h - fixedRatePeriod) / epochLength
      Math.max(fixedRate - oneEpochReduction * epoch, 0)
    }
  }

  /**
    * Return number of coins, issued at height `h` in favour of the foundation
    */
  def foundationRewardAtHeight(h: Long,
                               fixedRatePeriod: Int,
                               fixedRate: Long,
                               epochLength: Int,
                               oneEpochReduction: Long,
                               foundersInitialReward: Long): Long = {
    if (h < fixedRatePeriod) {
      foundersInitialReward
    } else if (h < fixedRatePeriod + epochLength) {
      foundersInitialReward - oneEpochReduction
    } else if (h < fixedRatePeriod + (2 * epochLength)) {
      foundersInitialReward - 2 * oneEpochReduction
    } else {
      0
    }
  }

  /**
    * Return number of coins, issued at height `h`
    */
  def emissionAtHeight(h: Long,
                       fixedRatePeriod: Int,
                       fixedRate: Long,
                       epochLength: Int,
                       oneEpochReduction: Long): Long = {
    if (h < fixedRatePeriod) {
      fixedRate
    } else {
      val epoch = 1 + (h - fixedRatePeriod) / epochLength
      Math.max(fixedRate - oneEpochReduction * epoch, 0)
    }
  }

}
