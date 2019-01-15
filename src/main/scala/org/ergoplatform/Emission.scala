package org.ergoplatform

import sigmastate.Values
import sigmastate.Values.ByteArrayConstant

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
    * Return number of coins, that should be kept in the foundation box at height `h`
    */
  def remainingFoundationRewardAtHeight(h: Long,
                                        fixedRatePeriod: Int,
                                        epochLength: Int,
                                        oneEpochReduction: Long,
                                        foundersInitialReward: Long): Long = {
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
  def foundationRewardAtHeight(h: Long,
                               fixedRatePeriod: Int,
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
