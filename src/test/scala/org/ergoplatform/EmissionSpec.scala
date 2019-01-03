package org.ergoplatform

import org.scalacheck.Gen
import sigmastate.helpers.SigmaTestingCommons

import scala.annotation.tailrec

class EmissionSpec extends SigmaTestingCommons {

  val genesisHeight = 1
  val blocksTotal = 2080799
  val fixedRatePeriod = 525600
  val epochLength = 64800
  val fixedRate = 75L * Emission.CoinsInOneErgo
  val oneEpochReduction = 3 * Emission.CoinsInOneErgo
  val foundersInitialReward: Long = fixedRate / 10
  val minerRewardDelay = 720
  val coinsTotal = 97739925L * Emission.CoinsInOneErgo

  property("correct sum from miner and foundation parts") {
    // collect coins after the fixed rate period
    forAll(Gen.choose(1, blocksTotal)) { height =>
      val currentRate = Emission.emissionAtHeight(height, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction)
      val minerPart = Emission.minersRewardAtHeight(height, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction, foundersInitialReward)
      val foundationPart = Emission.foundationRewardAtHeight(height, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction, foundersInitialReward)
      foundationPart + minerPart shouldBe currentRate
    }
  }

  property("issuedCoinsAfterHeight corresponds to emissionAtHeight") {
    val (coinsTotalCalced, blocksTotalCalced) = {
      @tailrec
      def loop(height: Int, acc: Long): (Long, Int) = {
        val currentRate = Emission.emissionAtHeight(height, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction)
        if (currentRate > 0) {
          loop(height + 1, acc + currentRate)
        } else {
          (acc, height - 1)
        }
      }

      loop(genesisHeight, 0)
    }
    // check total emission
    coinsTotalCalced shouldBe coinsTotal
    blocksTotalCalced shouldBe blocksTotal
    val ct2 = Emission.issuedCoinsAfterHeight(blocksTotal, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction)
    ct2 shouldBe coinsTotal

    // first block issue
    Emission.issuedCoinsAfterHeight(1, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction) shouldBe fixedRate

  }

}
