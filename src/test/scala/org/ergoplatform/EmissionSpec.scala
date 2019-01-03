package org.ergoplatform

import sigmastate.helpers.SigmaTestingCommons

import scala.annotation.tailrec

class EmissionSpec extends SigmaTestingCommons {

  property("issuedCoinsAfterHeight corresponds to emissionAtHeight") {
    val genesisHeight = 1
    val fixedRatePeriod = 525600
    val epochLength = 64800
    val fixedRate = 75L * Emission.CoinsInOneErgo
    val oneEpochReduction = 3 * Emission.CoinsInOneErgo

    val (coinsTotal, blocksTotal) = {
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
    coinsTotal shouldBe 97739925L * Emission.CoinsInOneErgo
    val ct2 = Emission.issuedCoinsAfterHeight(blocksTotal, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction)
    ct2 shouldBe coinsTotal

    // first block issue
    Emission.issuedCoinsAfterHeight(1, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction) shouldBe fixedRate

  }

}
