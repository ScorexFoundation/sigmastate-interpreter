package org.ergoplatform

import sigmastate.helpers.SigmaTestingCommons

import scala.annotation.tailrec

class AlgosSpec extends SigmaTestingCommons {

  property("issuedCoinsAfterHeight corresponds to emissionAtHeight") {
    val genesisHeight = 1
    val fixedRatePeriod = 525600
    val epochLength = 64800
    val fixedRate = 75L * Algos.CoinsInOneErgo
    val oneEpochReduction = 3 * Algos.CoinsInOneErgo

    val (coinsTotal, blocksTotal) = {
      @tailrec
      def loop(height: Int, acc: Long): (Long, Int) = {
        val currentRate = Algos.emissionAtHeight(height, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction)
        if (currentRate > 0) {
          loop(height + 1, acc + currentRate)
        } else {
          (acc, height - 1)
        }
      }

      loop(genesisHeight, 0)
    }
    // check total emission
    coinsTotal shouldBe 97739925L * Algos.CoinsInOneErgo
    val ct2 = Algos.issuedCoinsAfterHeight(blocksTotal, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction)
    ct2 shouldBe coinsTotal

    // first block issue
    Algos.issuedCoinsAfterHeight(1, fixedRatePeriod, fixedRate, epochLength, oneEpochReduction) shouldBe fixedRate

  }

}
