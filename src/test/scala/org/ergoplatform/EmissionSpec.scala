package org.ergoplatform

import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.settings.MonetarySettings
import org.scalacheck.Gen
import sigmastate.helpers.SigmaTestingCommons

class EmissionSpec extends SigmaTestingCommons {

  private val settings = MonetarySettings(30 * 2 * 24 * 365, 90 * 24 * 30, 75L * EmissionRules.CoinsInOneErgo,
    3L * EmissionRules.CoinsInOneErgo, 720, 75L * EmissionRules.CoinsInOneErgo / 10)
  private val emission = new EmissionRules(settings)

  def collectedFoundationReward(height: Int): Long = {
    (1 to height).map { h =>
      emission.foundationRewardAtHeight(h)
    }.sum
  }

  property("emission rules vectors") {
    emission.blocksTotal shouldBe 2080799
    emission.coinsTotal shouldBe 97739925L * EmissionRules.CoinsInOneErgo
    emission.foundersCoinsTotal shouldBe 4330792.5 * EmissionRules.CoinsInOneErgo
    emission.minersCoinsTotal shouldBe 93409132.5 * EmissionRules.CoinsInOneErgo

    emission.issuedCoinsAfterHeight(emission.blocksTotal) shouldBe emission.coinsTotal
    emission.issuedCoinsAfterHeight(1) shouldBe settings.fixedRate
  }

  property("correct sum from miner and foundation parts") {
    // collect coins after the fixed rate period
    forAll(Gen.choose(1, emission.blocksTotal)) { height =>
      val currentRate = emission.emissionAtHeight(height)
      val minerPart = emission.minersRewardAtHeight(height)
      val foundationPart = emission.foundationRewardAtHeight(height)
      foundationPart + minerPart shouldBe currentRate
    }
  }

  property("correct remainingFoundationRewardAtHeight") {
    val totalFoundersReward = collectedFoundationReward(emission.blocksTotal)

    def checkHeight(height: Int) = {
      val collectedFoundersPart = collectedFoundationReward(height)
      val remainingFoundersPart = emission.remainingFoundationRewardAtHeight(height)
      remainingFoundersPart + collectedFoundersPart shouldBe totalFoundersReward

    }
    // collect coins after the fixed rate period
    forAll(Gen.choose(1, emission.blocksTotal)) { height =>
      checkHeight(height)
    }
    checkHeight(settings.fixedRatePeriod)
    checkHeight(settings.fixedRatePeriod + settings.epochLength)
    checkHeight(settings.fixedRatePeriod + 2 * settings.epochLength)
    checkHeight(settings.fixedRatePeriod + 8 * settings.epochLength)
  }

}
