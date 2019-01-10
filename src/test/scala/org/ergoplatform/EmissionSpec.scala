package org.ergoplatform

import org.scalacheck.Gen
import sigmastate.Values.ByteArrayConstant
import sigmastate.helpers.SigmaTestingCommons

import scala.annotation.tailrec

class EmissionSpec extends SigmaTestingCommons {

  private val EmptyHeight = 0
  private val GenesisHeight = EmptyHeight + 1
  private val BlocksTotal = 2080799
  private val FixedRatePeriod = 525600
  private val EpochLength = 64800
  private val MinerRewardDelay = 720
  private val fixedRate = 75L * Emission.CoinsInOneErgo
  private val oneEpochReduction = 3 * Emission.CoinsInOneErgo
  private val foundersInitialReward: Long = fixedRate / 10
  private val coinsTotal = 97739925L * Emission.CoinsInOneErgo

  property("Emission boxes") {
    val minersRewardTotal = Emission.minersRewardAtHeight(BlocksTotal, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction, foundersInitialReward)
    val foundationRewardTotal = Emission.foundationRewardAtHeight(BlocksTotal, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction, foundersInitialReward)
    val noPremineProofs = Array("The Guardian Headline", "Last BTC block id", "another source of randomness")
      .map(_.getBytes("UTF-8"))
    val boxes = Emission.emissionBoxes(FixedRatePeriod: Int,
      EpochLength: Int,
      fixedRate: Long,
      oneEpochReduction: Long,
      MinerRewardDelay: Int,
      EmptyHeight: Int,
      minersRewardTotal: Long,
      foundationRewardTotal: Long,
      noPremineProofs: Seq[Array[Byte]])
    val emissionBox = boxes.head
    emissionBox.value shouldBe minersRewardTotal
    val prop = ErgoScriptPredef.emissionBoxProp(FixedRatePeriod, EpochLength, fixedRate, oneEpochReduction, MinerRewardDelay)
    emissionBox.proposition shouldBe prop

    val noPremineBox = boxes(1)
    noPremineProofs.foreach { p =>
      noPremineBox.additionalRegisters.values.exists(_ == ByteArrayConstant(p)) shouldBe true
    }
  }

  property("correct sum from miner and foundation parts") {
    // collect coins after the fixed rate period
    forAll(Gen.choose(1, BlocksTotal)) { height =>
      val currentRate = Emission.emissionAtHeight(height, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction)
      val minerPart = Emission.minersRewardAtHeight(height, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction, foundersInitialReward)
      val foundationPart = Emission.foundationRewardAtHeight(height, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction, foundersInitialReward)
      foundationPart + minerPart shouldBe currentRate
    }
  }

  property("issuedCoinsAfterHeight corresponds to emissionAtHeight") {
    val (coinsTotalCalced, blocksTotalCalced) = {
      @tailrec
      def loop(height: Int, acc: Long): (Long, Int) = {
        val currentRate = Emission.emissionAtHeight(height, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction)
        if (currentRate > 0) {
          loop(height + 1, acc + currentRate)
        } else {
          (acc, height - 1)
        }
      }

      loop(GenesisHeight, 0)
    }
    // check total emission
    coinsTotalCalced shouldBe coinsTotal
    blocksTotalCalced shouldBe BlocksTotal
    val ct2 = Emission.issuedCoinsAtHeight(BlocksTotal, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction)
    ct2 shouldBe coinsTotal

    // first block issue
    Emission.issuedCoinsAtHeight(1, FixedRatePeriod, fixedRate, EpochLength, oneEpochReduction) shouldBe fixedRate

  }

}
