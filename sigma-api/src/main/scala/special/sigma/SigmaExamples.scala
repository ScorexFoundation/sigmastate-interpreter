package special.sigma

import special.collection.Coll

import scalan.RType
import RType._

trait CrowdFunding extends SigmaContract {
  def deadline: Long
  def minToRaise: Long
  def backerPubKey: SigmaProp
  def projectPubKey: SigmaProp

  @clause def canOpen(ctx: Context) = verifyZK {
    val fundraisingFailure = sigmaProp(ctx.HEIGHT >= deadline) && backerPubKey
    val enoughRaised = (outBox: Box) => {
      outBox.value >= minToRaise &&
          outBox.propositionBytes == projectPubKey.propBytes
    }
    val fundraisingSuccess = ctx.HEIGHT < deadline &&
        projectPubKey.isValid &&
        ctx.OUTPUTS.exists(enoughRaised)

    fundraisingFailure || fundraisingSuccess
  }
}

trait CrossChainAtomicSwap extends SigmaContract {
  def deadlineBob: Long
  def deadlineAlice: Long
  def pkA: SigmaProp
  def pkB: SigmaProp
  def hx: Coll[Byte]
  
  def templateForBobChain(ctx: Context) = verifyZK {
    anyZK(Collection(
      sigmaProp(ctx.HEIGHT > deadlineBob) && pkA,
      pkB && blake2b256(ctx.getVar[Coll[Byte]](1).get) == hx
    ))
  }

  def templateForAliceChain(ctx: Context) = verifyZK {
    val x = ctx.getVar[Coll[Byte]](1).get
    anyZK(Collection(
      sigmaProp(ctx.HEIGHT > deadlineAlice) && pkB,
      allZK( Collection(
        pkA,
        sigmaProp(x.length < 33),
        sigmaProp(blake2b256(x) == hx)
      ))
    ))
  }
}

trait InChainAtomicSwap extends SigmaContract {
  def deadline: Long
  def pkA: SigmaProp
  def pkB: SigmaProp
  def token1: Coll[Byte]

  def templateForAlice(ctx: Context) = verifyZK {
    (pkA && ctx.HEIGHT > deadline) || {
      val tokenData = ctx.OUTPUTS(0).tokens(0)
      allOf(Collection(
        tokenData._1 == token1,
        tokenData._2 >= 60L,
        ctx.OUTPUTS(0).propositionBytes == pkA.propBytes,
        ctx.OUTPUTS(0).value >= 1L
      ))
    }
  }
  def templateForBob(ctx: Context) = verifyZK {
    (pkB && ctx.HEIGHT > deadline) ||
        allOf( Collection(
          ctx.OUTPUTS(1).value >= 100,
          ctx.OUTPUTS(1).propositionBytes == pkB.propBytes
        ))
  }
}

trait CoinEmission extends SigmaContract {
  def fixedRatePeriod: Long
  def epochLength: Long
  def fixedRate: Long
  def oneEpochReduction: Long

  def templateForTotalAmountBox(ctx: Context) = {
    val epoch = 1L + ((ctx.HEIGHT - fixedRatePeriod) / epochLength)
    val out = ctx.OUTPUTS(0)
    val coinsToIssue =
      if (ctx.HEIGHT < fixedRatePeriod) fixedRate
      else fixedRate - (oneEpochReduction * epoch)
    val correctCoinsConsumed = coinsToIssue == (ctx.SELF.value - out.value)
    val sameScriptRule = ctx.SELF.propositionBytes == out.propositionBytes
    val heightIncreased = ctx.HEIGHT > ctx.SELF.R4[Long].get
    val heightCorrect = out.R4[Long].get == ctx.HEIGHT
    val lastCoins = ctx.SELF.value <= oneEpochReduction
    allOf(Collection(
      correctCoinsConsumed,
      heightCorrect,
      heightIncreased,
      sameScriptRule)) ||
      (heightIncreased && lastCoins)
  }
}

trait DemurrageCurrency extends SigmaContract {
  def demurragePeriod: Int
  def demurrageCost: Long
  def regScript: SigmaProp

  @clause def canOpen(ctx: Context) = verifyZK {
    val c2 =
      ctx.HEIGHT >= ctx.SELF.R4[Int].get + demurragePeriod &&
      ctx.OUTPUTS.exists(out => {
        out.value >= ctx.SELF.value - demurrageCost && out.propositionBytes == ctx.SELF.propositionBytes
      })
    regScript || c2
  }
}

