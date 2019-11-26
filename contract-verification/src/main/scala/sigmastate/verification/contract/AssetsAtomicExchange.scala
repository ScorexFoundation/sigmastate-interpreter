package sigmastate.verification.contract

import sigmastate.verified._
import stainless.lang._

import scala.language.{implicitConversions, postfixOps}

sealed abstract class AssetsAtomicExchange extends SigmaContract {

  def buyer(ctx: Context,
            deadline: Int,
            tokenId: Coll[Byte],
            tokenAmount: Long,
            pkA: SigmaProp): SigmaProp = {
    import ctx._
    (HEIGHT > deadline && pkA) || {
      (OUTPUTS.nonEmpty &&
        OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].isDefined &&
        OUTPUTS(0).R4[Coll[Byte]].isDefined
        ) && {
        val tokenDataColl = OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get
        val tokenDataCorrect = tokenDataColl.nonEmpty &&
          tokenDataColl(0)._1 == tokenId &&
          tokenDataColl(0)._2 >= tokenAmount

        val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
        // TODO fix Coll.fromItems crashing Inox typer and rewrite with allOf(Coll.fromItems[Boolean](
        tokenDataCorrect &&
          OUTPUTS(0).propositionBytes == pkA.propBytes &&
          knownId
      }
    }
  }

  def seller(ctx: Context, deadline: Int, ergAmount: Long, pkB: SigmaProp): SigmaProp = {
    import ctx._
    (HEIGHT > deadline && pkB) || (
      OUTPUTS.isDefinedAt(1) &&
        OUTPUTS(1).R4[Coll[Byte]].isDefined
      ) && {
      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
      OUTPUTS(1).value >= ergAmount &&
        knownBoxId &&
        OUTPUTS(1).propositionBytes == pkB.propBytes
    }
  }
}

case object AssetsAtomicExchangeBuyerVerification extends AssetsAtomicExchange {

  private def conditionOutBoxPresentWithSomeTokens(ctx: Context): Boolean = {
    import ctx._
    OUTPUTS.nonEmpty &&
      OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].isDefined &&
      OUTPUTS(0).R4[Coll[Byte]].isDefined &&
      OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get.nonEmpty
  }

  private def conditionCorrectClaimableTokenAmountAgainstBuyerBox(ctx: Context,
                                                         tokenId: Coll[Byte],
                                                         tokenAmount: Long,
                                                         pkA: SigmaProp): Boolean = {
    import ctx._
    conditionOutBoxPresentWithSomeTokens(ctx) &&
    OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)._1 == tokenId &&
      OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)._2 >= tokenAmount &&
      OUTPUTS(0).propositionBytes == pkA.propBytes &&
      OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
  }

  def proveBuyerCanClaimAfterDeadline(ctx: Context,
                                      deadline: Int,
                                      tokenId: Coll[Byte],
                                         tokenAmount: Long,
                                         pkA: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT > deadline && pkA.isValid)
    buyer(ctx, deadline, tokenId, tokenAmount, pkA).isValid
  } holds

  def proveBuyerCannotClaimBeforeDeadline(ctx: Context,
                                             deadline: Int,
                                             tokenId: Coll[Byte],
                                             tokenAmount: Long,
                                             pkA: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT <= deadline &&
      pkA.isValid &&
      !conditionCorrectClaimableTokenAmountAgainstBuyerBox(ctx, tokenId, tokenAmount, pkA)
    )
    buyer(ctx, deadline, tokenId, tokenAmount, pkA).isValid
  } ensuring (_ == false)

  def proveSpendableTokensAgainstThisOrderAnyTime(ctx: Context,
                         deadline: Int,
                         tokenId: Coll[Byte],
                         tokenAmount: Long,
                         pkA: SigmaProp): Boolean = {
    import ctx._
    require(conditionCorrectClaimableTokenAmountAgainstBuyerBox(ctx, tokenId, tokenAmount, pkA))
    buyer(ctx, deadline, tokenId, tokenAmount, pkA).isValid
  } holds
}


case object AssetsAtomicExchangeSellerVerification extends AssetsAtomicExchange {

  private def conditionClaimableWithCorrectErgAmount(ctx: Context,
                                                     ergAmount: Long,
                                                     pkB: SigmaProp): Boolean = {
    import ctx._
    OUTPUTS.isDefinedAt(1) &&
      OUTPUTS(1).R4[Coll[Byte]].isDefined &&
      OUTPUTS(1).value >= ergAmount &&
      OUTPUTS(1).R4[Coll[Byte]].get == SELF.id &&
      OUTPUTS(1).propositionBytes == pkB.propBytes
  }

  def proveSellerCanClaimAfterDeadline(ctx: Context,
                                       deadline: Int,
                                       ergAmount: Long,
                                       pkB: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT > deadline && pkB.isValid)
    seller(ctx, deadline, ergAmount, pkB).isValid
  } holds

  def proveSellerCannotClaimBeforeDeadline(ctx: Context,
                                           deadline: Int,
                                           ergAmount: Long,
                                           pkB: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT <= deadline &&
      pkB.isValid &&
      !conditionClaimableWithCorrectErgAmount(ctx, ergAmount, pkB))
    seller(ctx, deadline, ergAmount, pkB).isValid
  } ensuring (_ == false)

  def proveSpendableErgAgainstThisOrderAnyTime(ctx: Context,
                                               deadline: Int,
                                               ergAmount: Long,
                                               pkB: SigmaProp): Boolean = {
    import ctx._
    require(conditionClaimableWithCorrectErgAmount(ctx, ergAmount, pkB))
    seller(ctx, deadline, ergAmount, pkB).isValid
  } holds
}
