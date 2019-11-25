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

  //  def seller(ctx: Context, deadline: Int, pkB: SigmaProp): SigmaProp = {
  //    import ctx._
  //    (HEIGHT > deadline && pkB) || {
  //      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
  //      allOf(Coll[Boolean](
  //        OUTPUTS(1).value >= 100L,
  //        knownBoxId,
//        OUTPUTS(1).propositionBytes == pkB.propBytes
//      ))
//    }
//  }
}

case object AssetsAtomicExchangeVerification extends AssetsAtomicExchange {

  def proveBuyerCanWithdrawAfterDeadline(ctx: Context,
                                         deadline: Int,
                                         tokenId: Coll[Byte],
                                         tokenAmount: Long,
                                         pkA: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT > deadline && pkA.isValid)
    buyer(ctx, deadline, tokenId, tokenAmount, pkA).isValid
  } holds

  def proveBuyerCannotWithdrawBeforeDeadline(ctx: Context,
                                             deadline: Int,
                                             tokenId: Coll[Byte],
                                             tokenAmount: Long,
                                             pkA: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT <= deadline &&
      pkA.isValid &&
      (OUTPUTS.nonEmpty &&
        OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].isDefined &&
        OUTPUTS(0).R4[Coll[Byte]].isDefined &&
        OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get.nonEmpty
        ) &&
      !(OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)._1 == tokenId &&
        OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)._2 >= tokenAmount &&
        OUTPUTS(0).propositionBytes == pkA.propBytes &&
        OUTPUTS(0).R4[Coll[Byte]].get == SELF.id)
    )
    buyer(ctx, deadline, tokenId, tokenAmount, pkA).isValid
  } ensuring (_ == false)

}
