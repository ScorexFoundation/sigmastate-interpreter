package sigmastate.verified

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import stainless.math._
import stainless.proof._

object CrowdFundingContractVerification {

  def arrayEquals(arr1: Array[Byte], arr2: Array[Byte]): Boolean = ???

  case class SigmaProp() {

    def isValid: Boolean = ???

    def propBytes: Array[Byte] = ???
  }

  case class Box(value: Long) {

    def propositionBytes: Array[Byte] = ???
  }

  case class Context(OUTPUTS: List[Box],
                     HEIGHT: Int,
                     deadline: Int,
                     minToRaise: Long,
                     pkBacker: SigmaProp,
                     pkProject: SigmaProp) {

    require(HEIGHT > 0 && deadline > 0 && minToRaise > 0 && OUTPUTS.nonEmpty)

  }

  def crowdFundingContract(ctx: Context): Boolean = {
    val fundraisingFailure = ctx.HEIGHT >= ctx.deadline && ctx.pkBacker.isValid
    val enoughRaised = { (outBox: Box) =>
      outBox.value >= ctx.minToRaise &&
        arrayEquals(outBox.propositionBytes, ctx.pkProject.propBytes)
    }
    val fundraisingSuccess = ctx.HEIGHT < ctx.deadline &&
      ctx.pkProject.isValid &&
      ctx.OUTPUTS.exists(enoughRaised)

    fundraisingFailure || fundraisingSuccess
  }

  def proveBackerGetsAfterDeadLine(ctx: Context): Boolean = {
    require(ctx.deadline <= ctx.HEIGHT &&
        ctx.pkBacker.isValid)

    crowdFundingContract(ctx)
  } holds

  def proveBackerDeniedBeforeDeadLine(ctx: Context): Boolean = {
    require(ctx.deadline > ctx.HEIGHT &&
      ctx.pkBacker.isValid &&
      !ctx.pkProject.isValid)

    crowdFundingContract(ctx)
  } ensuring(_ == false)

  def proveProjectGetsIfEnoughRaisedBeforeDeadline(ctx: Context): Boolean = {
    require(ctx.deadline > ctx.HEIGHT &&
      ctx.pkProject.isValid &&
      ctx.OUTPUTS.exists{ b =>
        b.value >= ctx.minToRaise && arrayEquals(b.propositionBytes, ctx.pkProject.propBytes)
      })

    crowdFundingContract(ctx)
  } holds

  def proveProjectDeniedIfEnoughRaisedButAfterDeadline(ctx: Context): Boolean = {
    require(ctx.deadline <= ctx.HEIGHT &&
      ctx.pkProject.isValid &&
      !ctx.pkBacker.isValid &&
      ctx.OUTPUTS.exists { b =>
        b.value >= ctx.minToRaise && arrayEquals(b.propositionBytes, ctx.pkProject.propBytes)
      })

    crowdFundingContract(ctx)
  } ensuring(_ == false)
}
