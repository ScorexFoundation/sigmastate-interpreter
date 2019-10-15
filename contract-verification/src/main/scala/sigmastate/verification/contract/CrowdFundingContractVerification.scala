package sigmastate.verification.contract

import sigmastate.verification.SigmaDsl.api.sigma.{Box, Context, SigmaContract, SigmaProp}
import sigmastate.verification.SigmaDsl.impl.sigma.PK
import stainless.annotation.ignore
import stainless.lang._

sealed abstract class CrowdFundingContract extends SigmaContract {

  def crowdFundingContract(deadline: Int, minToRaise: Long, pkBacker: SigmaProp,
                           pkProject: SigmaProp)(ctx: Context): Boolean = {
    import ctx._

    val fundraisingFailure = HEIGHT >= deadline && pkBacker.isValid
    val enoughRaised = { (outBox: Box) =>
      outBox.value >= minToRaise &&
        outBox.propositionBytes == pkProject.propBytes
    }
    val fundraisingSuccess = HEIGHT < deadline &&
      pkProject.isValid &&
      OUTPUTS.exists(enoughRaised)

    fundraisingFailure || fundraisingSuccess
  }

}

case object CrowdFundingContractVerification extends CrowdFundingContract {
  def proveBackerGetsAfterDeadLine(ctx: Context, deadline: Int, minToRaise: Long, pkBacker: SigmaProp,
                                   pkProject: SigmaProp): Boolean = {
    import ctx._
    require(deadline <= HEIGHT &&
      pkBacker.isValid)

    crowdFundingContract(deadline, minToRaise, pkBacker, pkProject)(ctx)
  } holds

  def proveBackerDeniedBeforeDeadLine(ctx: Context, deadline: Int, minToRaise: Long, pkBacker: SigmaProp,
                                      pkProject: SigmaProp): Boolean = {
    import ctx._
    require(deadline > HEIGHT &&
      pkBacker.isValid &&
      !pkProject.isValid)

    crowdFundingContract(deadline, minToRaise, pkBacker, pkProject)(ctx)
  } ensuring (_ == false)

  def proveProjectGetsIfEnoughRaisedBeforeDeadline(ctx: Context, deadline: Int, minToRaise: Long, pkBacker: SigmaProp,
                                                   pkProject: SigmaProp): Boolean = {
    import ctx._
    require(deadline > HEIGHT &&
      pkProject.isValid &&
      OUTPUTS.exists { b =>
        b.value >= minToRaise && b.propositionBytes == pkProject.propBytes
      })

    crowdFundingContract(deadline, minToRaise, pkBacker, pkProject)(ctx)
  } holds

  def proveProjectDeniedIfEnoughRaisedButAfterDeadline(ctx: Context, deadline: Int, minToRaise: Long, pkBacker: SigmaProp,
                                                       pkProject: SigmaProp): Boolean = {
    import ctx._
    require(deadline <= HEIGHT &&
      pkProject.isValid &&
      !pkBacker.isValid &&
      ctx.OUTPUTS.exists { b =>
        b.value >= minToRaise && b.propositionBytes == pkProject.propBytes
      })

    crowdFundingContract(deadline, minToRaise, pkBacker, pkProject)(ctx)
  } ensuring (_ == false)
}
