package sigmastate.verification.contract

import sigmastate.SigmaDslCompiler
import sigmastate.verification.SigmaDsl.api.sigma.{Box, Context, SigmaContract, SigmaDslBuilder, SigmaProp}
import stainless.annotation.extern
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

object CrowdFundingContractTree extends CrowdFundingContract {

  def tree: SigmaProp = {
    val pkBacker = SigmaDslCompiler.PK("9h7DHKSDgE4uvP8313GVGdsEg3AvdAWSSTG7XZsLwBfeth4aePG")
    val pkProject = SigmaDslCompiler.PK("9gBSqNT9LH9WjvWbyqEvFirMbYp4nfGHnoWdceKGu45AKiya3Fq")
    SigmaDslCompiler.compile(crowdFundingContract(5000, 20000, pkBacker, pkProject))
  }
}