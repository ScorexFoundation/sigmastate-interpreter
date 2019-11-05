package sigmastate.verification.contract

import sigmastate.compiler.macros.impl.{ErgoContract, ErgoContractCompiler}
import sigmastate.verification.SigmaDsl.api.sigma._
import stainless.annotation.ignore
import stainless.lang._

import scala.language.postfixOps

sealed abstract class DummyContract extends SigmaContract {

  def contract(ctx: Context, limit: Int): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0 && limit >= 0)
    // todo implicit boolops?
    sigmaProp(HEIGHT < limit)
  }
}

case object DummyContractVerification extends DummyContract {

  def proveTrue(ctx: Context, limit: Int): Boolean = {
    import ctx._
    require(HEIGHT < limit && HEIGHT >= 0 && limit >= 0)
    contract(ctx, limit).isValid
  } holds

  def proveFalse(ctx: Context, limit: Int): Boolean = {
    import ctx._
    require(HEIGHT > limit && HEIGHT >= 0 && limit >= 0)
    !contract(ctx, limit).isValid
  } holds
}

@ignore
case object DummyContractCompilation extends DummyContract {

  def contractInstance(lmt: Int): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract(context, lmt)
    }
}
