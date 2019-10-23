package sigmastate.verification.contract

import sigmastate.verification.SigmaDsl.api.sigma._
import stainless.lang._

import scala.language.postfixOps

sealed abstract class DummyContract extends SigmaContract {

  def contract(ctx: Context): SigmaProp = {
    import ctx._
    // todo implicit boolops?
    SigmaPropProof(TrivialProp(HEIGHT > 1))
  }

}

case object DummyContractVerification extends DummyContract {

  def proveTrue(ctx: Context): Boolean = {
    import ctx._
    require(HEIGHT > 1)
    contract(ctx).isValid
  } holds

  def proveFalse(ctx: Context): Boolean = {
    import ctx._
    require(HEIGHT <= 1)
    contract(ctx).isValid
  } ensuring(_ == false)
}
