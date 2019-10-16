package sigmastate.verification.contract

import sigmastate.verification.SigmaDsl.api.sigma.{Context, SigmaContract}
import stainless.lang._

import scala.language.postfixOps

sealed abstract class DummyContract extends SigmaContract {

  def contract(ctx: Context): Boolean = {
    import ctx._
    HEIGHT > 1
  }

}

case object DummyContractVerification extends DummyContract {

  def proveTrue(ctx: Context): Boolean = {
    import ctx._
    require(HEIGHT > 1)
    contract(ctx)
  } holds

  def proveFalse(ctx: Context): Boolean = {
    import ctx._
    require(HEIGHT <= 1)
    contract(ctx)
  } ensuring(_ == false)
}
