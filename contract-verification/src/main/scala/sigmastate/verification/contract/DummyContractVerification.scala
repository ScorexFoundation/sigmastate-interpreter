package sigmastate.verification.contract

import sigmastate.compiler.macros.impl.{ErgoContract, ErgoContractCompiler}
import sigmastate.verification.SigmaDsl.api.collection._
import sigmastate.verification.SigmaDsl.api.sigma._
import stainless.annotation.ignore
import stainless.lang._

import scala.language.{implicitConversions, postfixOps}

sealed abstract class DummyContract extends SigmaContract {

  def contract(ctx: Context, limit: Int): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0 && limit >= 0)
    // todo implicit boolops?
    sigmaProp(HEIGHT < limit)
  }

  def contract2(ctx: Context, start: Int, end: Long): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0 && start >= 0 && end >= 0)
    sigmaProp(HEIGHT >= start && HEIGHT <= end)
  }

  def contract3(ctx: Context, arr: Coll[Byte], arr2: Coll[Long]): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0)
    sigmaProp(arr.length > 0 && arr2.length > 0)
  }

  def contract4(ctx: Context, pk: SigmaProp): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0)
    pk
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

  def contract2Instance(s: Int, e: Long): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract2(context, s, e)
    }

  def contract3Instance(arr: Coll[Byte], arr2: Coll[Long]): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract3(context, arr, arr2)
    }

  def contract4Instance(pk: SigmaProp): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract4(context, pk)
    }
}
