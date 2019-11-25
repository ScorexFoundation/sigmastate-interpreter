package sigmastate.verification.contract

import sigmastate.compiler.macros.impl.{ErgoContract, ErgoContractCompiler}
import sigmastate.verified._
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
    HEIGHT >= 0 && pk
  }

  def contract5(ctx: Context, pk1: SigmaProp, pk2: SigmaProp): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0)
    pk1 && pk2
  }

  def contract6(ctx: Context, arr: Coll[Coll[Byte]]): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0 && arr.nonEmpty)
    sigmaProp(arr(0).length > 0)
  }

  def contract7(ctx: Context, pkAmounts: Coll[(Coll[Byte], Long)]): SigmaProp = {
    import ctx._
    require(HEIGHT >= 0 && pkAmounts.nonEmpty)
    sigmaProp(pkAmounts(0)._1.length > 0)
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

  def proveContract4(ctx: Context, pk: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT >= 0 && pk.isValid)
    contract4(ctx, pk).isValid
  } holds

  def proveContract5(ctx: Context, pk1: SigmaProp, pk2: SigmaProp): Boolean = {
    import ctx._
    require(HEIGHT >= 0 && pk1.isValid && pk2.isValid)
    contract5(ctx, pk1, pk2).isValid
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

  def contract5Instance(pk1: SigmaProp, pk2: SigmaProp): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract5(context, pk1, pk2)
    }

  def contract6Instance(arr: Coll[Coll[Byte]]): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract6(context, arr)
    }

  def contract7Instance(arr: Coll[(Coll[Byte], Long)]): ErgoContract =
    ErgoContractCompiler.compile { context: Context =>
      DummyContractVerification.contract7(context, arr)
    }
}

