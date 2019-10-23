package sigmastate.compiler.macros.impl

import sigmastate.TrivialProp
import sigmastate.Values.{SigmaPropConstant, SigmaPropValue}
import sigmastate.eval.CSigmaProp
import special.sigma.{Context, SigmaProp}
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext, SigmaProp => VerifiedSigmaProp}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.{Context => MacrosContext}
import scala.reflect.runtime.universe._

case class CompilationResult(scalaFunc: Context => SigmaProp, prop: SigmaPropValue)

object ErgoContractCompiler {

  def compile(verifiedContract: VerifiedContext => VerifiedSigmaProp): CompilationResult = macro compileImpl

  def compileImpl(c: MacrosContext)(verifiedContract: c.Expr[VerifiedContext => VerifiedSigmaProp]): c.Expr[CompilationResult] = {
    import c.universe._

    //    verifiedContract.tree match {
//      case Function(params, tree) =>
//      case v => c.abort(c.enclosingPosition, s"expected the root to be a function, got ${verifiedContract.toString}")
//    }

    val contract = reify({c: Context => CSigmaProp(TrivialProp(true))})
    val sigmaProp = reify(SigmaPropConstant(TrivialProp(true)))
    reify(CompilationResult(contract.splice, sigmaProp.splice))
  }
}

