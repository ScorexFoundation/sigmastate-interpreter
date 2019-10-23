package sigmastate.compiler.macros.impl

import sigmastate.TrivialProp
import sigmastate.Values.{SigmaPropConstant, SigmaPropValue}
import sigmastate.eval.CSigmaProp
import special.sigma.{Context, SigmaProp}
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext, SigmaProp => VerifiedSigmaProp}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.reflect.runtime.universe._

object ErgoContractCompiler {

  def compile(verifiedContract: VerifiedContext => VerifiedSigmaProp): (Context => SigmaProp, SigmaPropValue) = macro compileImpl

  def compileImpl(c: whitebox.Context)(verifiedContract: c.Expr[VerifiedContext => VerifiedSigmaProp]): c.Expr[(Context => SigmaProp, SigmaPropValue)] =
  {
    import c.universe._

    //    verifiedContract.tree match {
//      case Function(params, tree) =>
//      case v => c.abort(c.enclosingPosition, s"expected the root to be a function, got ${verifiedContract.toString}")
//    }

    val prop = reify({c: Context => CSigmaProp(TrivialProp(true))})
    val sigmaProp = reify(SigmaPropConstant(TrivialProp(true)))
    reify(Tuple2(prop.splice, sigmaProp.splice))
  }
}

