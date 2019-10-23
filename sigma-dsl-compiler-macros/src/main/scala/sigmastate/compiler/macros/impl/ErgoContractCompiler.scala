package sigmastate.compiler.macros.impl

import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition}
import org.ergoplatform.dsl.PropositionSpec
import sigmastate.TrivialProp
import sigmastate.Values.{ErgoTree, SigmaPropConstant, SigmaPropValue, TrueLeaf}
import sigmastate.eval.CSigmaProp
import special.sigma.{Context, SigmaProp}
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext, SigmaProp => VerifiedSigmaProp}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.reflect.runtime.universe._

object ErgoContractCompilerWrapper {

  def compile(verifiedContract: VerifiedContext => VerifiedSigmaProp): (Context => SigmaProp, SigmaPropValue) = macro ErgoContractCompiler.compile
}

class ErgoContractCompiler(val c: whitebox.Context) extends Liftables {

  import c.universe._

  def compile(verifiedContract: Expr[VerifiedContext => VerifiedSigmaProp]): Expr[(Context => SigmaProp, SigmaPropValue)] =
  {

    //    contract.tree match {
    //      case Function(params, tree) =>
    //      case v => abort(s"expected the root to be a function, got ${v}")
    //    }

    val prop: Context => SigmaProp = { _ => CSigmaProp(TrivialProp(true)) }
    val sigmaProp = SigmaPropConstant(TrivialProp(true))
    q"($prop, $sigmaProp)"
  }
}

case class PlainPropositionSpec(name: String,
                                dslSpec: Context => Boolean,
                                ergoTree: ErgoTree,
                                scriptSpec: ErgoScript)
  //extends PropositionSpec

