package sigmastate.compiler.macros.impl

import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition}
import org.ergoplatform.dsl.PropositionSpec
import sigmastate.TrivialProp
import sigmastate.Values.{ErgoTree, TrueLeaf}
import sigmastate.eval.CSigmaProp
import special.sigma.Context

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.reflect.runtime.universe._


class SigmaDslCompilerImp(val c: whitebox.Context) extends Liftables {

  import c.universe._

  def compile(contract: Expr[sigmastate.verification.SigmaDsl.api.sigma.Context => Boolean]): Tree = {

    //    contract.tree match {
    //      case Function(params, tree) =>
    //      case v => abort(s"expected the root to be a function, got ${v}")
    //    }

    val prop: Context => Boolean = { _ => true }
    val sigmaProp = CSigmaProp(TrivialProp(true))
    q"($prop, $sigmaProp)"
  }
}

case class PlainPropositionSpec(name: String,
                                dslSpec: Context => Boolean,
                                ergoTree: ErgoTree,
                                scriptSpec: ErgoScript)
  //extends PropositionSpec

