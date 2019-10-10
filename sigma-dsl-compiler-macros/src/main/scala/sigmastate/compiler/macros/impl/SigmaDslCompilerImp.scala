package sigmastate.compiler.macros.impl

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object SigmaDslCompilerImp {

  private def error(string: String): Unit =
    throw new RuntimeException(string)

  def compile(c: whitebox.Context)
             (contract: c.Expr[Any => Boolean]): c.Tree = {
    import c.universe._

    contract.tree match {
      case Function(params, tree) =>
      case v => error(s"expected the root to be a function, got ${v}")
    }

    // return a "no op"
    EmptyTree
  }
}
