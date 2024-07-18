package sigmastate.lang

import sigma.ast.{SType, SigmaBuilder, TransformingSigmaBuilder}
import sigma.ast.SigmaPredef.PredefinedFuncRegistry
import sigma.ast.syntax.SValue
import sigma.kiama.rewriting.Rewriter
import sigma.kiama.rewriting.Rewriter.{Duplicator, everywherebu, strategy}
import sigmastate.interpreter.Interpreter.ScriptEnv

/**
  * Type inference and analysis for Sigma expressions.
  */
class DirectCompiler(
  settings: CompilerSettings,
  predefFuncRegistry: PredefinedFuncRegistry
) {
  /** Constructs an instance for the given network type and with default settings. */
  def this(networkPrefix: Byte, predefFuncRegistry: PredefinedFuncRegistry) = this(
    CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true),
    predefFuncRegistry
  )

// Deep clone expr using kiama. This is to make sure JS reflection works correctly.
//  val copyRule = strategy[Any] { case x: SValue => Some(Rewriter.copy(x)) }
//  val Some(copy) = everywherebu(copyRule)(expr)

  def compileNode(env: ScriptEnv, node: SValue): SValue = node match {

    case _ =>
      val children = node.productIterator
        .map {
          case child: SValue => compileNode(env, child)
          case x => x.asInstanceOf[AnyRef] // everything else is left as is (modulo boxing)
        }
        .toArray
      Duplicator(node, children)
  }
}
