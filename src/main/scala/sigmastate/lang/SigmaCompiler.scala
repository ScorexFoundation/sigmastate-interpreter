package sigmastate.lang

import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.ErgoLikeContext
import scalan.RType
import sigmastate.{SBoolean, SType}
import sigmastate.Values.{TrueLeaf, Value, Constant, SValue}
import sigmastate.eval.{Evaluation, IRContext}
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef.PredefinedFuncRegistry
import sigmastate.lang.syntax.ParserException

/**
  * @param networkPrefix network prefix to decode an ergo address from string (PK op)
  * @param builder
  */
class SigmaCompiler(networkPrefix: NetworkPrefix, builder: SigmaBuilder) {

  def parse(x: String): SValue = {
    SigmaParser(x, builder) match {
      case Success(v, i) => v
      case f: Parsed.Failure[_,String] =>
        throw new ParserException(s"Syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  def typecheck(env: ScriptEnv, parsed: SValue): Value[SType] = {
    val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
    val binder = new SigmaBinder(env, builder, networkPrefix, predefinedFuncRegistry)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper(builder, predefinedFuncRegistry)
    val typed = typer.typecheck(bound)
    typed
  }

  def typecheck(env: ScriptEnv, code: String): Value[SType] = {
    val parsed = parse(code)
    typecheck(env, parsed)
  }

  def compile(env: ScriptEnv, code: String): Value[SType] = {
    val typed = typecheck(env, code)
    val spec = new SigmaSpecializer(builder)
    val ir = spec.specialize(typed)
    ir
  }
}

object SigmaCompiler {
  def apply(networkPrefix: NetworkPrefix, builder: SigmaBuilder = TransformingSigmaBuilder): SigmaCompiler =
    new SigmaCompiler(networkPrefix, builder)
}
