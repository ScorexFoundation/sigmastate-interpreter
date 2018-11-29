package sigmastate.lang

import sigmastate.SType
import sigmastate.lang.syntax.ParserException
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import sigmastate.Values.{SValue, SigmaTree, Value}
import sigmastate.interpreter.Interpreter.ScriptEnv

class SigmaCompiler(builder: SigmaBuilder) {

  def parse(x: String): SValue = {
    SigmaParser(x, builder) match {
      case Success(v, i) => v
      case f: Parsed.Failure[_,_] =>
        throw new ParserException(s"Syntax error: $f", Some(f))
    }
  }

  def typecheck(env: ScriptEnv, parsed: SValue): Value[SType] = {
    val binder = new SigmaBinder(env, builder)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper(builder)
    val typed = typer.typecheck(bound)
    typed
  }

  def typecheck(env: ScriptEnv, code: String): Value[SType] = {
    val parsed = parse(code)
    typecheck(env, parsed)
  }

  def compile(env: ScriptEnv, code: String, networkPrefix: NetworkPrefix): Value[SType] = {
    val typed = typecheck(env, code)
    val spec = new SigmaSpecializer(builder, networkPrefix)
    val ir = spec.specialize(typed)
    ir
  }
}

object SigmaCompiler {
  def apply(builder: SigmaBuilder = TransformingSigmaBuilder): SigmaCompiler = new SigmaCompiler(builder)
}
