package sigmastate.lang

import sigmastate.SType
import sigmastate.lang.syntax.ParserException
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import sigmastate.Values.{Value, SValue, SigmaTree}

class SigmaCompiler(builder: SigmaBuilder) {

  def parse(x: String): SValue = {
    SigmaParser(x, builder) match {
      case Success(v, i) => v
      case f: Parsed.Failure[_,_] =>
        throw new ParserException(s"Syntax error: $f", Some(f))
    }
  }

  def typecheck(env: Map[String, Any], code: String): Value[SType] = {
    val parsed = parse(code)
    val binder = new SigmaBinder(env, builder)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper(builder)
    val typed = typer.typecheck(bound)
    typed
  }

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    val typed = typecheck(env, code)
    val spec = new SigmaSpecializer(builder)
    val ir = spec.specialize(typed)
    ir
  }
}

object SigmaCompiler {
}
