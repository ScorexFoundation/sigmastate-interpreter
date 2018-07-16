package sigmastate.lang

import sigmastate.SType
import sigmastate.lang.syntax.ParserException
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import sigmastate.Values.{Value, SValue, SigmaTree}

class SigmaCompiler {
  def parse(x: String, builder: SigmaBuilder): SValue = {
    SigmaParser(x, builder) match {
      case Success(v, i) => v
      case f: Parsed.Failure[_,_] =>
        throw new ParserException(s"Syntax error: $f", Some(f))
    }
  }

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    val builder = TransformingSigmaBuilder
    val parsed = parse(code, builder)
    val binder = new SigmaBinder(env, builder)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper(builder)
    val typed = typer.typecheck(bound)
    val spec = new SigmaSpecializer(builder)
    val ir = spec.specialize(typed)
    ir
  }
}

object SigmaCompiler {
}
