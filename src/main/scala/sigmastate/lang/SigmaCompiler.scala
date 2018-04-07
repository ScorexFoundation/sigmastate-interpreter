package sigmastate.lang

import sigmastate.SType
import sigmastate.lang.syntax.ParserException
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import sigmastate.Values.{Value, SValue, SigmaTree}

class SigmaCompiler {
  import SigmaCompiler._

  def parse(x: String): SValue = {
    SigmaParser(x) match {
      case Success(v, i) => v
      case f: Parsed.Failure[_,_] =>
        throw new ParserException("Syntax error: ", Some(f))
    }
  }

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    val parsed = parse(code)
    val binder = new SigmaBinder(env)
    val bound = binder.bind(parsed)
    val st = new SigmaTree(bound)
    val typer = new SigmaTyper
    val typed = typer.typecheck(bound)
    val spec = new SigmaSpecializer
    val ir = spec.specialize(typed)
    ir
  }
}

object SigmaCompiler {
}
