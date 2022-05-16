package sigmastate.lang

import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import sigmastate.Values.{SValue, Value}
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef.PredefinedFuncRegistry
import sigmastate.lang.Terms.MethodCall
import sigmastate.lang.syntax.ParserException
import sigmastate.utxo._
import sigmastate.{Exponentiate, SCollection, SGroupElement, SType, STypeVar}

/**
  * @param networkPrefix    network prefix to decode an ergo address from string (PK op)
  * @param builder          used to create ErgoTree nodes
  * @param lowerMethodCalls if true, then MethodCall nodes are lowered to ErgoTree nodes
  *                         when [[sigmastate.SMethod.irInfo.irBuilder]] is defined. For
  *                         example, in the `coll.map(x => x+1)` code, the `map` method
  *                         call can be lowered to MapCollection node.
  *                         The lowering if preferable, because it is more compact (1 byte
  *                         for MapCollection instead of 3 bytes for MethodCall).
  */
case class CompilerSettings(
    networkPrefix: NetworkPrefix,
    builder: SigmaBuilder,
    lowerMethodCalls: Boolean
)

case class CompilerResult[Ctx <: IRContext](
  env: ScriptEnv,
  code: String,
  compiledGraph: Ctx#Ref[Ctx#Context => Any],
  /** Tree obtained from graph created by GraphBuilding */
  buildTree: SValue
)

class SigmaCompiler(settings: CompilerSettings) {
  def this(networkPrefix: Byte) = this(
    CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true)
  )

  @inline final def builder = settings.builder
  @inline final def networkPrefix = settings.networkPrefix

  /** Parses the given ErgoScript source code and produces expression tree. */
  def parse(x: String): SValue = {
    SigmaParser(x, builder) match {
      case Success(v, _) => v
      case f: Parsed.Failure[_,String] =>
        throw new ParserException(s"Syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  /** Typechecks the given parsed expression and assigns types for all sub-expressions. */
  def typecheck(env: ScriptEnv, parsed: SValue): Value[SType] = {
    val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
    val binder = new SigmaBinder(env, builder, networkPrefix, predefinedFuncRegistry)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper(builder, predefinedFuncRegistry, settings.lowerMethodCalls)
    val typed = typer.typecheck(bound)
    typed
  }

  def typecheck(env: ScriptEnv, code: String): Value[SType] = {
    val parsed = parse(code)
    typecheck(env, parsed)
  }

  private[sigmastate] def compileWithoutCosting(env: ScriptEnv, code: String): Value[SType] = {
    val typed = typecheck(env, code)
    val spec = new SigmaSpecializer(builder)
    val ir = spec.specialize(typed)
    ir
  }

  /** Compiles the given ErgoScript source code. */
  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val typed = typecheck(env, code)
    val res = compileTyped(env, typed)
    res.copy(code = code)
  }

  /** Compiles the given typed expression. */
  def compileTyped(env: ScriptEnv, typedExpr: SValue)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val compiledGraph = IR.buildGraph(env, typedExpr)
    val compiledTree = IR.buildTree(compiledGraph)
    CompilerResult(env, "<no source code>", compiledGraph, compiledTree)
  }

  def unlowerMethodCalls(expr: SValue): SValue = {
    import SCollection._
    val r = rule[Any]({
      case Exponentiate(l, r) =>
        MethodCall(l, SGroupElement.ExponentiateMethod, Vector(r), Map())
      case ForAll(xs, p) =>
        MethodCall(xs, ForallMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)), Vector(p), Map())
      case Exists(xs, p) =>
        MethodCall(xs, ExistsMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)), Vector(p), Map())
      case Fold(xs, z, op) =>
        MethodCall(xs,
          SCollection.FoldMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType, tOV -> z.tpe)),
          Vector(z, op), Map())
      case Slice(xs, from, until) =>
        MethodCall(xs,
          SCollection.SliceMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(from, until), Map())
    })
    rewrite(everywherebu(r))(expr)
  }
}

object SigmaCompiler {
  def apply(settings: CompilerSettings): SigmaCompiler =
    new SigmaCompiler(settings)
}
