package sigmastate.lang

import fastparse.Parsed
import fastparse.Parsed.Success
import sigma.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.Global
import sigmastate.Values.{SValue, Value}
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef.PredefinedFuncRegistry
import sigmastate.lang.Terms.MethodCall
import sigmastate.lang.syntax.ParserException
import sigmastate.utxo._
import sigmastate.{Exponentiate, MultiplyGroup, SCollection, SGlobal, SGroupElement, SType, Xor}

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

/** Result of ErgoScript source code compilation.
  * @param env compiler environment used to compile the code
  * @param code ErgoScript source code
  * @param compiledGraph graph obtained by using new [[GraphBuilding]]
  * @param buildTree ErgoTree expression obtained from graph created by [[GraphBuilding]]
  */
case class CompilerResult[Ctx <: IRContext](
  env: ScriptEnv,
  code: String,
  compiledGraph: Ctx#Ref[Ctx#Context => Any],
  /** Tree obtained from graph created by GraphBuilding */
  buildTree: SValue
)

/** Compiler which compiles ErgoScript source code into ErgoTree.
  * @param settings compilation parameters \
  */
class SigmaCompiler(settings: CompilerSettings) {
  /** Constructs an instance for the given network type and with default settings. */
  def this(networkPrefix: Byte) = this(
    CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true)
  )

  @inline final def builder = settings.builder
  @inline final def networkPrefix = settings.networkPrefix

  /** Parses the given ErgoScript source code and produces expression tree. */
  def parse(x: String): SValue = {
    SigmaParser(x) match {
      case Success(v, _) => v
      case f: Parsed.Failure =>
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

  /** Compiles the given ErgoScript source code. */
  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val typed = typecheck(env, code)
    val res = compileTyped(env, typed).copy(code = code)
    res
  }

  /** Compiles the given typed expression. */
  def compileTyped(env: ScriptEnv, typedExpr: SValue)(implicit IR: IRContext): CompilerResult[IR.type] = {
    val compiledGraph = IR.buildGraph(env, typedExpr)
    val compiledTree = IR.buildTree(compiledGraph)
    CompilerResult(env, "<no source code>", compiledGraph, compiledTree)
  }

  /** Unlowering transformation, which replaces some operations with equivalent MethodCall
    * node. This replacement is only defined for some operations.
    * This is inverse to `lowering` which is performed during compilation.
    */
  def unlowerMethodCalls(expr: SValue): SValue = {
    import SCollection._
    val r = rule[Any]({
      case MultiplyGroup(l, r) =>
        MethodCall(l, SGroupElement.MultiplyMethod, Vector(r), Map())
      case Exponentiate(l, r) =>
        MethodCall(l, SGroupElement.ExponentiateMethod, Vector(r), Map())
      case ForAll(xs, p) =>
        MethodCall(xs, ForallMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)), Vector(p), Map())
      case Exists(xs, p) =>
        MethodCall(xs, ExistsMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)), Vector(p), Map())
      case MapCollection(xs, f) =>
        MethodCall(xs,
          MapMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType, tOV -> f.tpe.tRange)),
          Vector(f), Map())
      case Fold(xs, z, op) =>
        MethodCall(xs,
          SCollection.FoldMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType, tOV -> z.tpe)),
          Vector(z, op), Map())
      case Slice(xs, from, until) =>
        MethodCall(xs,
          SCollection.SliceMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(from, until), Map())
      case Append(xs, ys) =>
        MethodCall(xs,
          SCollection.AppendMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(ys), Map())
      case Xor(l, r) =>
        MethodCall(Global, SGlobal.xorMethod, Vector(l, r), Map())
      case ByIndex(xs, index, Some(default)) =>
        MethodCall(xs,
          SCollection.GetOrElseMethod.withConcreteTypes(Map(tIV -> xs.tpe.elemType)),
          Vector(index, default), Map())
    })
    rewrite(everywherebu(r))(expr)
  }
}

object SigmaCompiler {
  def apply(settings: CompilerSettings): SigmaCompiler =
    new SigmaCompiler(settings)
}
