package sigmastate.lang

import java.lang.reflect.InvocationTargetException

import org.bitbucket.inkytonik.kiama.rewriting.CallbackRewriter
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform._
import scalan.Nullable
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef.PredefinedFuncRegistry
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{BinderException, InvalidArguments}
import sigma.util.Extensions._

object SrcCtxCallbackRewriter extends CallbackRewriter {
  override def rewriting[T](oldTerm: T, newTerm: T): T = (oldTerm, newTerm) match {
    case (o: SValue, n: SValue) if o.sourceContext.isDefined && n.sourceContext.isEmpty =>
      n.withSrcCtx(o.sourceContext).asInstanceOf[T]
    case _ => newTerm
  }
}

/**
  * @param env
  * @param builder
  * @param networkPrefix network prefix to decode an ergo address from string (PK op)
  */
class SigmaBinder(env: ScriptEnv, builder: SigmaBuilder,
                  networkPrefix: NetworkPrefix,
                  predefFuncRegistry: PredefinedFuncRegistry) {
  import SigmaBinder._
  import builder._
  import SrcCtxCallbackRewriter._

  private val PKFunc = predefFuncRegistry.PKFunc(networkPrefix)

  /** Rewriting of AST with respect to environment to resolve all references to global names
    * and infer their types. */
  private def eval(e: SValue, env: ScriptEnv): SValue = rewrite(reduce(strategy[SValue]({
    case i @ Ident(n, NoType) => env.get(n) match {
      case Some(v) => Option(liftAny(v).get.withPropagatedSrcCtx(i.sourceContext))
      case None => n match {
        case "HEIGHT" => Some(Height)
        case "MinerPubkey" => Some(MinerPubkey)
        case "INPUTS" => Some(Inputs)
        case "OUTPUTS" => Some(Outputs)
        case "LastBlockUtxoRootHash" => Some(LastBlockUtxoRootHash)
        case "EmptyByteArray" => Some(ByteArrayConstant(Array.emptyByteArray))
        case "SELF" => Some(Self)
        case "CONTEXT" => Some(Context)
        case "Global" => Some(Global)
        case "None" => Some(mkNoneValue(NoType))
        case _ => None
      }
    }

    // Rule: Coll[Int](...) -->
    case _ @ Apply(ApplyTypes(Ident("Coll", _), Seq(tpe)), args) =>
      //todo: morphic: do we need the commented out code below?
//      args.foreach{ e =>
//        if (e.tpe != tpe)
//          error(s"Invalid construction of collection $e: expected type $tpe, actual type ${e.tpe}",
//            e.sourceContext)
//      }
      Some(mkConcreteCollection(args, tpe))

    // Rule: Coll(...) -->
    case Apply(Ident("Coll", _), args) =>
      val tpe = if (args.isEmpty) NoType else args(0).tpe
      Some(mkConcreteCollection(args, tpe))

    // Rule: Some(x) -->
    case Apply(i @ Ident("Some", _), args) => args match {
      case Seq(arg) => Some(mkSomeValue(arg))
      case _ => error(s"Invalid arguments of Some: expected one argument but found $args", i.sourceContext)
    }

    // Rule: min(x, y) -->
    case Apply(i @ Ident("min", _), args) => args match {
      case Seq(l: SValue, r: SValue) =>
        Some(mkMin(l.asNumValue, r.asNumValue))
      case _ =>
        throw new InvalidArguments(s"Invalid arguments for min: $args", i.sourceContext.toOption)
    }

    // Rule: max(x, y) -->
    case Apply(i @ Ident("max", _), args) => args match {
      case Seq(l: SValue, r: SValue) =>
        Some(mkMax(l.asNumValue, r.asNumValue))
      case _ =>
        throw new InvalidArguments(s"Invalid arguments for max: $args", i.sourceContext.toOption)
    }

    // Rule: lambda (...) = ... --> lambda (...): T = ...
    case lam @ Lambda(params, args, t, Some(body)) =>
      require(params.isEmpty)
      val b1 = eval(body, env)
      val newLam = mkLambda(args, t, Some(b1))
      if (newLam != lam) Some(newLam) else None

    // Rule: { e } --> e
    case Block(Seq(), body) => Some(body)

    case block @ Block(binds, t) =>
      val newBinds = for (v @ Val(n, t, b) <- binds) yield {
        if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}", v.sourceContext)
        val b1 = eval(b, env)
        builder.currentSrcCtx.withValue(v.sourceContext) {
          mkVal(n, if (t != NoType) t else b1.tpe, b1)
        }
      }
      val t1 = eval(t, env)
      val newBlock = mkBlock(newBinds, t1)
      if (newBlock != block)
        Some(newBlock)
      else
        None

    case a @ Apply(PKFunc.symNoType, args) =>
      Some(PKFunc.irBuilder(PKFunc.sym, args).withPropagatedSrcCtx(a.sourceContext))

  })))(e)

  def bind(e: SValue): SValue =
    try eval(e, env)
    catch { case e: InvocationTargetException => throw e.getCause }
}

object SigmaBinder {
  def error(msg: String, srcCtx: Nullable[SourceContext]) = throw new BinderException(msg, srcCtx.toOption)
}
