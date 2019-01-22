package sigmastate.lang

import java.lang.reflect.InvocationTargetException

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform._
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef.PredefinedFuncRegistry
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{BinderException, InvalidArguments}

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

  private val PKFunc = predefFuncRegistry.PKFunc(networkPrefix)

  /** Rewriting of AST with respect to environment to resolve all references to global names
    * and infer their types. */
  private def eval(e: SValue, env: ScriptEnv): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, NoType) => env.get(n) match {
      case Some(v) => Option(liftAny(v).get)
      case None => n match {
        case "HEIGHT" => Some(Height)
        case "MinerPubkey" => Some(MinerPubkey)
        case "INPUTS" => Some(Inputs)
        case "OUTPUTS" => Some(Outputs)
        case "LastBlockUtxoRootHash" => Some(LastBlockUtxoRootHash)
        case "EmptyByteArray" => Some(ByteArrayConstant(Array.emptyByteArray))
        case "SELF" => Some(Self)
        case "None" => Some(mkNoneValue(NoType))
        case _ => None
      }
    }

    // Rule: Array[Int](...) -->
    case e @ Apply(ApplyTypes(Ident("Coll", _), Seq(tpe)), args) =>
      val resTpe = if (args.isEmpty) tpe
      else {
        val elemType = args(0).tpe
        if (elemType != tpe)
          error(s"Invalid construction of array $e: expected type $tpe, actual type $elemType")
        elemType
      }
      Some(mkConcreteCollection(args, resTpe))

    // Rule: Array(...) -->
    case Apply(Ident("Coll", _), args) =>
      val tpe = if (args.isEmpty) NoType else args(0).tpe
      Some(mkConcreteCollection(args, tpe))

    // Rule: Some(x) -->
    case Apply(Ident("Some", _), args) =>
      val arg =
        if (args.length == 1) args(0)
        else error(s"Invalid arguments of Some: expected one argument but found $args")
      Some(mkSomeValue(arg))

    // Rule: min(x, y) -->
    case Apply(Ident("min", _), args) => args match {
      case Seq(l: SValue, r: SValue) =>
        Some(mkMin(l.asNumValue, r.asNumValue))
      case _ =>
        throw new InvalidArguments(s"Invalid arguments for min: $args")
    }

    // Rule: max(x, y) -->
    case Apply(Ident("max", _), args) => args match {
      case Seq(l: SValue, r: SValue) =>
        Some(mkMax(l.asNumValue, r.asNumValue))
      case _ =>
        throw new InvalidArguments(s"Invalid arguments for max: $args")
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
      val newBinds = for (Val(n, t, b) <- binds) yield {
        if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}")
        val b1 = eval(b, env)
        mkVal(n, if (t != NoType) t else b1.tpe, b1)
      }
      val t1 = eval(t, env)
      val newBlock = mkBlock(newBinds, t1)
      if (newBlock != block)
        Some(newBlock)
      else
        None

    case Apply(PKFunc.symNoType, args) =>
      Some(PKFunc.irBuilder(PKFunc.sym, args))

  })))(e)

  def bind(e: SValue): SValue =
    try eval(e, env)
    catch { case e: InvocationTargetException => throw e.getCause }
}

object SigmaBinder {
  def error(msg: String) = throw new BinderException(msg, None)
}
