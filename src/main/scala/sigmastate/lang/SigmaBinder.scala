package sigmastate.lang

import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate.lang.Terms._
import sigmastate._
import Values._
import org.ergoplatform._
import sigmastate.utils.Extensions._
import sigmastate.Values.Value.Typed
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.exceptions.BinderException

class SigmaBinder(env: Map[String, Any], builder: SigmaBuilder) {
  import SigmaBinder._
  import SigmaPredef._
  import builder._

  /** Rewriting of AST with respect to environment to resolve all references to global names
    * and infer their types. */
  private def eval(e: SValue, env: Map[String, Any]): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, NoType) => env.get(n) match {
      case Some(v) => v match {
        case arr: Array[Boolean] => Some(BoolArrayConstant(arr))
        case arr: Array[Byte] => Some(ByteArrayConstant(arr))
        case arr: Array[Short] => Some(ShortArrayConstant(arr))
        case arr: Array[Int] => Some(IntArrayConstant(arr))
        case arr: Array[Long] => Some(LongArrayConstant(arr))
        case v: Byte => Some(ByteConstant(v))
        case v: Short => Some(ShortConstant(v))
        case v: Int => Some(IntConstant(v))
        case v: Long => Some(LongConstant(v))
        case v: BigInteger => Some(BigIntConstant(v))
        case v: CryptoConstants.EcPointType => Some(GroupElementConstant(v))
        case b: Boolean => Some(if(b) TrueLeaf else FalseLeaf)
        case b: ErgoBox => Some(BoxConstant(b))
        case avl: AvlTreeData => Some(AvlTreeConstant(avl))
        case v: SValue => Some(v)
        case _ => None
      }
      case None => predefinedEnv.get(n) match {
        case Some(v) => Some(Ident(n, v.tpe))
        case None => n match {
          case "HEIGHT" => Some(Height)
          case "INPUTS" => Some(Inputs)
          case "OUTPUTS" => Some(Outputs)
          case "LastBlockUtxoRootHash" => Some(LastBlockUtxoRootHash)
          case "EmptyByteArray" => Some(ByteArrayConstant(Array.emptyByteArray))
          case "SELF" => Some(Self)
          case "None" => Some(mkNoneValue(NoType))
          case _ => None
        }
      }
    }

    // Rule: Array[Int](...) -->
    case e @ Apply(ApplyTypes(Ident("Array", _), Seq(tpe)), args) =>
      val resTpe = if (args.isEmpty) tpe
      else {
        val elemType = args(0).tpe
        if (elemType != tpe)
          error(s"Invalid construction of array $e: expected type $tpe, actual type $elemType")
        elemType
      }
      Some(mkConcreteCollection(args, resTpe))

    // Rule: Array(...) -->
    case Apply(Ident("Array", _), args) =>
      val tpe = if (args.isEmpty) NoType else args(0).tpe
      Some(mkConcreteCollection(args, tpe))

    // Rule: Some(x) -->
    case Apply(Ident("Some", _), args) =>
      val arg =
        if (args.length == 1) args(0)
        else error(s"Invalid arguments of Some: expected one argument but found $args")
      Some(mkSomeValue(arg))

    case e @ Apply(ApplyTypes(f @ GetVarSym, targs), args) =>
      if (targs.length != 1 || args.length != 1)
        error(s"Wrong number of arguments in $e: expected one type argument and one variable id")
      val id = args.head match {
        case LongConstant(i) => i.toByteExact  //TODO use SByte.downcast once it is implemented
        case IntConstant(i) => i.toByteExact
        case ByteConstant(i) => i
      }
      Some(mkTaggedVariable(id, targs.head))

    // Rule: fun (...) = ... --> fun (...): T = ...
    case lam @ Lambda(args, t, Some(body)) =>
      val b1 = eval(body, env)
      val newLam = Lambda(args, t, Some(b1))
      if (newLam != lam) Some(newLam) else None

    // Rule: { e } --> e
    case Block(Seq(), body) => Some(body)
    
    case block @ Block(binds, t) =>
      val newBinds = for (Let(n, t, b) <- binds) yield {
        if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}")
        val b1 = eval(b, env)
        Let(n, if (t != NoType) t else b1.tpe, b1)
      }
      val t1 = eval(t, env)
      val newBlock = mkBlock(newBinds, t1)
      if (newBlock != block)
        Some(newBlock)
      else
        None
  })))(e)

  def bind(e: SValue): SValue = eval(e, env)
}

object SigmaBinder {
  def error(msg: String) = throw new BinderException(msg, None)
}
