package sigmastate.lang

import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate.lang.Terms._
import sigmastate._
import Values._
import org.ergoplatform._
import sigmastate.utxo._
import sigmastate.Values.Value.Typed
import sigmastate.interpreter.CryptoConstants

class SigmaBinder(env: Map[String, Any]) {
  import SigmaBinder._
  import SigmaPredef._

  /** Rewriting of AST with respect to environment to resolve all references to global names
    * and infer their types. */
  private def eval(e: SValue, env: Map[String, Any]): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, NoType) => env.get(n) match {
      case Some(v) => v match {
        case arr: Array[Byte] => Some(ByteArrayConstant(arr))
        case arr: Array[Long] => Some(LongArrayConstant(arr))
        case arr: Array[Boolean] => Some(BoolArrayConstant(arr))
        case v: Byte => Some(ByteConstant(v))
        case v: Int => Some(LongConstant(v))
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
          case "None" => Some(NoneValue(NoType))
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
      Some(ConcreteCollection(args)(resTpe))

    // Rule: Array(...) -->
    case Apply(Ident("Array", _), args) =>
      val tpe = if (args.isEmpty) NoType else args(0).tpe
      Some(ConcreteCollection(args)(tpe))

    // Rule: Some(x) -->
    case Apply(Ident("Some", _), args) =>
      val arg =
        if (args.length == 1) args(0)
        else error(s"Invalid arguments of Some: expected one argument but found $args")
      Some(SomeValue(arg))

    // Rule: col(i) --> ByIndex(col, i)
    case Apply(Typed(obj, tCol: SCollection[_]), Seq(LongConstant(i))) =>
      Some(ByIndex(obj.asValue[SCollection[SType]], i.toInt))

    // Rule: allOf(Array(...)) --> AND(...)
    case Apply(AllSym, Seq(ConcreteCollection(args: Seq[Value[SBoolean.type]]@unchecked, _))) =>
      Some(AND(args))

    // Rule: anyOf(Array(...)) --> OR(...)
    case Apply(AnySym, Seq(ConcreteCollection(args: Seq[Value[SBoolean.type]]@unchecked, _))) =>
      Some(OR(args))

    // Rule: allOf(arr) --> AND(arr)
    case Apply(AllSym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
      Some(AND(arr))

    // Rule: anyOf(arr) --> OR(arr)
    case Apply(AnySym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
      Some(OR(arr))

    case e @ Apply(ApplyTypes(f @ GetVarSym, targs), args) =>
      if (targs.length != 1 || args.length != 1)
        error(s"Wrong number of arguments in $e: expected one type argument and one variable id")
      val id = args.head match {
        case LongConstant(i) => i.toByte
        case ByteConstant(i) => i
      }
      Some(TaggedVariable(id, targs.head))

    // Rule: fun (...) = ... --> fun (...): T = ...
    case lam @ Lambda(args, t, Some(body)) =>
      val b1 = eval(body, env)
      val t1 = if (t != NoType) t else b1.tpe
      val newLam = Lambda(args, t1, Some(b1))
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
      val newBlock = Block(newBinds, t1)
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
