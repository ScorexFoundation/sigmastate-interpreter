package sigmastate.lang

import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate.lang.Terms._
import sigmastate._
import Values._
import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.utxo._
import sigmastate.SCollection._
import sigmastate.Values.Value.Typed

class SigmaBinder(env: Map[String, Any]) {
  import SigmaBinder._
  import SigmaPredef._

  /** Rewriting of AST with respect to environment to resolve all references and infer types.
    * If successfull, returns type-checked Value which is ready for evaluation by the interpreter. */
  private def eval(e: SValue, env: Map[String, Any]): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, NoType) => env.get(n) match {
      case Some(v) => v match {
        case arr: Array[Byte] => Some(ByteArrayConstant(arr))
        case v: Int => Some(IntConstant(v))
        case v: Long => Some(IntConstant(v))
        case v: BigInteger => Some(BigIntConstant(v))
        case v: GroupElement => Some(GroupElementConstant(v))
        case b: Boolean => Some(if(b) TrueLeaf else FalseLeaf)
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
          case "SELF" => Some(Self)
          case _ => None
        }
      }
    }
    // Rule: Array(...) -->
    case Apply(Ident("Array", _), args) =>
      val tpe = if (args.isEmpty) NoType else args(0).tpe
      Some(ConcreteCollection(args)(tpe))

    // Rule: col.size --> SizeOf(col)
    case Select(obj, "size") if obj.tpe.isCollection =>
      Some(SizeOf(obj.asValue[SCollection[SType]]))

    // Rule: col(i) --> ByIndex(col, i)
    case Apply(Typed(obj, tCol: SCollection[_]), Seq(IntConstant(i))) =>
      Some(ByIndex(obj.asValue[SCollection[SType]], i.toInt))

    // Rule: all(Array(...)) --> AND(...)
    case Apply(AllSym, Seq(ConcreteCollection(args: Seq[Value[SBoolean.type]]@unchecked))) =>
      Some(AND(args))

    // Rule: exists(input, f) -->
    case Apply(ExistsSym, Seq(input: Value[SCollection[SType]]@unchecked, pred: Value[SFunc]@unchecked)) =>
      val tItem = input.tpe.elemType
      val expectedTpe = SFunc(Vector(tItem), SBoolean)
      if (!pred.tpe.canBeTypedAs(expectedTpe))
        error(s"Invalid type of $pred. Expected $expectedTpe")
      val args = expectedTpe.tDom.zipWithIndex.map { case (t, i) => (s"arg${i+1}", t) }
      Some(ExistsSym)

    // Rule: fun (...) = ... --> fun (...): T = ...
    case lam @ Lambda(args, t, Some(body)) =>
      val b1 = eval(body, env)
      val t1 = if (t != NoType) t else b1.tpe
      val newLam = Lambda(args, t1, Some(b1))
      if (newLam != lam) Some(newLam) else None

    // Rule: { e } --> e
    case Block(Seq(), e) => Some(e)
    
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
//    case v =>
//      val v1 = rewrite(some(rule[Value[SType]] { case v => eval(v, env) }))(v)
//      Some(v1)
  })))(e)

  def bind(e: Value[SType]): Value[SType] = eval(e, env)
}

class BinderException(msg: String) extends Exception(msg)

object SigmaBinder {
  def error(msg: String) = throw new BinderException(msg)
}
