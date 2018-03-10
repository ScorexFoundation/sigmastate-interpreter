package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.utxo._
import sigmastate.SCollection._

trait Binder {
  def bind(e: SValue): SValue
}

class SigmaBinder(env: Map[String, Any]) extends Binder {
  import SigmaBinder._

  val predefinedEnv: Map[String, SValue] = Seq(
    "all" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "exists" -> Lambda(
                  Vector("input" -> SCollection(NoType), "pred" -> SFunc(Vector(NoType), SBoolean)),
                  SBoolean, None),
  ).toMap

  def PredefIdent(name: String) = {
    val v = predefinedEnv(name)
    Ident(Seq(name), v.tpe)
  }

  val AllSym = PredefIdent("all")
  val ExistsSym = PredefIdent("exists")

  /** Rewriting of AST with respect to environment to resolve all references and infer types.
    * If successfull, returns type-checked Value which is ready for evaluation by the interpreter. */
  private def eval(e: SValue, env: Map[String, Any]): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(Seq(n), NoType) => env.get(n) match {
      case Some(v) => v match {
        case arr: Array[Byte] => Some(ByteArrayConstant(arr))
        case v: Int => Some(IntConstant(v))
        case v: Long => Some(IntConstant(v))
        case b: Boolean => Some(if(b) TrueLeaf else FalseLeaf)
        case v: SValue => Some(v)
        case _ => error(s"Variable $n has invalid value $v")
      }
      case None => predefinedEnv.get(n) match {
        case Some(v) => Some(Ident(Seq(n), v.tpe))
        case None => n match {
          case "HEIGHT" => Some(Height)
          case "INPUTS" => Some(Inputs)
          case "OUTPUTS" => Some(Outputs)
          case "LastBlockUtxoRootHash" => Some(LastBlockUtxoRootHash)
          case "SELF" => Some(Self)
          case _ =>
            error(s"Variable name $n is not defined")
        }
      }
    }
    case Select(obj, "size") if obj.tpe.isCollection =>
      Some(SizeOf(obj.asValue[SCollection[SType]]))
    case Apply(AllSym, Seq(ConcreteCollection(args: Seq[Value[SBoolean.type]]))) =>
      Some(AND(args))
    case Apply(ExistsSym, Seq(input: Value[SCollection[v]], pred: Value[SFunc])) =>
      Some(Exists(input, ))
    case Block(Some(Let(n, tpe, b)), t) =>
      if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}")
      val b1 = eval(b, env)
      val t1 = eval(t, env + (n -> b1))
      Some(Block(Some(Let(n, tpe, b1)), t1))
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
