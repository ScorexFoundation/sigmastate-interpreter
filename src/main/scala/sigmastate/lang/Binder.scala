package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate.lang.Terms.{Ident, Let, Block}
import sigmastate.{ByteArrayConstant, Value, SType}

trait Binder {
  def bind(e: Value[SType]): Value[SType]
}

class SigmaBinder(env: Map[String, Any]) extends Binder {
  import SigmaBinder._

  private def eval(e: Value[SType], env: Map[String, Any]): Value[SType] = rewrite(strategy[Value[SType]]({
    case Ident(n, _) => env.get(n) match {
      case Some(v) => v match {
        case arr: Array[Byte] => Some(ByteArrayConstant(arr))
        case _ => error(s"Variable $n has invalid value $v")
      }
      case None =>
        error(s"Variable name $n is not defined")
    }
    case Block(Some(Let(n, b)), t) =>
      if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}")
      val b1 = eval(b, env)
      val t1 = eval(t, env + (n -> b1))
      Some(Block(Some(Let(n, b1)), t1))
    case v =>
      val v1 = rewrite(some(rule[Value[SType]] { case v => eval(v, env) }))(v)
      Some(v1)
  }))(e)

  def bind(e: Value[SType]): Value[SType] = eval(e, env)
}

class BinderException(msg: String) extends Exception(msg)

object SigmaBinder {
  def error(msg: String) = throw new BinderException(msg)
}
