package sigmastate.lang

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{reduce, strategy, rewrite}
import sigmastate.Values.Value.Typed
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.{Lambda, Let, Apply, Select, Block, Ident}
import sigmastate.utxo._

class SigmaSpecializer {
  import SigmaSpecializer._

  /** Rewriting of AST with respect to environment to resolve all references
    * to let bound and lambda bound names. */
  private def eval(env: Map[String, SValue], e: SValue): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, _) => env.get(n)

    case block @ Block(binds, res) =>
      var curEnv = env
      for (Let(n, t, b) <- binds) {
        if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
        val b1 = eval(curEnv, b)
        curEnv = curEnv + (n -> b1)
      }
      val res1 = eval(curEnv, res)
      Some(res1)
    case Apply(Blake2b256Sym, Seq(arg: Value[SByteArray.type]@unchecked)) =>
      Some(CalcBlake2b256(arg))
    case Apply(TaggedByteArraySym, Seq(IntConstant(i))) =>
      Some(TaggedByteArray(i.toByte))
  })))(e)

  def specialize(typed: SValue): SValue = {
    specialize(Map(), typed)
  }

  def specialize(env: Map[String, SValue], typed: SValue): SValue = {
    val res = eval(env, typed)
    res
  }

}

class SpecializerException(msg: String) extends Exception(msg)

object SigmaSpecializer {
  def error(msg: String) = throw new SpecializerException(msg)
}
