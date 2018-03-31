package sigmastate.lang

import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{strategy, rewrite, reduce}
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.{Lambda, Let, SelectGen, Apply, Select, Block, Ident, ValueOps}
import sigmastate.utxo._

class SigmaSpecializer {
  import SigmaSpecializer._

  /** Create name -> TaggedXXX(tag) pair to be used in environment. */
  def mkTagged(name: String, tpe: SType, tag: Byte): TaggedVariable[SType] = {
    val tagged = tpe match {
      case SBoolean => TaggedBoolean(tag)
      case SInt => TaggedInt(tag)
      case SBigInt => TaggedBigInt(tag)
      case SBox => TaggedBox(tag)
      case SByteArray => TaggedByteArray(tag)
      case SGroupElement => TaggedGroupElement(tag)
      case SAvlTree => TaggedAvlTree(tag)
      case _ => error(s"Don't know how to mkTagged($name, $tpe, $tag)")
    }
    tagged.asInstanceOf[TaggedVariable[SType]]
  }

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
    case SelectGen(obj, field, _) if obj.tpe == SBox => (obj.asValue[SBox.type], field) match {
      case (box, SBox.Value) => Some(ExtractAmount(box))
      case (box, SBox.PropositionBytes) => Some(ExtractScriptBytes(box))
      case (box, SBox.Id) => Some(ExtractId(box))
      case (box, SBox.Bytes) => Some(ExtractBytes(box))
      case _ => error(s"Don't know how to specialize $e")
    }
    case SelectGen(obj: SigmaBoolean, field, _) => field match {
      case SigmaBoolean.PropBytes => Some(ByteArrayConstant(obj.propBytes))
      case SigmaBoolean.IsValid => Some(obj)
    }

    case SelectGen(obj, "value", SInt) if obj.tpe == SBox =>
      Some(ExtractAmount(obj.asValue[SBox.type]))

    case Apply(SelectGen(col,"exists", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(Exists(col.asValue[SCollection[SType]], tagged.id, body1.asValue[SBoolean.type]))

    case Apply(SelectGen(col,"forall", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(ForAll(col.asValue[SCollection[SType]], tagged.id, body1.asValue[SBoolean.type]))

    case Apply(SelectGen(col,"map", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(MapCollection(col.asValue[SCollection[SType]], tagged.id, body1)(body1.tpe))

    case Apply(SelectGen(col,"fold", _), Seq(zero, Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
      val taggedZero = mkTagged(zeroArg, tZero, 21)
      val taggedOp = mkTagged(opArg, tOp, 22)
      val body1 = eval(env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp), body)
      Some(Fold(col.asValue[SCollection[SType]], taggedZero.id, zero, taggedOp.id, body1)(body1.tpe))
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
