package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{strategy, rewrite, reduce}
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.Values.Value.Typed
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.{Lambda, Let, Apply, ValueOps, Select, Block, Ident}
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

    case _ @ Block(binds, res) =>
      var curEnv = env
      for (Let(n, _, b) <- binds) {
        if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
        val b1 = eval(curEnv, b)
        curEnv = curEnv + (n -> b1)
      }
      val res1 = eval(curEnv, res)
      Some(res1)

    case Apply(Blake2b256Sym, Seq(arg: Value[SByteArray.type]@unchecked)) =>
      Some(CalcBlake2b256(arg))

    case Apply(Sha256Sym, Seq(arg: Value[SByteArray.type]@unchecked)) =>
      Some(CalcSha256(arg))

    case Apply(TaggedAvlTreeSym, Seq(IntConstant(i))) =>
      Some(TaggedAvlTree(i.toByte))

    case Apply(TaggedGroupElementSym, Seq(IntConstant(i))) =>
      Some(TaggedGroupElement(i.toByte))

    case Apply(TaggedBoxSym, Seq(IntConstant(i))) =>
      Some(TaggedBox(i.toByte))

    case Apply(TaggedByteArraySym, Seq(IntConstant(i))) =>
      Some(TaggedByteArray(i.toByte))

    case Apply(TaggedBigIntSym, Seq(IntConstant(i))) =>
      Some(TaggedBigInt(i.toByte))

    case Apply(TaggedIntSym, Seq(IntConstant(i))) =>
      Some(TaggedInt(i.toByte))

    case Apply(TaggedBooleanSym, Seq(IntConstant(i))) =>
      Some(TaggedBoolean(i.toByte))

    case Apply(IsMemberSym, Seq(tree: Value[SAvlTree.type]@unchecked, key: Value[SByteArray.type]@unchecked, proof: Value[SByteArray.type]@unchecked)) =>
      Some(IsMember(tree, key, proof))

    case Apply(ProveDlogSym, Seq(g: Value[SGroupElement.type]@unchecked)) =>
      Some(ProveDlog(g))

    // Rule: col.size --> SizeOf(col)
    case Select(obj, "size", _) =>
      if (obj.tpe.isCollection)
        Some(SizeOf(obj.asValue[SCollection[SType]]))
      else
        error(s"The type of $obj is expected to be Collection to select 'size' property")

    case sel @ Apply(Select(Select(Typed(box, SBox), regName, _), "valueOrElse", Some(_)), Seq(arg)) =>
      val reg = ErgoBox.registerByName.getOrElse(regName,
        error(s"Invalid register name $regName in expression $sel"))
      Some(ExtractRegisterAs(box.asBox, reg, Some(arg))(arg.tpe))

    case sel @ Select(Select(Typed(box, SBox), regName, _), "value", Some(regType)) =>
      val reg = ErgoBox.registerByName.getOrElse(regName,
        error(s"Invalid register name $regName in expression $sel"))
      Some(ExtractRegisterAs(box.asBox, reg)(regType))

    case sel @ Select(obj, field, _) if obj.tpe == SBox =>
      (obj.asValue[SBox.type], field) match {
        case (box, SBox.Value) => Some(ExtractAmount(box))
        case (box, SBox.PropositionBytes) => Some(ExtractScriptBytes(box))
        case (box, SBox.Id) => Some(ExtractId(box))
        case (box, SBox.Bytes) => Some(ExtractBytes(box))
        case (box, SBox.BytesWithNoRef) => Some(ExtractBytesWithNoRef(box))
        case (box, _) if box.tpe.hasField(field) =>
          None  // leave it as it is and handle on a level of parent node
        case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
      }

    case Select(obj: SigmaBoolean, field, _) =>
      field match {
        case SigmaBoolean.PropBytes => Some(ByteArrayConstant(obj.propBytes))
        case SigmaBoolean.IsValid => Some(obj)
      }

    case Select(obj, "value", Some(SInt)) if obj.tpe == SBox =>
      Some(ExtractAmount(obj.asValue[SBox.type]))

    case Apply(Select(col,"exists", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(Exists(col.asValue[SCollection[SType]], tagged.id, body1.asValue[SBoolean.type]))

    case Apply(Select(col,"forall", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(ForAll(col.asValue[SCollection[SType]], tagged.id, body1.asValue[SBoolean.type]))

    case Apply(Select(col,"map", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(MapCollection(col.asValue[SCollection[SType]], tagged.id, body1)(body1.tpe))

    case Apply(Select(col,"fold", _), Seq(zero, Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
      val taggedZero = mkTagged(zeroArg, tZero, 21)
      val taggedOp = mkTagged(opArg, tOp, 22)
      val body1 = eval(env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp), body)
      Some(Fold(col.asValue[SCollection[SType]], taggedZero.id, zero, taggedOp.id, body1)(body1.tpe))

    case opt: OptionValue[_] =>
      error(s"Option values are not supported: $opt")
  })))(e)

  def specialize(typed: SValue): SValue = {
    specialize(Map(), typed)
  }

  def specialize(env: Map[String, SValue], typed: SValue): SValue = {
    val res = eval(env, typed)
    res
  }
}

object SigmaSpecializer {
  def error(msg: String) = throw new SpecializerException(msg, None)
}
