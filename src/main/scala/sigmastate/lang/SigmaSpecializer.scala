package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{reduce, rewrite, strategy}
import org.ergoplatform.ErgoBox
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.SCollection.SByteArray
import sigmastate.Values.Value.Typed
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.{Apply, Block, Ident, Lambda, Let, Select, ValueOps}
import sigmastate.lang.exceptions.SpecializerException
import sigmastate.utxo._

class SigmaSpecializer {
  import SigmaSpecializer._

  /** Create name -> TaggedXXX(tag) pair to be used in environment. */
  def mkTagged(name: String, tpe: SType, tag: Byte): TaggedVariable[SType] = {
    val tagged = TaggedVariable(tag, tpe)
    tagged
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

    // Rule: allOf(arr) --> AND(arr)
    case Apply(AllSym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
      Some(AND(arr))

    // Rule: anyOf(arr) --> OR(arr)
    case Apply(AnySym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
      Some(OR(arr))

    case Apply(Blake2b256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
      Some(CalcBlake2b256(arg))

    case Apply(Sha256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
      Some(CalcSha256(arg))

    case Apply(IsMemberSym, Seq(tree: Value[SAvlTree.type]@unchecked, key: Value[SByteArray]@unchecked, proof: Value[SByteArray]@unchecked)) =>
      Some(IsMember(tree, key, proof))

    case Apply(ProveDlogSym, Seq(g: Value[SGroupElement.type]@unchecked)) =>
      Some(ProveDlog(g))

    case Apply(IntToByteSym, Seq(arg: Value[SInt.type]@unchecked)) =>
      Some(IntToByte(arg))

    case Apply(LongToByteArraySym, Seq(arg: Value[SLong.type]@unchecked)) =>
      Some(LongToByteArray(arg))

    case Upcast(Constant(value, tpe), toTpe: SNumericType) =>
      Some(Constant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

    // Rule: col.size --> SizeOf(col)
    case Select(obj, "size", _) =>
      if (obj.tpe.isCollectionLike)
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
        case (box, _) if box.tpe.hasMethod(field) =>
          None  // leave it as it is and handle on a level of parent node
        case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
      }

    case Select(obj: SigmaBoolean, field, _) =>
      field match {
        case SigmaBoolean.PropBytes => Some(ByteArrayConstant(obj.bytes))
        case SigmaBoolean.IsValid => Some(obj)
      }

    case Select(obj, "value", Some(SLong)) if obj.tpe == SBox =>
      Some(ExtractAmount(obj.asValue[SBox.type]))

    case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
      val index = fn.substring(1).toByte
      Some(SelectField(tuple.asTuple, index))

    case Apply(Select(col, "slice", _), Seq(from, until)) =>
      Some(Slice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

    case Apply(Select(col, "where", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(Where(col.asValue[SCollection[SType]], tagged.varId, body1.asValue[SBoolean.type]))

    case Apply(Select(col,"exists", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(Exists(col.asValue[SCollection[SType]], tagged.varId, body1.asValue[SBoolean.type]))

    case Apply(Select(col,"forall", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(ForAll(col.asValue[SCollection[SType]], tagged.varId, body1.asValue[SBoolean.type]))

    case Apply(Select(col,"map", _), Seq(Lambda(Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(MapCollection(col.asValue[SCollection[SType]], tagged.varId, body1))

    case Apply(Select(col,"fold", _), Seq(zero, Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
      val taggedZero = mkTagged(zeroArg, tZero, 21)
      val taggedOp = mkTagged(opArg, tOp, 22)
      val body1 = eval(env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp), body)
      Some(Fold(col.asValue[SCollection[SType]], taggedZero.varId, zero, taggedOp.varId, body1))

    case Apply(Select(col,"getOrElse", _), Seq(index, defaultValue)) =>
      val index1 = eval(env, index).asValue[SInt.type]
      val defaultValue1 = eval(env, defaultValue).asValue[SType]
      Some(ByIndex(col.asValue[SCollection[SType]], index1, Some(defaultValue1)))

    case Apply(col, Seq(index)) if col.tpe.isCollection =>
      Some(ByIndex(col.asCollection[SType], index.asValue[SInt.type]))

    case opt: OptionValue[_] =>
      error(s"Option constructors are not supported: $opt")

    case AND(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[AND]) =>
      Some(AND(
        items.flatMap {
          case AND(ConcreteCollection(innerItems, SBoolean)) => innerItems
          case v => IndexedSeq(v)
        }))

    case OR(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[OR]) =>
      Some(OR(
        items.flatMap {
          case OR(ConcreteCollection(innerItems, SBoolean)) => innerItems
          case v => IndexedSeq(v)
        }))

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
