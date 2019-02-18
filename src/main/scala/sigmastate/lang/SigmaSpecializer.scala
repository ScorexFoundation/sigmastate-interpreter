package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{reduce, rewrite, strategy}
import org.ergoplatform.ErgoAddressEncoder.{NetworkPrefix, TestnetNetworkPrefix}
import org.ergoplatform._
import scalan.Nullable
import scorex.util.encode.{Base58, Base64}
import sigmastate.SCollection._
import sigmastate.Values.Value.Typed
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.{Apply, ApplyTypes, Block, Ident, Lambda, Select, Val, ValueOps}
import sigmastate.lang.exceptions.SpecializerException
import sigmastate.utxo._
import sigma.util.Extensions._

class SigmaSpecializer(val builder: SigmaBuilder) {
  import SigmaSpecializer._
  import builder._

  private implicit val predefFuncRegistry: PredefinedFuncRegistry = new PredefinedFuncRegistry(builder)

  /** Create name -> TaggedXXX(tag) pair to be used in environment. */
  def mkTagged(name: String, tpe: SType, tag: Byte): TaggedVariable[SType] = {
    val tagged = mkTaggedVariable(tag, tpe)
    tagged
  }

  /** Rewriting of AST with respect to environment to resolve all references
    * to let bound and lambda bound names. */
  private def eval(env: Map[String, SValue], e: SValue): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, _) => env.get(n)

    case _ @ Block(binds, res) =>
      var curEnv = env
      for (v @ Val(n, _, b) <- binds) {
        if (curEnv.contains(n)) error(s"${v.sourceContext} Variable $n already defined ($n = ${curEnv(n)}")
        val b1 = eval(curEnv, b)
        curEnv = curEnv + (n -> b1)
      }
      val res1 = eval(curEnv, res)
      Some(res1)

    case Upcast(Constant(value, tpe), toTpe: SNumericType) =>
      Some(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

    case Downcast(Constant(value, tpe), toTpe: SNumericType) =>
      Some(mkConstant(toTpe.downcast(value.asInstanceOf[AnyVal]), toTpe))

    // Rule: numeric.to* casts
    case Select(obj, method, Some(tRes: SNumericType))
      if obj.tpe.isNumType && obj.asNumValue.tpe.isCastMethod(method) =>
      val numValue = obj.asNumValue
      if (numValue.tpe == tRes)
        Some(numValue)
      else if ((numValue.tpe max tRes) == numValue.tpe)
        Some(mkDowncast(numValue, tRes))
      else
        Some(mkUpcast(numValue, tRes))

    // Rule: col.size --> SizeOf(col)
    case Select(obj, SizeMethod.name, _) =>
      if (obj.tpe.isCollectionLike)
        Some(mkSizeOf(obj.asValue[SCollection[SType]]))
      else
        error(s"The type of $obj is expected to be Collection to select 'size' property")

    // Rule: proof.isProven --> IsValid(proof)
    case Select(p, SSigmaProp.IsProven, _) if p.tpe == SSigmaProp =>
      Some(SigmaPropIsProven(p.asSigmaProp))

    // Rule: proof.propBytes --> ProofBytes(proof)
    case Select(p, SSigmaProp.PropBytes, _) if p.tpe == SSigmaProp =>
      Some(SigmaPropBytes(p.asSigmaProp))

    case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
      val reg = ErgoBox.registerByName.getOrElse(regName,
        error(s"Invalid register name $regName in expression $sel"))
      Some(mkExtractRegisterAs(box.asBox, reg, SOption(valType)).asValue[SOption[valType.type]])

    case Select(nrv: NotReadyValue[SOption[SType]]@unchecked, SOption.Get, _) =>
      Some(mkOptionGet(nrv))

    case Apply(Select(nrv: NotReadyValue[SOption[SType]]@unchecked, SOption.GetOrElse, _), Seq(arg)) =>
      Some(mkOptionGetOrElse(nrv, arg))

    case Select(nrv: NotReadyValue[SOption[SType]]@unchecked, SOption.IsDefined, _) =>
      Some(mkOptionIsDefined(nrv))

    case sel @ Select(obj, field, _) if obj.tpe == SBox =>
      (obj.asValue[SBox.type], field) match {
        case (box, SBox.Value) => Some(mkExtractAmount(box))
        case (box, SBox.PropositionBytes) => Some(mkExtractScriptBytes(box))
        case (box, SBox.Id) => Some(mkExtractId(box))
        case (box, SBox.Bytes) => Some(mkExtractBytes(box))
        case (box, SBox.BytesWithNoRef) => Some(mkExtractBytesWithNoRef(box))
        case (box, SBox.CreationInfo) => Some(mkExtractCreationInfo(box))
        case (box, _) if box.tpe.hasMethod(field) =>
          None  // leave it as it is and handle on a level of parent node
        case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
      }

    case node @ Select(obj: SigmaBoolean, field, _) =>
      field match {
        case SigmaBoolean.PropBytes => Some(ByteArrayConstant(obj.bytes))
        case _ => None
      }

    case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
      val index = fn.substring(1).toByte
      Some(mkSelectField(tuple.asTuple, index))

    case Apply(Select(col, SliceMethod.name, _), Seq(from, until)) =>
      Some(mkSlice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

    case Apply(Select(col, FilterMethod.name, _), Seq(Lambda(_, Seq((n, t)), _, Some(body)))) =>
      val tagged = mkTagged(n, t, 21)
      val body1 = eval(env + (n -> tagged), body)
      Some(mkFilter(col.asValue[SCollection[SType]], tagged.varId, body1.asValue[SBoolean.type]))

    case Apply(Select(col, ExistsMethod.name, _), Seq(l @ Lambda(_, _, _, _))) =>
      Some(mkExists(col.asValue[SCollection[SType]], l))

    case Apply(Select(col, ForallMethod.name, _), Seq(l @ Lambda(_, _, _, _))) =>
      Some(mkForAll(col.asValue[SCollection[SType]], l))

    case Apply(Select(col, MapMethod.name, _), Seq(l @ Lambda(_, _, _, _))) =>
      Some(mkMapCollection(col.asValue[SCollection[SType]], l))

    case Apply(Select(col, FoldMethod.name, _), Seq(zero, l @ Lambda(_, _, _, _))) =>
      Some(mkFold(col.asValue[SCollection[SType]], zero, l))

    case Apply(Select(col, GetOrElseMethod.name, _), Seq(index, defaultValue)) =>
      val index1 = eval(env, index).asValue[SInt.type]
      val defaultValue1 = eval(env, defaultValue).asValue[SType]
      Some(mkByIndex(col.asValue[SCollection[SType]], index1, Some(defaultValue1)))

    case Apply(col, Seq(index)) if col.tpe.isCollection =>
      Some(ByIndex(col.asCollection[SType], index.asValue[SInt.type]))

    case opt: OptionValue[_] =>
      error(s"Option constructors are not supported: $opt")

    case AND(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[AND]) =>
      Some(mkAND(
        mkConcreteCollection(
          items.flatMap {
            case AND(ConcreteCollection(innerItems, SBoolean)) => innerItems
            case v => IndexedSeq(v)
          }, SBoolean)))

    case OR(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[OR]) =>
      Some(mkOR(
        mkConcreteCollection(
          items.flatMap {
            case OR(ConcreteCollection(innerItems, SBoolean)) => innerItems
            case v => IndexedSeq(v)
          }, SBoolean)))

    case PredefinedFuncApply(irNode) =>
      Some(irNode)

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
  def error(msg: String, srcCtx: SourceContext) = throw new SpecializerException(msg, Some(srcCtx))
}
