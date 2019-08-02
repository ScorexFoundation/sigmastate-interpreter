package sigmastate.eval

import org.ergoplatform.ErgoBox

import scala.language.{existentials, implicitConversions}
import sigmastate._
import sigmastate.Values.{Constant, Value}
import sigmastate.lang.Terms._
import sigmastate.utxo._
import sigmastate.SType._
import sigmastate.SCollection._
import sigmastate.SBigInt._
import sigmastate.Values.Value.Typed
import sigmastate.lang.Terms
import sigma.util.Extensions._

trait CompiletimeCosting extends RuntimeCosting { IR: IRContext =>
  import builder._
  import SigmaProp._
  import CollBuilder._
  import SigmaDslBuilder._

  override def rewriteDef[T](d: Def[T]): Rep[_] = d match {
    case AllOf(b, HasSigmas(bools, sigmas), _) =>
      val zkAll = sigmaDslBuilder.allZK(b.fromItems(sigmas:_*))
      if (bools.isEmpty)
        zkAll.isValid
      else
        (sigmaDslBuilder.sigmaProp(sigmaDslBuilder.allOf(b.fromItems(bools:_*))) && zkAll).isValid

    case AnyOf(b, HasSigmas(bs, ss), _) =>
      val zkAny = sigmaDslBuilder.anyZK(b.fromItems(ss:_*))
      if (bs.isEmpty)
        zkAny.isValid
      else
        (sigmaDslBuilder.sigmaProp(sigmaDslBuilder.anyOf(b.fromItems(bs:_*))) || zkAny).isValid

    case AllOf(_,items,_) if items.length == 1 => items(0)
    case AnyOf(_,items,_) if items.length == 1 => items(0)
    case AllZk(_,items,_) if items.length == 1 => items(0)
    case AnyZk(_,items,_) if items.length == 1 => items(0)

    case _ => super.rewriteDef(d)
  }

  override def evalNode[T <: SType](ctx: RCosted[Context], env: CostingEnv, node: Value[T]): RCosted[T#WrappedType] = {
    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    val res: Sym = node match {
      case Ident(n, _) =>
        env.getOrElse(n, !!!(s"Variable $n not found in environment $env"))

      case sigmastate.Upcast(Constant(value, _), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

      case sigmastate.Downcast(Constant(value, _), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.downcast(value.asInstanceOf[AnyVal]), toTpe))

      // Rule: col.size --> SizeOf(col)
      case Select(obj, "size", _) =>
        if (obj.tpe.isCollectionLike)
          eval(mkSizeOf(obj.asValue[SCollection[SType]]))
        else
          error(s"The type of $obj is expected to be Collection to select 'size' property", obj.sourceContext.toOption)

      // Rule: proof.isProven --> IsValid(proof)
      case Select(p, SSigmaProp.IsProven, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropIsProven(p.asSigmaProp))

      // Rule: proof.propBytes --> ProofBytes(proof)
      case Select(p, SSigmaProp.PropBytes, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropBytes(p.asSigmaProp))

      // box.R$i[valType] =>
      case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel", sel.sourceContext.toOption))
        eval(mkExtractRegisterAs(box.asBox, reg, SOption(valType)).asValue[SOption[valType.type]])

      // opt.get =>
      case Select(nrv: Value[SOption[SType]]@unchecked, SOption.Get, _) =>
        eval(mkOptionGet(nrv))

      // opt.getOrElse =>
      case Terms.Apply(Select(nrv: Value[SOption[SType]]@unchecked, SOption.GetOrElse, _), Seq(arg)) =>
        eval(mkOptionGetOrElse(nrv, arg))

      // opt.isDefined =>
      case Select(nrv: Value[SOption[SType]]@unchecked, SOption.IsDefined, _) =>
        eval(mkOptionIsDefined(nrv))

      case sel @ Select(obj, field, _) if obj.tpe == SBox =>
        (obj.asValue[SBox.type], field) match {
          case (box, SBox.Value) => eval(mkExtractAmount(box))
          case (box, SBox.PropositionBytes) => eval(mkExtractScriptBytes(box))
          case (box, SBox.Id) => eval(mkExtractId(box))
          case (box, SBox.Bytes) => eval(mkExtractBytes(box))
          case (box, SBox.BytesWithoutRef) => eval(mkExtractBytesWithNoRef(box))
          case (box, SBox.CreationInfo) => eval(mkExtractCreationInfo(box))
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found", sel.sourceContext.toOption)
        }

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        eval(mkSelectField(tuple.asTuple, index))

      case Select(obj, method, Some(tRes: SNumericType))
        if obj.tpe.isNumType && obj.asNumValue.tpe.isCastMethod(method) =>
        val numValue = obj.asNumValue
        if (numValue.tpe == tRes)
          eval(numValue)
        else if ((numValue.tpe max tRes) == numValue.tpe)
          eval(mkDowncast(numValue, tRes))
        else
          eval(mkUpcast(numValue, tRes))

      case Terms.Apply(Select(col, SliceMethod.name, _), Seq(from, until)) =>
        eval(mkSlice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

      case Terms.Apply(Select(col, ExistsMethod.name, _), Seq(l)) if l.tpe.isFunc =>
        eval(mkExists(col.asValue[SCollection[SType]], l.asFunc))

      case Terms.Apply(Select(col, ForallMethod.name, _), Seq(l)) if l.tpe.isFunc =>
        eval(mkForAll(col.asValue[SCollection[SType]], l.asFunc))

      case Terms.Apply(Select(col, MapMethod.name, _), Seq(l)) if l.tpe.isFunc =>
        eval(mkMapCollection(col.asValue[SCollection[SType]], l.asFunc))

      case Terms.Apply(Select(col, FoldMethod.name, _), Seq(zero, l @ Terms.Lambda(_, _, _, _))) =>
        eval(mkFold(col.asValue[SCollection[SType]], zero, l))

      case Terms.Apply(col, Seq(index)) if col.tpe.isCollection =>
        eval(mkByIndex(col.asCollection[SType], index.asValue[SInt.type], None))

      case Select(input, ModQMethod.name, _) =>
        eval(mkModQ(input.asBigInt))

      case Terms.Apply(Select(l, PlusModQMethod.name, _), Seq(r)) =>
        eval(mkPlusModQ(l.asBigInt, r.asBigInt))
      case Terms.Apply(Select(l, MinusModQMethod.name, _), Seq(r)) =>
        eval(mkMinusModQ(l.asBigInt, r.asBigInt))

      case _ =>
        super.evalNode(ctx, env, node)
    }
    asRep[Costed[T#WrappedType]](res)
  }
}
