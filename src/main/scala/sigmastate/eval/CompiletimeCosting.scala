package sigmastate.eval

import org.ergoplatform.ErgoBox

import scala.language.{existentials, implicitConversions}
import scapi.sigma.{ProveDiffieHellmanTuple, DLogProtocol}
import sigmastate.SCollection.SByteArray
import sigmastate._
import sigmastate.Values.{Constant, SigmaPropConstant, Value, NotReadyValue, SigmaBoolean}
import sigmastate.lang.Terms.{Apply, _}
import sigmastate.lang.SigmaPredef._
import sigmastate.utxo._
import sigmastate.SType._
import sigmastate.Values.Value.Typed
import sigmastate.lang.SigmaSpecializer.error
import sigmastate.lang.{Terms, TransformingSigmaBuilder}

trait CompiletimeCosting extends RuntimeCosting {
  import builder._

  override def evalNode[T <: SType](ctx: Rep[CostedContext], env: Map[String, RCosted[_]], node: Value[T]): RCosted[T#WrappedType] = {
    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    val res: Sym = node match {
      case Ident(n, _) =>
        env.getOrElse(n, !!!(s"Variable $n not found in environment $env"))

      case _: DLogProtocol.ProveDlog | _: ProveDiffieHellmanTuple =>
        eval(SigmaPropConstant(node.asSigmaBoolean))

      // Rule: allOf(arr) --> AND(arr)
      case Terms.Apply(AllSym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
        eval(mkAND(arr))

      // Rule: anyOf(arr) --> OR(arr)
      case Terms.Apply(AnySym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
        eval(mkOR(arr))

      case Terms.Apply(Blake2b256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
        eval(mkCalcBlake2b256(arg))

      case Terms.Apply(Sha256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
        eval(mkCalcSha256(arg))

      case Terms.Apply(IsMemberSym, Seq(tree: Value[SAvlTree.type]@unchecked, key: Value[SByteArray]@unchecked, proof: Value[SByteArray]@unchecked)) =>
        eval(mkIsMember(tree, key, proof))

      case Terms.Apply(ProveDlogSym, Seq(g: Value[SGroupElement.type]@unchecked)) =>
        eval(mkProveDlog(g))

      case Terms.Apply(LongToByteArraySym, Seq(arg: Value[SLong.type]@unchecked)) =>
        eval(mkLongToByteArray(arg))

      case sigmastate.Upcast(Constant(value, tpe), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

      // Rule: col.size --> SizeOf(col)
      case Select(obj, "size", _) =>
        if (obj.tpe.isCollectionLike)
          eval(mkSizeOf(obj.asValue[SCollection[SType]]))
        else
          error(s"The type of $obj is expected to be Collection to select 'size' property")

      // Rule: proof.isValid --> IsValid(proof)
      case Select(p, SSigmaProp.IsValid, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropIsValid(p.asSigmaProp))

      // Rule: proof.propBytes --> ProofBytes(proof)
      case Select(p, SSigmaProp.PropBytes, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropBytes(p.asSigmaProp))

      case Terms.Apply(PKSym, Seq(arg: Value[SString.type]@unchecked)) =>
        eval(mkPK(arg))

      // box.R$i[valType] =>
      case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        eval(mkExtractRegisterAs(box.asBox, reg, SOption(valType)).asValue[SOption[valType.type]])

      // col.getOrElse(i, default) =>
      case Terms.Apply(Select(col,"getOrElse", _), Seq(index, defaultValue)) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        eval(mkByIndex(col.asValue[SCollection[SType]], index1, Some(defaultValue1)))

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
          case (box, SBox.BytesWithNoRef) => eval(mkExtractBytesWithNoRef(box))
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
        }

      case Select(obj: SigmaBoolean, field, _) =>
        field match {
          case SigmaBoolean.PropBytes => eval(SigmaPropBytes(SigmaPropConstant(obj)))
          case SigmaBoolean.IsValid => eval(SigmaPropIsValid(SigmaPropConstant(obj)))
        }

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        eval(mkSelectField(tuple.asTuple, index))

      case Terms.Apply(Select(col, "slice", _), Seq(from, until)) =>
        eval(mkSlice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

      case _ =>
        super.evalNode(ctx, env, node)
    }
    asRep[Costed[T#WrappedType]](res)
  }
}
