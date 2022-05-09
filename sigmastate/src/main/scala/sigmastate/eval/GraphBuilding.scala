package sigmastate.eval

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules.{CheckIsSupportedIndexExpression, CheckTupleType}
import scalan.{Lazy, MutableLazy, Nullable, SigmaLibrary}
import sigmastate.Values.Value.Typed
import sigmastate.{AND, ArithOp, AtLeast, BinAnd, BinOr, BinXor, BoolToSigmaProp, ByteArrayToLong, CalcBlake2b256, CalcSha256, CreateProveDHTuple, CreateProveDlog, DecodePoint, If, LogicalNot, ModQ, ModQArithOp, Negation, OR, Relation, SBigInt, SBox, SCollection, SCollectionType, SGlobal, SGroupElement, SInt, SNumericType, SOption, SSigmaProp, SType, SigmaAnd, SigmaOr, SubstConstants, Values, Xor, XorOf, utxo}
import sigmastate.Values._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms
import sigmastate.lang.Terms.{Ident, Select, Val, ValueOps}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.{DivisionCode, MaxCode, MinCode, MinusCode, ModuloCode, MultiplyCode, PlusCode}
import sigmastate.utxo.{Append, BooleanTransformer, ByIndex, CostTable, Filter, Fold, GetVar, MapCollection, SelectField, SigmaPropBytes, SigmaPropIsProven, Slice}
import spire.syntax.all.cfor

import scala.collection.mutable

trait GraphBuilding extends SigmaLibrary { this: IRContext =>
  import builder._
  import Liftables._
  import Context._;
  import Header._;
  import PreHeader._;
  import GroupElement._;
  import BigInt._;
  import WOption._
  import Coll._;
  import CollBuilder._;
  import SigmaProp._;
  import Box._
  import SigmaDslBuilder._
  import MonoidBuilder._
  import AvlTree._
  import WSpecialPredef._

  def doBuild(env: ScriptEnv, typed: SValue, okRemoveIsProven: Boolean): Ref[Context => Any] = {
    val g = buildGraph[Any](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    g
  }

  def buildGraph[T](envVals: Map[Any, SValue], tree: SValue): Ref[Context => T] = {
    fun { ctxC: Ref[Context] =>
      val env = envVals.mapValues(v => buildNode(ctxC, Map.empty, v))
      val res = asRep[T](buildNode(ctxC, env, tree))
      res
    }
  }

  type CompilingEnv = Map[Any, Ref[_]]


  protected def buildNode[T <: SType](ctx: Ref[Context], env: CompilingEnv, node: Value[T]): Ref[T#WrappedType] = {
    def eval[T <: SType](node: Value[T]): Ref[T#WrappedType] = buildNode(ctx, env, node)
    object In { def unapply(v: SValue): Nullable[Ref[Any]] = Nullable(asRep[Any](buildNode(ctx, env, v))) }
    class InColl[T: Elem] {
      def unapply(v: SValue): Nullable[Ref[Coll[T]]] = {
        val res = asRep[Def[_]](buildNode(ctx, env, v))
        Nullable(tryCast[Coll[T]](res))
      }
    }
    val InCollByte = new InColl[Byte]; val InCollAny = new InColl[Any]()(AnyElement); val InCollInt = new InColl[Int]

    val InCollCollByte = new InColl[Coll[Byte]]()(eCollByte)
    val InPairCollByte = new InColl[(Coll[Byte], Coll[Byte])]()(ePairOfCollByte)

    object InSeq { def unapply(items: Seq[SValue]): Nullable[Seq[Ref[Any]]] = {
      val res = items.map { x: SValue =>
        val r = eval(x)
        asRep[Any](r)
      }
      Nullable(res)
    }}

    val res: Ref[Any] = node match {
      case c @ Constant(v, tpe) => v match {
        case p: SSigmaProp =>
          assert(tpe == SSigmaProp)
          val resV = liftConst(p)
          resV
        case bi: SBigInt =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          resV
        case p: SGroupElement =>
          assert(tpe == SGroupElement)
          val resV = liftConst(p)
          resV
        case coll: SColl[a] =>
          val tpeA = tpe.asCollection[SType].elemType
          stypeToElem(tpeA) match {
            case eWA: Elem[wa] =>
              implicit val l = liftableFromElem[wa](eWA).asInstanceOf[Liftable[a, wa]]
              val resVals = liftConst[SColl[a], Coll[wa]](coll)
              resVals
          }
        case box: SBox =>
          val boxV = liftConst(box)
          boxV
        case tree: special.sigma.AvlTree =>
          val treeV = liftConst(tree)
          treeV
        case s: String =>
          val resV = toRep(s)(stypeToElem(tpe).asInstanceOf[Elem[String]])
          resV
        case _ =>
          val resV = toRep(v)(stypeToElem(tpe))
          resV
      }
      case org.ergoplatform.Context => ctx
      case Global => sigmaDslBuilder
      case Height => ctx.HEIGHT
      case Inputs => ctx.INPUTS
      case Outputs => ctx.OUTPUTS
      case Self => ctx.SELF
      case LastBlockUtxoRootHash => ctx.LastBlockUtxoRootHash
      case MinerPubkey => ctx.minerPubKey

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

      // Rule: prop.propBytes --> SigmaProofBytes(prop)
      case Select(p, SSigmaProp.PropBytes, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropBytes(p.asSigmaProp))

      // box.R$i[valType] =>
      case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel", sel.sourceContext.toOption))
        eval(mkExtractRegisterAs(box.asBox, reg, SOption(valType)).asValue[SOption[valType.type]])

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

      case Terms.Apply(col, Seq(index)) if col.tpe.isCollection =>
        eval(mkByIndex(col.asCollection[SType], index.asValue[SInt.type], None))


      //      case op @ GetVar(id, optTpe) =>
//        stypeToElem(optTpe.elemType) match { case e: Elem[t] =>
//          val v = ctx.value.getVar[t](id)(e)
//          val s = tryCast[SizeContext](ctx.size).getVar(id)(e)
//          RCCostedPrim(v, opCost(v, Nil, sigmaDslBuilder.CostModel.GetVar), s)
//        }
//
//      case Terms.Block(binds, res) =>
//        var curEnv = env
//        for (v @ Val(n, _, b) <- binds) {
//          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}", v.sourceContext.toOption)
//          val bC = evalNode(ctx, curEnv, b)
//          curEnv = curEnv + (n -> bC)
//        }
//        val res1 = evalNode(ctx, curEnv, res)
//        res1
//
//      case BlockValue(binds, res) =>
//        var curEnv = env
//        val len = binds.length
//        cfor(0)(_ < len, _ + 1) { i =>
//          val vd = binds(i).asInstanceOf[ValDef]
//          val n = vd.id
//          val b = vd.rhs
//          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}", vd.sourceContext.toOption)
//          val bC = evalNode(ctx, curEnv, b)
//          curEnv = curEnv + (n -> bC)
//        }
//        val res1 = evalNode(ctx, curEnv, res)
//        res1
//
//      case ValUse(valId, _) =>
//        env.getOrElse(valId, !!!(s"ValUse $valId not found in environment $env"))
//
//      case CreateProveDlog(In(_v)) =>
//        val vC = asRep[Costed[GroupElement]](_v)
//        val resV: Ref[SigmaProp] = sigmaDslBuilder.proveDlog(vC.value)
//        val cost = opCost(resV, Array(vC.cost), CostOfProveDlog)
//        RCCostedPrim(resV, cost, SizeSigmaProposition)
//
//      case CreateProveDHTuple(In(_gv), In(_hv), In(_uv), In(_vv)) =>
//        val gvC = asRep[Costed[GroupElement]](_gv)
//        val hvC = asRep[Costed[GroupElement]](_hv)
//        val uvC = asRep[Costed[GroupElement]](_uv)
//        val vvC = asRep[Costed[GroupElement]](_vv)
//        val resV: Ref[SigmaProp] = sigmaDslBuilder.proveDHTuple(gvC.value, hvC.value, uvC.value, vvC.value)
//        val cost = opCost(resV, Array(gvC.cost, hvC.cost, uvC.cost, vvC.cost), CostOfDHTuple)
//        RCCostedPrim(resV, cost, SizeSigmaProposition)
//
//      case sigmastate.Exponentiate(In(_l), In(_r)) =>
//        val l = asRep[Costed[GroupElement]](_l)
//        val r = asRep[Costed[BigInt]](_r)
//        val value = l.value.exp(r.value)
//        val cost = opCost(value, Array(l.cost, r.cost), costOf(node))
//        RCCostedPrim(value, cost, SizeGroupElement)
//
//      case sigmastate.MultiplyGroup(In(_l), In(_r)) =>
//        val l = asRep[Costed[GroupElement]](_l)
//        val r = asRep[Costed[GroupElement]](_r)
//        val value = l.value.multiply(r.value)
//        val cost = opCost(value, Array(l.cost, r.cost), costOf(node))
//        RCCostedPrim(value, cost, SizeGroupElement)
//
//      case Values.GroupGenerator =>
//        SigmaDslBuilderCoster(costedGlobal, SGlobal.groupGeneratorMethod, Nil)
//
//      case sigmastate.ByteArrayToBigInt(In(_arr)) =>
//        val arrC = asRep[Costed[Coll[Byte]]](_arr)
//        val arr = arrC.value
//        val value = sigmaDslBuilder.byteArrayToBigInt(arr)
//        val size = arrC.size.dataSize
//        val cost = opCost(value, Array(arrC.cost), costOf(node) + costOf("new_BigInteger_per_item", node.opType) * size.toInt)
//        RCCostedPrim(value, cost, SizeBigInt)
//
//      case sigmastate.LongToByteArray(In(_x)) =>
//        val xC = asRep[Costed[Long]](_x)
//        val col = sigmaDslBuilder.longToByteArray(xC.value) // below we assume col.length == typeSize[Long]
//        val cost = opCost(col, Array(xC.cost), costOf(node))
//        LongBytesInfo.mkCostedColl(col, cost)
//
//      // opt.get =>
//      case utxo.OptionGet(In(_opt)) =>
//        OptionCoster(_opt, SOption.GetMethod, Nil)
//
//      // opt.isDefined =>
//      case utxo.OptionIsDefined(In(_opt)) =>
//        OptionCoster(_opt, SOption.IsDefinedMethod, Nil)
//
//      // opt.getOrElse =>
//      case utxo.OptionGetOrElse(In(_opt), In(_default)) =>
//        OptionCoster(_opt, SOption.GetOrElseMethod, Array(_default))
//
//      case SelectField(In(_tup), fieldIndex) =>
//        val eTuple = _tup.elem.eVal.asInstanceOf[Elem[_]]
//        CheckTupleType(IR)(eTuple)
//        eTuple match {
//          case pe: PairElem[a,b] =>
//            assert(fieldIndex == 1 || fieldIndex == 2, s"Invalid field index $fieldIndex of the pair ${_tup}: $pe")
//            implicit val ea = pe.eFst
//            implicit val eb = pe.eSnd
//            val pair = tryCast[CostedPair[a,b]](_tup)
//            val res = if (fieldIndex == 1)
//              attachCost(pair.l, pair.accCost, selectFieldCost)
//            else
//              attachCost(pair.r, pair.accCost, selectFieldCost)
//            res
//          // TODO soft-fork: implement similar to Pair case
//          //          case se: StructElem[_] =>
//          //            val tup = asRep[Costed[Struct]](_tup)
//          //            val fn = STuple.componentNameByIndex(fieldIndex - 1)
//          //            val v = tup.value.getUntyped(fn)
//          //            val c = opCost(v, Seq(tup.cost), costedBuilder.SelectFieldCost)
//          //            val s: RSize[Any] = ???
//          //            RCCostedPrim(v, c, s)
//        }
//
//      case Values.Tuple(InSeq(Seq(x, y))) =>
//        val v = Pair(x, y)
//        val costs = Array(x.cost, y.cost, CostTable.newPairValueCost: Ref[Int])
//        val c = mkNormalizedOpCost(v, costs)
//        RCCostedPair(x, y, c)
//
//      case node: BooleanTransformer[_] =>
//        val tpeIn = node.input.tpe.elemType
//        val eIn = stypeToElem(tpeIn)
//        val xs = asRep[CostedColl[Any]](eval(node.input))
//        val eAny = xs.elem.asInstanceOf[CostedElem[Coll[Any],_]].eVal.eA
//        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
//        val conditionC = asRep[CostedFunc[Unit, Any, SType#WrappedType]](evalNode(ctx, env, node.condition))
//        val condC = conditionC.func
//        val (calcF, costF) = splitCostedFunc2(condC, okRemoveIsValid = true)
//        val sizeF = condC.sliceSize
//        val sizes = xs.sizes
//        val len = sizes.length
//        val cost = if (tpeIn.isConstantSize) {
//          val predicateCost: Ref[Int] = Apply(costF, Pair(IntZero, constantTypeSize(eAny)), false)
//          len * (predicateCost + CostTable.lambdaInvoke)
//        } else {
//          colBuilder.replicate(len, IntZero).zip(sizes).map(costF).sum(intPlusMonoid) + len * CostTable.lambdaInvoke
//        }
//        val res = costedBooleanTransformer[Any](node, xs, conditionC, calcF, cost)
//        res
//
//      case MapCollection(input, sfunc) =>
//        val inputC = evalNode(ctx, env, input)
//        val mapper = evalNode(ctx, env, sfunc)
//        val res = CollCoster(inputC, SCollection.MapMethod, Array(mapper))
//        res
//
//      case Fold(input, zero, sfunc) =>
//        val eItem = stypeToElem(input.tpe.elemType)
//        val eState = stypeToElem(zero.tpe)
//        (eState, eItem) match { case (eState: Elem[s], eItem: Elem[a]) =>
//          val inputC = asRep[CostedColl[a]](eval(input))
//          implicit val eA = inputC.elem.asInstanceOf[CostedElem[Coll[a],_]].eVal.eA
//          assert(eItem == eA, s"Types should be equal: but $eItem != $eA")
//
//          val zeroC = asRep[Costed[s]](eval(zero))
//          implicit val eS = zeroC.elem.eVal
//          assert(eState == eS, s"Types should be equal: but $eState != $eS")
//
//          val foldOpC = fun { in: Ref[CostedPair[s, a]] =>
//            val acc = in.l; val item = in.r
//            val out = sfunc match {
//              case Terms.Lambda(_, Seq((accN, _), (n, _)), _, Some(op)) =>
//                evalNode(ctx, env + (accN -> acc, n -> item), op)
//              case FuncValue(Seq((tpl, _)), op) =>
//                evalNode(ctx, env + (tpl -> in), op)
//            }
//            asRep[Costed[s]](out)
//          }
//          val res = inputC.foldCosted(zeroC, asRep[Costed[(s,a)] => Costed[s]](foldOpC))
//          res
//        }
//
//      case op @ Slice(In(input), In(from), In(until)) =>
//        val inputC = asRep[CostedColl[Any]](input)
//        val fromC = asRep[Costed[Int]](from)
//        val untilC = asRep[Costed[Int]](until)
//        val f = fromC.value
//        val u = untilC.value
//        val vals = inputC.values.slice(f, u)
//        val costs = inputC.costs
//        val sizes = inputC.sizes
//        RCCostedColl(vals, costs, sizes, opCost(vals, Array(inputC.valuesCost), costOf(op)))
//
//      case Append(In(_col1), In(_col2)) =>
//        val col1 = asRep[CostedColl[Any]](_col1)
//        val col2 = asRep[CostedColl[Any]](_col2)
//        val values = col1.values.append(col2.values)
//        val costs = col1.costs.append(col2.costs)
//        val sizes = col1.sizes.append(col2.sizes)
//        RCCostedColl(values, costs, sizes, opCost(values, Array(col1.cost, col2.cost), costOf(node)))
//
//      case Filter(input, p) =>
//        val inputC = evalNode(ctx, env, input)
//        val pC = evalNode(ctx, env, p)
//        val res = CollCoster(inputC, SCollection.FilterMethod, Array(pC))
//        res
//
//      case Terms.Apply(f, Seq(x)) if f.tpe.isFunc =>
//        val fC = asRep[CostedFunc[Unit, Any, Any]](evalNode(ctx, env, f))
//        val xC = asRep[Costed[Any]](evalNode(ctx, env, x))
//        f.tpe.asFunc.tRange match {
//          case _: SCollectionType[_] =>
//            val (calcF, costF, sizeF) = splitCostedCollFunc(asRep[CostedCollFunc[Any,Any]](fC.func))
//            val value = xC.value
//            val values: RColl[Any] = Apply(calcF, value, false)
//            val costRes: Ref[(Coll[Int], Int)] = Apply(costF, Pair(xC.cost, xC.size), false)
//            val sizes: RColl[Size[Any]] = Apply(sizeF, xC.size, false)
//            RCCostedColl(values, costRes._1, sizes, costRes._2)
//          //          case optTpe: SOption[_] =>
//          //            val (calcF, costF, sizeF) = splitCostedOptionFunc(asRep[CostedOptionFunc[Any,Any]](fC.func))
//          //            val value = xC.value
//          //            val values: Ref[WOption[Any]] = Apply(calcF, value, false)
//          //            val costRes: Ref[(WOption[Int], Int)] = Apply(costF, Pair(value, Pair(xC.cost, xC.dataSize)), false)
//          //            val sizes: Ref[WOption[Long]]= Apply(sizeF, Pair(value, xC.dataSize), false)
//          //            RCCostedOption(values, costRes._1, sizes, costRes._2)
//          case _ =>
//            val calcF = fC.sliceCalc
//            val costF = fC.sliceCost
//            val sizeF = fC.sliceSize
//            val value = xC.value
//            val y: Ref[Any] = Apply(calcF, value, false)
//            val c: Ref[Int] = opCost(y, Array(fC.cost, xC.cost), asRep[Int](Apply(costF, Pair(IntZero, xC.size), false)) + CostTable.lambdaInvoke)
//            val s: Ref[Size[Any]]= Apply(sizeF, xC.size, false)
//            RCCostedPrim(y, c, s)
//        }
//
//      case opt: OptionValue[_] =>
//        error(s"Option constructors are not supported: $opt", opt.sourceContext.toOption)
//
//      case CalcBlake2b256(In(input)) =>
//        val bytesC = asRep[Costed[Coll[Byte]]](input)
//        val res = sigmaDslBuilder.blake2b256(bytesC.value)
//        val cost = opCost(res, Array(bytesC.cost), perKbCostOf(node, bytesC.size.dataSize))
//        HashInfo.mkCostedColl(res, cost)
//      case CalcSha256(In(input)) =>
//        val bytesC = asRep[Costed[Coll[Byte]]](input)
//        val res = sigmaDslBuilder.sha256(bytesC.value)
//        val cost = opCost(res, Array(bytesC.cost), perKbCostOf(node, bytesC.size.dataSize))
//        HashInfo.mkCostedColl(res, cost)
//
//      case utxo.SizeOf(In(xs)) =>
//        xs.elem.eVal.asInstanceOf[Any] match {
//          case ce: CollElem[a,_] =>
//            val xsC = asRep[Costed[Coll[a]]](xs)
//            val v = xsC.value.length
//            RCCostedPrim(v, opCost(v, Array(xsC.cost), costOf(node)), SizeInt)
//          case pe: PairElem[a,b] =>
//            val xsC = asRep[Costed[(a,b)]](xs)
//            val v: Ref[Int] = 2
//            RCCostedPrim(v, opCost(v, Array(xsC.cost), costOf(node)), SizeInt)
//        }

      case ByIndex(xs, i, defaultOpt) =>
        val xsV = asRep[Coll[Any]](eval(xs))
        val iV = asRep[Int](eval(i))

        defaultOpt match {
          case Some(defaultValue) =>
            val defaultV = asRep[Any](eval(defaultValue))
            val value = xsV.getOrElse(iV, defaultV)
            value
          case None =>
            val value = xsV(iV)
            value
        }

//      case SigmaPropIsProven(p) =>
//        val pC = asRep[Costed[SigmaProp]](eval(p))
//        val v = pC.value.isValid
//        val c = opCost(v, Array(pC.cost), costOf(node))
//        RCCostedPrim(v, c, SizeBoolean)
//      case SigmaPropBytes(p) =>
//        val pC = asRep[Costed[SigmaProp]](eval(p))
//        val v = pC.value.propBytes
//        SigmaPropBytesInfo.mkCostedColl(v, opCost(v, Array(pC.cost), costOf(node)))
//
//      case utxo.ExtractId(In(box)) =>
//        val boxC = asRep[Costed[Box]](box)
//        val id = boxC.value.id
//        HashInfo.mkCostedColl(id, opCost(id, Array(boxC.cost), costOf(node)))
//      case utxo.ExtractBytesWithNoRef(In(box)) =>
//        val boxC = asRep[Costed[Box]](box)
//        val v = boxC.value.bytesWithoutRef
//        BoxBytesWithoutRefsInfo.mkCostedColl(v, opCost(v, Array(boxC.cost), costOf(node)))
//      case utxo.ExtractAmount(In(box)) =>
//        val boxC = asRep[Costed[Box]](box)
//        val v = boxC.value.value
//        val c = opCost(v, Array(boxC.cost), costOf(node))
//        RCCostedPrim(v, c, SizeLong)
//      case utxo.ExtractScriptBytes(In(box)) =>
//        val boxC = asRep[Costed[Box]](box)
//        val bytes = boxC.value.propositionBytes
//        BoxPropositionBytesInfo.mkCostedColl(bytes, opCost(bytes, Array(boxC.cost), costOf(node)))
//      case utxo.ExtractBytes(In(box)) =>
//        val boxC = asRep[Costed[Box]](box)
//        val bytes = boxC.value.bytes
//        BoxBytesInfo.mkCostedColl(bytes, opCost(bytes, Array(boxC.cost), costOf(node)))
//      case utxo.ExtractCreationInfo(In(box)) =>
//        BoxCoster(box, SBox.creationInfoMethod, Nil)
//      case utxo.ExtractRegisterAs(In(box), regId, optTpe) =>
//        implicit val elem = stypeToElem(optTpe.elemType).asInstanceOf[Elem[Any]]
//        val i: RCosted[Int] = RCCostedPrim(regId.number.toInt, IntZero, SizeInt)
//        BoxCoster(box, SBox.getRegMethod, Array(i), Array(liftElem(elem)))
//
//      case BoolToSigmaProp(bool) =>
//        val boolC = eval(bool)
//        val value = sigmaDslBuilder.sigmaProp(boolC.value)
//        val cost = opCost(value, Array(boolC.cost), costOf(node))
//        RCCostedPrim(value, cost, SizeSigmaProposition)
//
//      case AtLeast(bound, input) =>
//        val inputC = asRep[CostedColl[SigmaProp]](evalNode(ctx, env, input))
//        if (inputC.values.length.isConst) {
//          val inputCount = valueFromRep(inputC.values.length)
//          if (inputCount > AtLeast.MaxChildrenCount)
//            error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount", node.sourceContext.toOption)
//        }
//        val boundC = eval(bound)
//        val res = sigmaDslBuilder.atLeast(boundC.value, inputC.values)
//        val cost = opCost(res, Array(boundC.cost, inputC.cost), costOf(node))
//        RCCostedPrim(res, cost, SizeSigmaProposition)
//
//      case op: ArithOp[t] if op.tpe == SBigInt =>
//        import OpCodes._
//        val xC = asRep[Costed[BigInt]](eval(op.left))
//        val yC = asRep[Costed[BigInt]](eval(op.right))
//        val opName = op.opName
//        var v: Ref[BigInt] = null;
//        op.opCode match {
//          case PlusCode =>
//            v = xC.value.add(yC.value)
//          case MinusCode =>
//            v = xC.value.subtract(yC.value)
//          case MultiplyCode =>
//            v = xC.value.multiply(yC.value)
//          case DivisionCode =>
//            v = xC.value.divide(yC.value)
//          case ModuloCode =>
//            v = xC.value.mod(yC.value)
//          case MinCode =>
//            v = xC.value.min(yC.value)
//          case MaxCode =>
//            v = xC.value.max(yC.value)
//          case code => error(s"Cannot perform Costing.evalNode($op): unknown opCode ${code}", op.sourceContext.toOption)
//        }
//        val c = opCost(v, Array(xC.cost, yC.cost), costOf(op))
//        RCCostedPrim(v, c, SizeBigInt)
//
//      case op: ArithOp[t] =>
//        val tpe = op.left.tpe
//        val et = stypeToElem(tpe)
//        val binop = opcodeToEndoBinOp(op.opCode, et)
//        val x = evalNode(ctx, env, op.left)
//        val y = evalNode(ctx, env, op.right)
//        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
//          val v = ApplyBinOp(binop, x.value, y.value)
//          withConstantSize(v, opCost(v, Array(x.cost, y.cost), costOf(op)))
//        }
//
//      case LogicalNot(input) =>
//        val inputC = evalNode(ctx, env, input)
//        val v = ApplyUnOp(Not, inputC.value)
//        withConstantSize(v, opCost(v, Array(inputC.cost), costOf(node)))
//
//      case ModQ(input) =>
//        val inputC = asRep[Costed[BigInt]](eval(input))
//        val v = inputC.value.modQ
//        RCCostedPrim(v, opCost(v, Array(inputC.cost), costOf(node)), SizeBigInt)
//
//      case ModQArithOp(l, r, code) =>
//        val lC = asRep[Costed[BigInt]](eval(l))
//        val rC = asRep[Costed[BigInt]](eval(r))
//        val v = code match {
//          case OpCodes.PlusModQCode => lC.value.plusModQ(rC.value)
//          case OpCodes.MinusModQCode => lC.value.minusModQ(rC.value)
//          case code => error(s"unknown code for modular arithmetic op: $code")
//        }
//        RCCostedPrim(v, opCost(v, Array(lC.cost, rC.cost), costOf(node)), SizeBigInt)
//
//      case OR(input) => input match {
//        case ConcreteCollection(items, _) =>
//          val len = items.length
//          val values = new Array[Ref[Boolean]](len)
//          val costs = new Array[Ref[Int]](len)
//          cfor(0)(_ < len, _ + 1) { i =>
//            val item = items(i)
//            val itemC = eval(adaptSigmaBoolean(item))
//            values(i) = itemC.value
//            costs(i) = itemC.cost
//          }
//          val res = sigmaDslBuilder.anyOf(colBuilder.fromItems(values: _*))
//          val nOps = costs.length - 1
//          val cost = opCost(res, costs, perItemCostOf(node, nOps))
//          withConstantSize(res, cost)
//        case _ =>
//          val inputC = asRep[CostedColl[Boolean]](eval(input))
//          val res = sigmaDslBuilder.anyOf(inputC.value)
//          val nOps = inputC.sizes.length - 1
//          val cost = opCost(res, Array(inputC.cost), perItemCostOf(node, nOps))
//          withConstantSize(res, cost)
//      }
//
//      case AND(input) => input match {
//        case ConcreteCollection(items, _) =>
//          val len = items.length
//          val values = new Array[Ref[Boolean]](len)
//          val costs = new Array[Ref[Int]](len)
//          cfor(0)(_ < len, _ + 1) { i =>
//            val item = items(i)
//            val itemC = eval(adaptSigmaBoolean(item))
//            values(i) = itemC.value
//            costs(i) = itemC.cost
//          }
//          val res = sigmaDslBuilder.allOf(colBuilder.fromItems(values: _*))
//          val nOps = costs.length - 1
//          val cost = opCost(res, costs, perItemCostOf(node, nOps))
//          withConstantSize(res, cost)
//        case _ =>
//          val inputC = tryCast[CostedColl[Boolean]](eval(input))
//          val res = sigmaDslBuilder.allOf(inputC.value)
//          val nOps = inputC.sizes.length - 1
//          val cost = opCost(res, Array(inputC.cost), perItemCostOf(node, nOps))
//          withConstantSize(res, cost)
//      }
//
//      case XorOf(input) => input match {
//        case ConcreteCollection(items, _) =>
//          val len = items.length
//          val values = new Array[Ref[Boolean]](len)
//          val costs = new Array[Ref[Int]](len)
//          cfor(0)(_ < len, _ + 1) { i =>
//            val item = items(i)
//            val itemC = eval(adaptSigmaBoolean(item))
//            values(i) = itemC.value
//            costs(i) = itemC.cost
//          }
//          val res = sigmaDslBuilder.xorOf(colBuilder.fromItems(values: _*))
//          val nOps = costs.length - 1
//          val cost = opCost(res, costs, perItemCostOf(node, nOps))
//          withConstantSize(res, cost)
//        case _ =>
//          val inputC = tryCast[CostedColl[Boolean]](eval(input))
//          val res = sigmaDslBuilder.xorOf(inputC.value)
//          val nOps = inputC.sizes.length - 1
//          val cost = opCost(res, Array(inputC.cost), perItemCostOf(node, nOps))
//          withConstantSize(res, cost)
//      }
//
//      case BinOr(l, r) =>
//        val lC = evalNode(ctx, env, l)
//        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), IntZero)
//        val v = Or.applyLazy(lC.value, rC.value)
//        val c = opCost(v, Array(lC.cost, rC.cost), costOf(node))
//        withConstantSize(v, c)
//
//      case BinAnd(l, r) =>
//        val lC = evalNode(ctx, env, l)
//        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), IntZero)
//        val v = And.applyLazy(lC.value, rC.value)
//        val c = opCost(v, Array(lC.cost, rC.cost), costOf(node))
//        withConstantSize(v, c)
//
//      case BinXor(l, r) =>
//        val lC = evalNode(ctx, env, l)
//        val rC = evalNode(ctx, env, r)
//        val v = BinaryXorOp.apply(lC.value, rC.value)
//        val c = opCost(v, Array(lC.cost, rC.cost), costOf(node))
//        withConstantSize(v, c)
//
//      case neg: Negation[SNumericType]@unchecked =>
//        val tpe = neg.input.tpe
//        val et = stypeToElem(tpe)
//        val op = NumericNegate(elemToExactNumeric(et))(et)
//        val inputC = evalNode(ctx, env, neg.input)
//        inputC match { case x: RCosted[a] =>
//          val v = ApplyUnOp(op, x.value)
//          withConstantSize(v, opCost(v, Array(x.cost), costOf(neg)))
//        }
//
//      case SigmaAnd(items) =>
//        val len = items.length
//        val values = new Array[Ref[SigmaProp]](len)
//        val costs = new Array[Ref[Int]](len)
//        cfor(0)(_ < len, _ + 1) { i =>
//          val item = items(i)
//          val itemC = eval(item)
//          values(i) = asRep[SigmaProp](itemC.value)
//          costs(i) = itemC.cost
//        }
//        val res = sigmaDslBuilder.allZK(colBuilder.fromItems(values: _*))
//        val cost = opCost(res, costs, perItemCostOf(node, costs.length))
//        RCCostedPrim(res, cost, SizeSigmaProposition)
//
//      case SigmaOr(items) =>
//        val len = items.length
//        val values = new Array[Ref[SigmaProp]](len)
//        val costs = new Array[Ref[Int]](len)
//        cfor(0)(_ < len, _ + 1) { i =>
//          val item = items(i)
//          val itemC = eval(item)
//          values(i) = asRep[SigmaProp](itemC.value)
//          costs(i) = itemC.cost
//        }
//        val res = sigmaDslBuilder.anyZK(colBuilder.fromItems(values: _*))
//        val cost = opCost(res, costs, perItemCostOf(node, costs.length))
//        RCCostedPrim(res, cost, SizeSigmaProposition)
//
//      case If(c, t, e) =>
//        val cC = evalNode(ctx, env, c)
//        def tC = evalNode(ctx, env, t)
//        def eC = evalNode(ctx, env, e)
//        val resV = IF (cC.value) THEN tC.value ELSE eC.value
//        val resCost = opCost(resV, Array(cC.cost, tC.cost, eC.cost), costOf("If", If.GenericOpType))
//        RCCostedPrim(resV, resCost, tC.size) // TODO costing: implement tC.size max eC.size
//
//      case rel: Relation[t, _] =>
//        val tpe = rel.left.tpe
//        val et = stypeToElem(tpe)
//        val binop = opcodeToBinOp(rel.opCode, et)
//        val x = eval(rel.left)
//        val y = eval(rel.right)
//        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
//          val value = binop.apply(x.value, asRep[t#WrappedType](y.value))
//          val cost =
//            if (tpe.isConstantSize) {
//              val opcost = if (tpe == SBigInt) {
//                costOf(rel.opName, SBigInt.RelationOpType)
//              } else
//                costOf(rel)
//              opCost(value, Array(x.cost, y.cost), opcost)
//            }
//            else opCost(value, Array(x.cost, y.cost), perKbCostOf(node, x.size.dataSize + y.size.dataSize))
//          val res = withConstantSize(value, cost)
//          res
//        }
//
//      case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
//        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
//        val eCostedArg = elemToCostedElem(eArg)
//        val f = fun { x: Ref[Costed[Any]] =>
//          evalNode(ctx, env + (n -> x), body)
//        }(Lazy(eCostedArg))
//        val eRes = f.elem.eRange.eVal
//        mkCostedFunc(f, opCost(f, Nil, costOf(node)), l.tpe.dataSize(SType.DummyValue), eArg, eRes)
//
//      case l @ FuncValue(Seq((n, argTpe)), body) =>
//        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
//        val xElem = elemToCostedElem(eArg)
//        val f = fun { x: Ref[Costed[Any]] =>
//          evalNode(ctx, env + (n -> x), body)
//        }(Lazy(xElem))
//        val eRes = f.elem.eRange.eVal
//        mkCostedFunc(f, opCost(f, Nil, costOf(node)), l.tpe.dataSize(SType.DummyValue), eArg, eRes)

      case col @ ConcreteCollection(InSeq(vs), elemType) =>
        implicit val eAny = stypeToElem(elemType).asInstanceOf[Elem[Any]]
        val values = colBuilder.fromItems(vs: _*)(eAny)
        values

//      case sigmastate.Upcast(In(inputC), tpe) =>
//        val elem = stypeToElem(tpe.asNumType)
//        val res = upcast(inputC.value)(elem)
//        withConstantSize(res, opCost(res, Array(inputC.cost), costOf(node)))
//
//      case sigmastate.Downcast(In(inputC), tpe) =>
//        val elem = stypeToElem(tpe.asNumType)
//        val res = downcast(inputC.value)(elem)
//        withConstantSize(res, opCost(res, Array(inputC.cost), costOf(node)))
//
//      case ByteArrayToLong(In(arr)) =>
//        val arrC = asRep[Costed[Coll[Byte]]](arr)
//        val value = sigmaDslBuilder.byteArrayToLong(arrC.value)
//        val cost = opCost(value, Array(arrC.cost), costOf(node))
//        RCCostedPrim(value, cost, SizeLong)
//
//      case Xor(InCollByte(l), InCollByte(r)) =>
//        val values = colBuilder.xor(l.value, r.value)
//        val sizes = r.sizes
//        val len = sizes.length
//        val costs = colBuilder.replicate(len, IntZero)
//        val cost = opCost(values, Array(l.cost, r.cost), perKbCostOf(node, len.toLong))
//        RCCostedColl(values, costs, sizes, cost)
//
//      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
//        val values = sigmaDslBuilder.substConstants(bytes.values, positions.values, newValues.values)
//        val len = bytes.size.dataSize + newValues.size.dataSize
//        val cost = opCost(values, Array(bytes.cost, positions.cost, newValues.cost), perKbCostOf(node, len))
//        mkCostedColl(values, len.toInt, cost)
//
//      case DecodePoint(InCollByte(bytes)) =>
//        val res = sigmaDslBuilder.decodePoint(bytes.values)
//        RCCostedPrim(res, opCost(res, Array(bytes.cost), costOf(node)), SizeGroupElement)
//
//      // fallback rule for MethodCall, should be the last case in the list
//      case Terms.MethodCall(obj, method, args, typeSubst) if method.objType.coster.isDefined =>
//        val objC = eval(obj)
//        val argsC: Seq[RCosted[SType#WrappedType]] =
//          if (args.isEmpty)
//            EmptySeqOfSym.asInstanceOf[Seq[RCosted[SType#WrappedType]]]
//          else {
//            val len = args.length
//            val res = new Array[RCosted[SType#WrappedType]](len)
//            cfor(0)(_ < len, _ + 1) { i =>
//              res(i) = eval(args(i))
//            }
//            res
//          }
//        val elems: Seq[Sym] =
//          if (typeSubst.isEmpty)
//            EmptySeqOfSym
//          else {
//            val ts = typeSubst.values.toArray
//            val len = ts.length
//            val res = new Array[Sym](len)
//            cfor(0)(_ < len, _ + 1) { i =>
//              res(i) = liftElem(stypeToElem(ts(i)).asInstanceOf[Elem[Any]])
//            }
//            res
//          }
//        method.objType.coster.get(IR)(objC, method, argsC, elems)

      case _ =>
        error(s"Don't know how to buildNode($node)", node.sourceContext.toOption)
    }
    val resC = asRep[T#WrappedType](res)
    resC
  }
}
