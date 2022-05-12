package sigmastate.eval

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules.{CheckIsSupportedIndexExpression, CheckTupleType}
import scalan.{ExactOrdering, Lazy, MutableLazy, Nullable, SigmaLibrary}
import scalan.ExactOrdering.{ByteIsExactOrdering, IntIsExactOrdering, LongIsExactOrdering, ShortIsExactOrdering}
import sigmastate.Values.Value.Typed
import sigmastate.{AND, ArithOp, AtLeast, BinAnd, BinOr, BinXor, BoolToSigmaProp, ByteArrayToLong, CalcBlake2b256, CalcSha256, CreateProveDHTuple, CreateProveDlog, DecodePoint, If, LogicalNot, ModQ, ModQArithOp, Negation, OR, Relation, SAvlTree, SBigInt, SBox, SCollection, SCollectionType, SContext, SGlobal, SGroupElement, SInt, SNumericType, SOption, SSigmaProp, SType, SigmaAnd, SigmaOr, SubstConstants, Values, Xor, XorOf, utxo}
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

  private lazy val elemToExactOrderingMap = Map[Elem[_], ExactOrdering[_]](
    (ByteElement,   ByteIsExactOrdering),
    (ShortElement,  ShortIsExactOrdering),
    (IntElement,    IntIsExactOrdering),
    (LongElement,   LongIsExactOrdering),
    (bigIntElement, NumericOps.BigIntIsExactOrdering)
  )

  // TODO v5.x - Remove *Graph suffix when RuntimeCosting is dropped.
  def elemToExactOrderingGraph[T](e: Elem[T]): ExactOrdering[T] = elemToExactOrderingMap(e).asInstanceOf[ExactOrdering[T]]

  // TODO v5.x - Remove *Graph suffix when RuntimeCosting is dropped.
  def opcodeToEndoBinOpGraph[T](opCode: Byte, eT: Elem[T]): EndoBinOp[T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToExactNumeric(eT))(eT)
    case OpCodes.MinusCode => NumericMinus(elemToExactNumeric(eT))(eT)
    case OpCodes.MultiplyCode => NumericTimes(elemToExactNumeric(eT))(eT)
    case OpCodes.DivisionCode => IntegralDivide(elemToExactIntegral(eT))(eT)
    case OpCodes.ModuloCode => IntegralMod(elemToExactIntegral(eT))(eT)
    case OpCodes.MinCode => OrderingMin(elemToExactOrdering(eT))(eT)
    case OpCodes.MaxCode => OrderingMax(elemToExactOrdering(eT))(eT)
    case _ => error(s"Cannot find EndoBinOp for opcode $opCode")
  }

  // TODO v5.x - Remove *Graph suffix when RuntimeCosting is dropped.
  def opcodeToBinOpGraph[A](opCode: Byte, eA: Elem[A]): BinOp[A,_] = opCode match {
    case OpCodes.EqCode  => Equals[A]()(eA)
    case OpCodes.NeqCode => NotEquals[A]()(eA)
    case OpCodes.GtCode  => OrderingGT[A](elemToExactOrderingGraph(eA))
    case OpCodes.LtCode  => OrderingLT[A](elemToExactOrderingGraph(eA))
    case OpCodes.GeCode  => OrderingGTEQ[A](elemToExactOrderingGraph(eA))
    case OpCodes.LeCode  => OrderingLTEQ[A](elemToExactOrderingGraph(eA))
    case _ => error(s"Cannot find BinOp for opcode $opCode")
  }

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
    def throwError =
      error(s"Don't know how to buildNode($node)", node.sourceContext.toOption)

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

      case op @ GetVar(id, optTpe) =>
        val e = stypeToElem(optTpe.elemType)
        ctx.getVar(id)(e)

      case Terms.Block(binds, res) =>
        var curEnv = env
        for (v @ Val(n, _, b) <- binds) {
          if (curEnv.contains(n))
            error(s"Variable $n already defined ($n = ${curEnv(n)}", v.sourceContext.toOption)
          val bV = buildNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bV)
        }
        val resV = buildNode(ctx, curEnv, res)
        resV

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

      case sigmastate.Exponentiate(In(l), In(r)) =>
        val lV = asRep[GroupElement](l)
        val rV = asRep[BigInt](r)
        lV.exp(rV)

//      case sigmastate.MultiplyGroup(In(_l), In(_r)) =>
//        val l = asRep[Costed[GroupElement]](_l)
//        val r = asRep[Costed[GroupElement]](_r)
//        val value = l.value.multiply(r.value)
//        val cost = opCost(value, Array(l.cost, r.cost), costOf(node))
//        RCCostedPrim(value, cost, SizeGroupElement)
//
      case Values.GroupGenerator =>
        sigmaDslBuilder.groupGenerator

      case sigmastate.ByteArrayToBigInt(In(arr)) =>
        val arrV = asRep[Coll[Byte]](arr)
        sigmaDslBuilder.byteArrayToBigInt(arrV)

      case sigmastate.LongToByteArray(In(x)) =>
        val xV = asRep[Long](x)
        sigmaDslBuilder.longToByteArray(xV)

      // opt.get =>
      case utxo.OptionGet(In(opt: ROption[_]@unchecked)) =>
        opt.get

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

    case Values.Tuple(InSeq(Seq(x, y))) =>
      Pair(x, y)

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

    case Filter(input, p) =>
      val inputV = asRep[Coll[Any]](eval(input))
      val pV = asRep[Any => Boolean](eval(p))
      inputV.filter(pV)

    case Terms.Apply(f, Seq(x)) if f.tpe.isFunc =>
      val fV = asRep[Any => Coll[Any]](eval(f))
      val xV = asRep[Any](eval(x))
      Apply(fV, xV, mayInline = false)

//      case opt: OptionValue[_] =>
//        error(s"Option constructors are not supported: $opt", opt.sourceContext.toOption)
//
      case CalcBlake2b256(In(input)) =>
        val inputV = asRep[Coll[Byte]](input)
        val res = sigmaDslBuilder.blake2b256(inputV)
        res

      case CalcSha256(In(input)) =>
        val inputV = asRep[Coll[Byte]](input)
        val res = sigmaDslBuilder.sha256(inputV)
        res
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
     case utxo.ExtractAmount(In(box)) =>
       val boxV = asRep[Box](box)
       boxV.value
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

    case BoolToSigmaProp(bool) =>
      sigmaDslBuilder.sigmaProp(eval(bool))

    case AtLeast(bound, input) =>
      val inputV = asRep[Coll[SigmaProp]](eval(input))
      if (inputV.length.isConst) {
        val inputCount = valueFromRep(inputV.length)
        if (inputCount > AtLeast.MaxChildrenCount)
          error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount", node.sourceContext.toOption)
      }
      val boundV = eval(bound)
      sigmaDslBuilder.atLeast(boundV, inputV)

    case op: ArithOp[t] if op.tpe == SBigInt =>
      import OpCodes._
      val xV = asRep[BigInt](eval(op.left))
      val yV = asRep[BigInt](eval(op.right))
      op.opCode match {
        case PlusCode     => xV.add(yV)
        case MinusCode    => xV.subtract(yV)
        case MultiplyCode => xV.multiply(yV)
        case DivisionCode => xV.divide(yV)
        case ModuloCode   => xV.mod(yV)
        case MinCode      => xV.min(yV)
        case MaxCode      => xV.max(yV)
        case code         => error(s"Cannot perform buildNode($op): unknown opCode ${code}", op.sourceContext.toOption)
      }

    case op: ArithOp[t] =>
      val tpe = op.left.tpe
      val et = stypeToElem(tpe)
      val binop = opcodeToEndoBinOpGraph(op.opCode, et)
      val x = eval(op.left)
      val y = eval(op.right)
      ApplyBinOp(binop, x, y)

    case LogicalNot(input) =>
      val inputV = eval(input)
      ApplyUnOp(Not, inputV)

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

    case OR(input) => input match {
      case ConcreteCollection(items, _) =>
        val len = items.length
        val values = new Array[Ref[Boolean]](len)
        cfor(0)(_ < len, _ + 1) { i =>
          val item = items(i)
          values(i) = eval(item)
        }
        sigmaDslBuilder.anyOf(colBuilder.fromItems(values: _*))
      case _ =>
        val inputV = asRep[Coll[Boolean]](eval(input))
        sigmaDslBuilder.anyOf(inputV)
    }

    case AND(input) => input match {
      case ConcreteCollection(items, _) =>
        val len = items.length
        val values = new Array[Ref[Boolean]](len)
        cfor(0)(_ < len, _ + 1) { i =>
          val item = items(i)
          values(i) = eval(item)
        }
        sigmaDslBuilder.allOf(colBuilder.fromItems(values: _*))
      case _ =>
        val inputV = asRep[Coll[Boolean]](eval(input))
        sigmaDslBuilder.allOf(inputV)
    }

    case XorOf(input) => input match {
      case ConcreteCollection(items, _) =>
        val len = items.length
        val values = new Array[Ref[Boolean]](len)
        cfor(0)(_ < len, _ + 1) { i =>
          val item = items(i)
          values(i) = eval(item)
        }
        sigmaDslBuilder.xorOf(colBuilder.fromItems(values: _*))
      case _ =>
        val inputV = asRep[Coll[Boolean]](eval(input))
        sigmaDslBuilder.xorOf(inputV)
    }

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

    case BinXor(l, r) =>
      val lV = buildNode(ctx, env, l)
      val rV = buildNode(ctx, env, r)
      BinaryXorOp.apply(lV, rV)

     case neg: Negation[SNumericType]@unchecked =>
       val et = stypeToElem(neg.input.tpe)
       val op = NumericNegate(elemToExactNumeric(et))(et)
       val x = buildNode(ctx, env, neg.input)
       ApplyUnOp(op, x)

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

    case rel: Relation[t, _] =>
      val tpe = rel.left.tpe
      val et = stypeToElem(tpe)
      val binop = opcodeToBinOpGraph(rel.opCode, et)
      val x = eval(rel.left)
      val y = eval(rel.right)
      binop.apply(x, asRep[t#WrappedType](y))

    case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
      val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
      val f = fun { x: Ref[Any] =>
        buildNode(ctx, env + (n -> x), body)
      }(Lazy(eArg))
      f

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

      case sigmastate.Upcast(In(input), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        upcast(input)(elem)

      case sigmastate.Downcast(In(input), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        downcast(input)(elem)

      case ByteArrayToLong(In(arr)) =>
        val coll = asRep[Coll[Byte]](arr)
        sigmaDslBuilder.byteArrayToLong(coll)

      case Xor(InCollByte(l), InCollByte(r)) =>
        colBuilder.xor(l, r)

//      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
//        val values = sigmaDslBuilder.substConstants(bytes.values, positions.values, newValues.values)
//        val len = bytes.size.dataSize + newValues.size.dataSize
//        val cost = opCost(values, Array(bytes.cost, positions.cost, newValues.cost), perKbCostOf(node, len))
//        mkCostedColl(values, len.toInt, cost)

      case DecodePoint(InCollByte(bytes)) =>
        sigmaDslBuilder.decodePoint(bytes)

      // fallback rule for MethodCall, should be the last case in the list
      case Terms.MethodCall(obj, method, args, typeSubst) if method.objType.coster.isDefined =>
        val objV = eval(obj)
        val argsV = args.map(eval)
        (objV, method.objType) match {
          case (xs: RColl[t]@unchecked, SCollection) => method.name match {
            case SCollection.IndicesMethod.name =>
              xs.indices
            case SCollection.PatchMethod.name =>
              val from = asRep[Int](argsV(0))
              val patch = asRep[Coll[t]](argsV(1))
              val replaced = asRep[Int](argsV(2))
              xs.patch(from, patch, replaced)
            case SCollection.UpdatedMethod.name =>
              val index = asRep[Int](argsV(0))
              val value = asRep[t](argsV(1))
              xs.updated(index, value)
            case SCollection.UpdateManyMethod.name =>
              val indexes = asRep[Coll[Int]](argsV(0))
              val values = asRep[Coll[t]](argsV(1))
              xs.updateMany(indexes, values)
            case SCollection.IndexOfMethod.name =>
              val elem = asRep[t](argsV(0))
              val from = asRep[Int](argsV(1))
              xs.indexOf(elem, from)
            case SCollection.ZipMethod.name =>
              val ys = asRep[Coll[Any]](argsV(0))
              xs.zip(ys)
            case _ => throwError
          }
          case (opt: ROption[t]@unchecked, SOption) => method.name match {
            case SOption.MapMethod.name =>
              opt.map(asRep[t => Any](argsV(0)))
            case SOption.FilterMethod.name =>
              opt.filter(asRep[t => Boolean](argsV(0)))
            case _ => throwError
          }
          case (box: Ref[Box]@unchecked, SBox) => method.name match {
            case SBox.tokensMethod.name =>
              box.tokens
            case _ => throwError
          }
          case (ctx: Ref[Context]@unchecked, SContext) => method.name match {
            case SContext.dataInputsMethod.name =>
              asRep[Context](objV).dataInputs
            case _ => throwError
          }
          case (tree: Ref[AvlTree]@unchecked, SAvlTree) => method.name match {
            case SAvlTree.digestMethod.name =>
              tree.digest
            case _ => throwError
          }
          case _ => throwError
        }

      case _ =>
        throwError
    }
    val resC = asRep[T#WrappedType](res)
    resC
  }

}
