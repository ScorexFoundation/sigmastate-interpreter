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
import sigmastate.utxo.{Append, BooleanTransformer, ByIndex, CostTable, Exists, Filter, Fold, ForAll, GetVar, MapCollection, SelectField, SigmaPropBytes, SigmaPropIsProven, Slice}
import spire.syntax.all.cfor

import scala.collection.mutable

trait GraphBuilding extends SigmaLibrary { IR: IRContext =>
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
    fun(removeIsProven({ ctxC: Ref[Context] =>
      val env = envVals.mapValues(v => buildNode(ctxC, Map.empty, v))
      val res = asRep[T](buildNode(ctxC, env, tree))
      res
    }))
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
      case CreateProveDlog(In(_v)) =>
        val v = asRep[GroupElement](_v)
        sigmaDslBuilder.proveDlog(v)

      case CreateProveDHTuple(In(_gv), In(_hv), In(_uv), In(_vv)) =>
        val gv = asRep[GroupElement](_gv)
        val hv = asRep[GroupElement](_hv)
        val uv = asRep[GroupElement](_uv)
        val vv = asRep[GroupElement](_vv)
        sigmaDslBuilder.proveDHTuple(gv, hv, uv, vv)

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

      // opt.isDefined =>
      case utxo.OptionIsDefined(In(opt: ROption[_]@unchecked)) =>
        opt.isDefined

      // opt.getOrElse =>
      case utxo.OptionGetOrElse(In(opt: ROption[a]@unchecked), In(default)) =>
        opt.getOrElse(asRep[a](default))

      case SelectField(In(tup), fieldIndex) =>
        val eTuple = tup.elem.asInstanceOf[Elem[_]]
        CheckTupleType(IR)(eTuple)
        eTuple match {
          case pe: PairElem[a,b] =>
            assert(fieldIndex == 1 || fieldIndex == 2, s"Invalid field index $fieldIndex of the pair ${tup}: $pe")
            implicit val ea = pe.eFst
            implicit val eb = pe.eSnd
            val pair = asRep[(a,b)](tup)
            val res = if (fieldIndex == 1) pair._1 else pair._2
            res
        }

      case Values.Tuple(InSeq(Seq(x, y))) =>
        Pair(x, y)

      case node: BooleanTransformer[_] =>
        val tpeIn = node.input.tpe.elemType
        val eIn = stypeToElem(tpeIn)
        val xs = asRep[Coll[Any]](eval(node.input))
        val eAny = xs.elem.asInstanceOf[CollElem[Any,_]].eItem
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val conditionC = asRep[Any => SType#WrappedType](eval(node.condition))
        val res = conditionC.elem.eRange.asInstanceOf[Elem[_]] match {
          case BooleanElement =>
            node match {
              case _: ForAll[_] =>
                xs.forall(asRep[Any => Boolean](conditionC))
              case _: Exists[_] =>
                xs.exists(asRep[Any => Boolean](conditionC))
            }
          case _: SigmaPropElem[_] =>
            val children = xs.map(asRep[Any => SigmaProp](conditionC))
            val size = SizeSigmaProposition
            node match {
              case _: ForAll[_] =>
                sigmaDslBuilder.allZK(children)
              case _: Exists[_] =>
                sigmaDslBuilder.anyZK(children)
            }
        }
        res

      case MapCollection(InCollAny(inputV), sfunc) =>
        val mapper = asRep[Any => Any](eval(sfunc))
        inputV.map(mapper)

      case Fold(input, zero, sfunc) =>
        val eItem = stypeToElem(input.tpe.elemType)
        val eState = stypeToElem(zero.tpe)
        (eState, eItem) match { case (eState: Elem[s], eItem: Elem[a]) =>
          val inputV = asRep[Coll[a]](eval(input))
          implicit val eA = inputV.elem.asInstanceOf[CollElem[a,_]].eItem
          assert(eItem == eA, s"Types should be equal: but $eItem != $eA")

          val zeroV = asRep[s](eval(zero))
          implicit val eS = zeroV.elem
          assert(eState == eS, s"Types should be equal: but $eState != $eS")

          val op = asRep[((s,a)) => s](eval(sfunc))
          val res = inputV.foldLeft(zeroV, op)
          res
        }

      case op @ Slice(InCollAny(inputV), In(from), In(until)) =>
        val fromV = asRep[Int](from)
        val untilV = asRep[Int](until)
        inputV.slice(fromV, untilV)

      case Append(InCollAny(col1), InCollAny(col2)) =>
        col1.append(col2)

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

      case utxo.SizeOf(In(xs)) =>
        xs.elem.asInstanceOf[Any] match {
          case ce: CollElem[a,_] =>
            val xsV = asRep[Coll[a]](xs)
            xsV.length
          case pe: PairElem[a,b] =>
            val v: Ref[Int] = 2
            v
        }

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

      case SigmaPropIsProven(p) =>
        val pV = asRep[SigmaProp](eval(p))
        pV.isValid

      case SigmaPropBytes(p) =>
        val pV = asRep[SigmaProp](eval(p))
        pV.propBytes

      case utxo.ExtractId(In(box: Ref[Box]@unchecked)) =>
        box.id

      case utxo.ExtractBytesWithNoRef(In(box: Ref[Box]@unchecked)) =>
        box.bytesWithoutRef

      case utxo.ExtractAmount(In(box)) =>
        val boxV = asRep[Box](box)
        boxV.value

      case utxo.ExtractScriptBytes(In(box: Ref[Box]@unchecked)) =>
        box.propositionBytes

      case utxo.ExtractBytes(In(box: Ref[Box]@unchecked)) =>
        box.bytes

      case utxo.ExtractCreationInfo(In(box: Ref[Box]@unchecked)) =>
        box.creationInfo

      case utxo.ExtractRegisterAs(In(box: Ref[Box]@unchecked), regId, optTpe) =>
        val elem = stypeToElem(optTpe.elemType).asInstanceOf[Elem[Any]]
        val i: Ref[Int] = regId.number.toInt
        box.getReg(i)(elem)

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
        val binop = opcodeToEndoBinOp(op.opCode, et)
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

      case BinOr(l, r) =>
        val lV = eval(l)
        val rV = Thunk(eval(r))
        Or.applyLazy(lV, rV)

      case BinAnd(l, r) =>
        val lV = eval(l)
        val rV = Thunk(eval(r))
        And.applyLazy(lV, rV)

      case BinXor(l, r) =>
        val lV = eval(l)
        val rV = eval(r)
        BinaryXorOp.apply(lV, rV)

      case neg: Negation[SNumericType]@unchecked =>
        val et = stypeToElem(neg.input.tpe)
        val op = NumericNegate(elemToExactNumeric(et))(et)
        val x = buildNode(ctx, env, neg.input)
        ApplyUnOp(op, x)

      case SigmaAnd(items) =>
        val itemsV = items.map(item => asRep[SigmaProp](eval(item)))
        sigmaDslBuilder.allZK(colBuilder.fromItems(itemsV: _*))

      case SigmaOr(items) =>
        val itemsV = items.map(item => asRep[SigmaProp](eval(item)))
        sigmaDslBuilder.anyZK(colBuilder.fromItems(itemsV: _*))
        
      case If(c, t, e) =>
        val cV = eval(c)
        def tV = eval(t)
        def eV = eval(e)
        val resV = IF (cV) THEN tV ELSE eV
        resV

      case rel: Relation[t, _] =>
        val tpe = rel.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(rel.opCode, et)
        val x = eval(rel.left)
        val y = eval(rel.right)
        binop.apply(x, asRep[t#WrappedType](y))

      case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
        val f = fun { x: Ref[Any] =>
          buildNode(ctx, env + (n -> x), body)
        }(Lazy(eArg))
        f

      case Terms.Lambda(_, Seq((accN, accTpe), (n, tpe)), _, Some(body)) =>
        (stypeToElem(accTpe), stypeToElem(tpe)) match { case (eAcc: Elem[s], eA: Elem[a]) =>
          val eArg = pairElement(eAcc, eA)
          val f = fun { x: Ref[(s, a)] =>
            buildNode(ctx, env + (accN -> x._1) + (n -> x._2), body)
          }(Lazy(eArg))
          f
        }

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

      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
        sigmaDslBuilder.substConstants(bytes, positions, newValues)

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
            case SCollection.FlatMapMethod.name =>
              val ys = asRep[Any => Coll[Any]](argsV(0))
              xs.flatMap(ys)
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
            case SAvlTree.keyLengthMethod.name =>
              tree.keyLength
            case SAvlTree.valueLengthOptMethod.name =>
              tree.valueLengthOpt
            case SAvlTree.enabledOperationsMethod.name =>
              tree.enabledOperations
            case SAvlTree.getMethod.name =>
              val key = asRep[Coll[Byte]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.get(key, proof)
            case SAvlTree.getManyMethod.name =>
              val keys = asRep[Coll[Coll[Byte]]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.getMany(keys, proof)
            case SAvlTree.containsMethod.name =>
              val key = asRep[Coll[Byte]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.contains(key, proof)
            case SAvlTree.insertMethod.name =>
              val operations = asRep[Coll[(Coll[Byte], Coll[Byte])]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.insert(operations, proof)
            case SAvlTree.removeMethod.name =>
              val operations = asRep[Coll[Coll[Byte]]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.remove(operations, proof)
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
