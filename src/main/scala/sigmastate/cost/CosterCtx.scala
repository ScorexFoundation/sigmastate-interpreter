package sigmastate.lang

import com.sun.org.apache.xml.internal.serializer.ToUnknownStream
import org.bouncycastle.math.ec.ECPoint

import scalan.{Lazy, SigmaLibrary}
import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values.Value.Typed
import sigmastate._
import sigmastate.Values.{OptionValue, Constant, SValue, Value, ByteArrayConstant, TaggedVariableNode, SigmaBoolean, ConcreteCollection}
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait CosterCtx extends SigmaLibrary {
  import Context._;
  import WArray._;
  import Col._;
  import ColBuilder._;
  import Sigma._;
  import Box._
  import ColOverArrayBuilder._;
  import ConcreteCostedBuilder._
  import Costed._;
  import CostedPrim._;
  import ProveDlogEvidence._
  import SigmaDslBuilder._

  override def rewriteDef[T](d: Def[T]): Rep[_] = {
    val CBM = ColBuilderMethods
    d match {
      case ThunkForce(Def(ThunkDef(root, sch))) if sch.map(_.sym) == Seq(root) => root
//      case CBM.apply_apply_items(b, arr: mutable.WrappedArray[_]) =>
//        val items = arr.iterator.toIndexedSeq
//        b.apply(items:_*)
      case _ => super.rewriteDef(d)
    }
  }
  
  var defCounter = 0
  var defTime: Long = 0
  override def def_unapply[T](e: Rep[T]) = {
    defCounter += 1
    val start = System.nanoTime()
    val res = super.def_unapply(e)
    val end = System.nanoTime()
    defTime += (end - start)
    res
  }

  /** Should be specified in the final cake */
  val builder: sigmastate.lang.SigmaBuilder
  import builder._

  val colBuilder: Rep[ColBuilder] = RColOverArrayBuilder()
  val costedBuilder = RConcreteCostedBuilder()
  import Cost._

  def byteSize[T](eT: BaseElem[T]): Int = eT match {
    case BooleanElement => 1
    case ByteElement => 1
    case CharElement => 2
    case ShortElement => 2
    case IntElement => 4
    case LongElement => 8
  }

  def split[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, T => Int)] = {
    implicit val eT = f.elem.eDom
    val calc = fun { x: Rep[T] =>
      val y = f(x);
      y.value
    }
    val cost = fun { x: Rep[T] => f(x).cost }
    Pair(calc, cost)
  }

  def stypeToElem[T <: SType](t: T): Elem[T#WrappedType] = (t match {
    case SByte => ByteElement
    case SShort => ShortElement
    case SInt => IntElement
    case SLong => LongElement
    case SBox => boxElement
    case SGroupElement => EcPointElement
    case SSigmaProp => sigmaElement
    case c: SCollection[a] => colElement(stypeToElem(c.elemType))
    case _ => error(s"Don't know how to convert SType $t to Elem")
  }).asElem[T#WrappedType]

  private val elemToNumericMap = Map[Elem[_], Numeric[_]](
    (ByteElement, numeric[Byte]),
    (ShortElement, numeric[Short]),
    (IntElement, numeric[Int]),
    (LongElement, numeric[Long])
  )
  private val elemToIntegralMap = Map[Elem[_], Integral[_]](
    (ByteElement, integral[Byte]),
    (ShortElement, integral[Short]),
    (IntElement, integral[Int]),
    (LongElement, integral[Long])
  )
  private val elemToOrderingMap = Map[Elem[_], Ordering[_]](
    (ByteElement, implicitly[Ordering[Byte]]),
    (ShortElement, implicitly[Ordering[Short]]),
    (IntElement, implicitly[Ordering[Int]]),
    (LongElement, implicitly[Ordering[Long]])
  )

  def elemToNumeric [T](e: Elem[T]): Numeric[T]  = elemToNumericMap(e).asInstanceOf[Numeric[T]]
  def elemToIntegral[T](e: Elem[T]): Integral[T] = elemToIntegralMap(e).asInstanceOf[Integral[T]]
  def elemToOrdering[T](e: Elem[T]): Ordering[T] = elemToOrderingMap(e).asInstanceOf[Ordering[T]]

  def opcodeToEndoBinOp[T](opCode: Byte, eT: Elem[T]): EndoBinOp[T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToNumeric(eT))(eT)
    case OpCodes.MinusCode => NumericMinus(elemToNumeric(eT))(eT)
    case OpCodes.MultiplyCode => NumericTimes(elemToNumeric(eT))(eT)
    case OpCodes.DivisionCode => IntegralDivide(elemToIntegral(eT))(eT)
    case OpCodes.ModuloCode => IntegralMod(elemToIntegral(eT))(eT)
    case _ => error(s"Cannot find EndoBinOp for opcode $opCode")
  }

  def opcodeToBinOp[A](opCode: Byte, eA: Elem[A]): BinOp[A,_] = opCode match {
    case OpCodes.EqCode => Equals[A]()
    case OpCodes.GtCode => OrderingGT[A](elemToOrdering(eA))
    case OpCodes.LtCode => OrderingLT[A](elemToOrdering(eA))
    case OpCodes.GeCode => OrderingGTEQ[A](elemToOrdering(eA))
    case OpCodes.LeCode => OrderingLTEQ[A](elemToOrdering(eA))
    case _ => error(s"Cannot find BinOp for opcode $opCode")
  }

  type RCosted[A] = Rep[Costed[A]]

  def evalBlock[T](block: => Rep[T]): Rep[T] = {
    val th = Thunk(block)
    th.force
//    th match {
//      case Def(ThunkDef(root, schedule)) =>
//        val items = schedule.map(_.sym)
//        val res = semicolonMulti(items, root)
//        res
//      case _ => !!!("Don't know how to process Thunk", th)
//    }
//    val Def(v: ThunkView[Any, T]@unchecked) = th
//    val Def(ThunkDef(root, schedule)) = v.source
//    val items = schedule.map(_.sym)
//    val res = semicolonMulti(items, v.innerIso.to(root))
//    res
  }

  def evalCostedBlock[T](block: => Rep[Costed[T]]): Rep[Costed[T]] = {
    val v = evalBlock {
      val costed = block
      costed.value
    }
    val c = evalBlock {
      val costed = block
      costed.cost
    }
    RCostedPrim(v, c)
  }

  type Env = Map[String, SValue]

  class SpecializationRule(env: Env) {
    def unapply(e: SValue): Option[(SValue, Env)] = {
      val (optRes, env1) = specializationRule(env, e)
      optRes.map((_, env1))
    }
  }

  def specializationRule(env: Env, e: SValue): (Option[SValue], Env) = {
    implicit val sameEnv = env
    def some[T](v: T)(implicit env: Env): (Option[T], Env) = (Some(v), env)
    e match {
      case Ident(n, _) => (env.get(n), env)

      // Rule: allOf(arr) --> AND(arr)
      case Terms.Apply(AllSym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
        some(mkAND(arr))

      // Rule: anyOf(arr) --> OR(arr)
      case Terms.Apply(AnySym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
        some(mkOR(arr))

      case Terms.Apply(Blake2b256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
        some(mkCalcBlake2b256(arg))

      case Terms.Apply(Sha256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
        some(mkCalcSha256(arg))

      case Terms.Apply(IsMemberSym, Seq(tree: Value[SAvlTree.type]@unchecked, key: Value[SByteArray]@unchecked, proof: Value[SByteArray]@unchecked)) =>
        some(mkIsMember(tree, key, proof))

      case Terms.Apply(ProveDlogSym, Seq(g: Value[SGroupElement.type]@unchecked)) =>
        some(mkProveDlog(g))

      case Terms.Apply(IntToByteSym, Seq(arg: Value[SInt.type]@unchecked)) =>
        some(mkIntToByte(arg))

      case Terms.Apply(LongToByteArraySym, Seq(arg: Value[SLong.type]@unchecked)) =>
        some(mkLongToByteArray(arg))

      case Upcast(Constant(value, tpe), toTpe: SNumericType) =>
        some(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

      // Rule: col.size --> SizeOf(col)
      case Select(obj, "size", _) =>
        if (obj.tpe.isCollectionLike)
          some(mkSizeOf(obj.asValue[SCollection[SType]]))
        else
          error(s"The type of $obj is expected to be Collection to select 'size' property")

      // Rule: proof.isValid --> IsValid(proof)
      case Select(p, SSigmaProp.IsValid, _) if p.tpe == SSigmaProp =>
        some(SigmaPropIsValid(p.asSigmaProp))

      // Rule: proof.propBytes --> ProofBytes(proof)
      case Select(p, SSigmaProp.PropBytes, _) if p.tpe == SSigmaProp =>
        some(SigmaPropBytes(p.asSigmaProp))

      case sel @ Terms.Apply(Select(Select(Typed(box, SBox), regName, _), "valueOrElse", Some(_)), Seq(arg)) =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        some(mkExtractRegisterAs(box.asBox, reg, arg.tpe, Some(arg)))

      case sel @ Select(Select(Typed(box, SBox), regName, _), "value", Some(regType)) =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        some(mkExtractRegisterAs(box.asBox, reg, regType, None))

      case sel @ Select(obj, field, _) if obj.tpe == SBox =>
        (obj.asValue[SBox.type], field) match {
          case (box, SBox.Value) => some(mkExtractAmount(box))
          case (box, SBox.PropositionBytes) => some(mkExtractScriptBytes(box))
          case (box, SBox.Id) => some(mkExtractId(box))
          case (box, SBox.Bytes) => some(mkExtractBytes(box))
          case (box, SBox.BytesWithNoRef) => some(mkExtractBytesWithNoRef(box))
          case (box, _) if box.tpe.hasMethod(field) =>
            (None, env)  // leave it as it is and handle on a level of parent node
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
        }

      case Select(obj: SigmaBoolean, field, _) =>
        field match {
          case SigmaBoolean.PropBytes => some(ByteArrayConstant(obj.bytes))
          case SigmaBoolean.IsValid => some(obj)
        }

      case Select(obj, "value", Some(SLong)) if obj.tpe == SBox =>
        some(mkExtractAmount(obj.asValue[SBox.type]))

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        some(mkSelectField(tuple.asTuple, index))

      case Terms.Apply(Select(col, "slice", _), Seq(from, until)) =>
        some(mkSlice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

      case Terms.Apply(Select(col, "where", _), Seq(Terms.Lambda(Seq((n, t)), _, Some(body)))) =>
        val tagged = mkTaggedVariable(21, t)
        val env1 = env + (n -> tagged)
        some(mkWhere(col.asValue[SCollection[SType]], tagged.varId, body.asValue[SBoolean.type]))(env1)

      case Terms.Apply(Select(col,"exists", _), Seq(Terms.Lambda(Seq((n, t)), _, Some(body)))) =>
        val tagged = mkTaggedVariable(21, t)
        val env1 = env + (n -> tagged)
        some(mkExists(col.asValue[SCollection[SType]], tagged.varId, body.asValue[SBoolean.type]))(env1)

      case Terms.Apply(Select(col,"forall", _), Seq(Terms.Lambda(Seq((n, t)), _, Some(body)))) =>
        val tagged = mkTaggedVariable(21, t)
        val env1 = env + (n -> tagged)
        some(mkForAll(col.asValue[SCollection[SType]], tagged.varId, body.asValue[SBoolean.type]))(env1)

      case Terms.Apply(Select(col,"map", _), Seq(Terms.Lambda(Seq((n, t)), _, Some(body)))) =>
        val tagged = mkTaggedVariable(21, t)
        val env1 = env + (n -> tagged)
        some(mkMapCollection(col.asValue[SCollection[SType]], tagged.varId, body))(env1)

      case Terms.Apply(Select(col,"fold", _), Seq(zero, Terms.Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
        val taggedZero = mkTaggedVariable(21, tZero)
        val taggedOp = mkTaggedVariable(22, tOp)
        val env1 = env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp)
        some(mkFold(col.asValue[SCollection[SType]], taggedZero.varId, zero, taggedOp.varId, body))(env1)

      case Terms.Apply(Select(col,"getOrElse", _), Seq(index, defaultValue)) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        some(mkByIndex(col.asValue[SCollection[SType]], index1, Some(defaultValue1)))

      case opt: OptionValue[_] =>
        error(s"Option constructors are not supported: $opt")

      case AND(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[AND]) =>
        some(mkAND(
          mkConcreteCollection(
            items.flatMap {
              case AND(ConcreteCollection(innerItems, SBoolean)) => innerItems
              case v => IndexedSeq(v)
            }, SBoolean)))

      case OR(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[OR]) =>
        some(mkOR(
          mkConcreteCollection(
            items.flatMap {
              case OR(ConcreteCollection(innerItems, SBoolean)) => innerItems
              case v => IndexedSeq(v)
            }, SBoolean)))
      case _ => (None, env)
    }
  }

  private def evalNode[T <: SType](ctx: Rep[Context], env: Map[String, SValue], vars: Map[Byte, Rep[_]], node: Value[T]): RCosted[T#WrappedType] = {
    import MonoidBuilderInst._; import WOption._; import WSpecialPredef._
    val SR = new SpecializationRule(env)
    val res: Rep[Any] = node match {
//      case _ @ Block(binds, res) =>
//        var curEnv = env
//        for (Let(n, _, b) <- binds) {
//          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
//          val b1 = eval(curEnv, b)
//          curEnv = curEnv + (n -> b1)
//        }
//        val res1 = eval(curEnv, res)
//        Some(res1)

      case Constant(v, tpe) => v match {
        case p: scapi.sigma.DLogProtocol.ProveDlog =>
          val ge = evalNode(ctx, env, vars, p.value).asRep[Costed[WECPoint]]
          CostedPrimRep(RProveDlogEvidence(ge.value), ge.cost + Dlog)
        case ge: CryptoConstants.EcPointType =>
          assert(tpe == SGroupElement)
          val ge1 = toRep(ge)(stypeToElem(SGroupElement)).asRep[ECPoint]
          CostedPrimRep(WEcPointNew(ge1), ConstantNode)
        case _ =>
          CostedPrimRep(toRep(v)(stypeToElem(tpe)), ConstantNode)
      }
      case Height => CostedPrimRep(ctx.HEIGHT, HeightAccess)
      case Inputs => CostedPrimRep(ctx.INPUTS, InputsAccess)
      case Outputs => CostedPrimRep(ctx.OUTPUTS, OutputsAccess)
      case Self => CostedPrimRep(ctx.SELF, SelfAccess)
      case TaggedVariableNode(id, tpe) =>
        if (id >= 21) {
          vars.get(id) match {
            case Some(x: RCosted[a]@unchecked) => CostedPrimRep(x.value, x.cost + VariableAccess)
            case _ => !!!(s"Variable $node not found in environment $vars")
          }
        } else {
          CostedPrimRep(ctx.getVar(id)(stypeToElem(tpe)), VariableAccess)
        }
      case SizeOf(xs) =>
        val xsC = evalNode(ctx, env, vars, xs).asRep[Costed[Col[Any]]]
        CostedPrimRep(xsC.value.length, xsC.cost + SizeOfDeclaration)
      case SigmaPropIsValid(p) =>
        val pC = evalNode(ctx, env, vars, p).asRep[Costed[Sigma]]
        CostedPrimRep(pC.value.isValid, pC.cost + SigmaPropIsValidDeclaration)
      case SigmaPropBytes(p) =>
        val pC = evalNode(ctx, env, vars, p).asRep[Costed[Sigma]]
        CostedPrimRep(pC.value.propBytes, pC.cost + SigmaPropBytesDeclaration + pC.value.propBytes.length)
      case utxo.ExtractAmount(box) =>
        val boxC = evalNode(ctx, env, vars, box).asRep[Costed[Box]]
        CostedPrimRep(boxC.value.value, boxC.cost + Cost.ExtractAmount)
      case utxo.ExtractScriptBytes(box) =>
        val boxC = evalNode(ctx, env, vars, box).asRep[Costed[Box]]
        val bytes = boxC.value.propositionBytes
        CostedPrimRep(bytes, boxC.cost + Cost.ExtractScriptBytes + bytes.length)
      case utxo.ExtractRegisterAs(box, regId, tpe, default) =>
        val boxC = evalNode(ctx, env, vars, box).asRep[Costed[Box]]
        val elem = stypeToElem(tpe)
        val valueOpt = boxC.value.getReg(regId.number.toInt)(elem)
        val baseCost = boxC.cost + Cost.ExtractRegister
        val (v, c) = if (default.isDefined) {
          val d = evalNode(ctx, env, vars, default.get)
          (RWSpecialPredef.optionGetOrElse(valueOpt, d.value), baseCost + d.cost)
        } else {
          (valueOpt.get, baseCost)
        }
        CostedPrimRep(v, c)
      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(ctx, env, vars, op.left)
        val y = evalNode(ctx, env, vars, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(ApplyBinOp(binop, x.value, y.value), x.cost + y.cost + TripleDeclaration)
        }
      case OR(input) => input.matchCase(
        cc => {
          val headC = evalNode(ctx, env, vars, cc.items(0))
          val tailC = cc.items.iterator.drop(1).map(x => evalCostedBlock(evalNode(ctx, env, vars, x)))
          val itemsC = (Iterator.single(headC) ++ tailC).toIndexedSeq
          val res = sigmaDslBuilder.anyOf(colBuilder.apply(new mutable.WrappedArray.ofRef(itemsC.map(_.value).toArray):_*))
          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + OrDeclaration
          CostedPrimRep(res, cost)
        },
        const => ???,
        tup => ???
      )
      case AND(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(ctx, env, vars, _))
          val res = sigmaDslBuilder.allOf(colBuilder.apply(itemsC.map(_.value): _*))
          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + AndDeclaration
          CostedPrimRep(res, cost)
        },
        const => ???,
        tup => ???
      )
      case rel: Relation[t, _] =>
        val tpe = rel.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(rel.opCode, et)
        val x = evalNode(ctx, env, vars, rel.left)
        val y = evalNode(ctx, env, vars, rel.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(
            binop.apply(x.value, y.value.asRep[t#WrappedType]),
            x.cost + y.cost + TripleDeclaration
          )
        }

      case MapCollection(input, id, mapper) =>
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, vars, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val Pair(mapperCalc, mapperCost) = split(fun { x: Rep[Any] =>
          evalNode(ctx, env, vars + (id -> CostedPrimRep(x, 0)), mapper)
        })
        val res = inputC.value.map(mapperCalc)
        val cost = inputC.cost + inputC.value.map(mapperCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(res, cost)

      case Where(input, id, cond) =>
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, vars, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val Pair(condCalc, condCost) = split(fun { x: Rep[Any] =>
          evalNode(ctx, env, vars + (id -> CostedPrimRep(x, 0)), cond)
        })
        val res = inputC.value.filter(condCalc)
        val cost = inputC.cost + inputC.value.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(res, cost)

      case trans: BooleanTransformer[_] =>
        val eItem = stypeToElem(trans.input.tpe.elemType)
        val inputC = evalNode(ctx, env, vars, trans.input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eItem == eAny, s"Types should be equal: but $eItem != $eAny")
        val Pair(condCalc, condCost) = split(fun { x: Rep[Any] =>
          evalNode(ctx, env, vars + (trans.id -> CostedPrimRep(x, 0)), trans.condition)
        })
        val value = trans.opCode match {
          case OpCodes.ExistsCode =>
            inputC.value.exists(condCalc)
          case OpCodes.ForAllCode =>
            inputC.value.forall(condCalc)
        }
        val cost = inputC.cost + inputC.value.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(value, cost)

      case SR(v, env1) => evalNode(ctx, env1, vars, v)

      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    res.asRep[Costed[T#WrappedType]]
  }

  def buildCostedGraph[T <: SType](env: Map[String, SValue], ctxVars: Map[Byte, SValue], tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = {
    fun { ctx: Rep[Context] =>
      val vars = ctxVars.mapValues(v => evalNode(ctx, env, Map(), v))
      evalNode(ctx, env, vars, tree)
    }
  }

  def error(msg: String) = throw new CosterException(msg, None)
}
