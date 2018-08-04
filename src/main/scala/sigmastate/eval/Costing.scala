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

trait Costing extends SigmaLibrary {
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
  lazy val compiler = new SigmaCompiler(builder)

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

  def evalCostedBlock[T](block: => Rep[Costed[T]]): Rep[Costed[T]] = {
    val v = Thunk.forced {
      val costed = block
      costed.value
    }
    val c = Thunk.forced {
      val costed = block
      costed.cost
    }
    RCostedPrim(v, c)
  }

  type Env = Map[String, Sym]

  private def evalNode[T <: SType](ctx: Rep[Context], env: Map[String, RCosted[_]], node: Value[T]): RCosted[T#WrappedType] = {
    import MonoidBuilderInst._; import WOption._; import WSpecialPredef._
    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    val res: Rep[Any] = node match {
      case Ident(n, _) =>
        env.getOrElse(n, !!!(s"Variable $n not found in environment $env"))

      case Constant(v, tpe) => v match {
        case p: scapi.sigma.DLogProtocol.ProveDlog =>
          val ge = evalNode(ctx, env, p.value).asRep[Costed[WECPoint]]
          CostedPrimRep(RProveDlogEvidence(ge.value), ge.cost + DlogDeclaration)
        case ge: CryptoConstants.EcPointType =>
          assert(tpe == SGroupElement)
          val ge1 = toRep(ge)(stypeToElem(SGroupElement)).asRep[ECPoint]
          CostedPrimRep(WEcPointNew(ge1), ConstantNode)
//        case arr: Array[_] =>
//          CostedPrimRep(colBuilder.fromArray(Const(arr)()), arr.length)
        case _ =>
          CostedPrimRep(toRep(v)(stypeToElem(tpe)), ConstantNode)
      }

      case Height => CostedPrimRep(ctx.HEIGHT, HeightAccess)
      case Inputs => CostedPrimRep(ctx.INPUTS, InputsAccess)
      case Outputs => CostedPrimRep(ctx.OUTPUTS, OutputsAccess)
      case Self => CostedPrimRep(ctx.SELF, SelfAccess)

      case TaggedVariableNode(id, tpe) =>
        CostedPrimRep(ctx.getVar(id)(stypeToElem(tpe)), VariableAccess)

      case Terms.Block(binds, res) =>
        var curEnv = env
        for (Let(n, _, b) <- binds) {
          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
          val bC = evalNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bC)
        }
        val res1 = evalNode(ctx, curEnv, res)
        res1

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

      case Terms.Apply(IntToByteSym, Seq(arg: Value[SInt.type]@unchecked)) =>
        eval(mkIntToByte(arg))

      case Terms.Apply(LongToByteArraySym, Seq(arg: Value[SLong.type]@unchecked)) =>
        eval(mkLongToByteArray(arg))

      case Upcast(Constant(value, tpe), toTpe: SNumericType) =>
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

      case sel @ Terms.Apply(Select(Select(Typed(box, SBox), regName, _), "valueOrElse", Some(_)), Seq(arg)) =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        eval(mkExtractRegisterAs(box.asBox, reg, arg.tpe, Some(arg)))

      case sel @ Select(Select(Typed(box, SBox), regName, _), "value", Some(regType)) =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        eval(mkExtractRegisterAs(box.asBox, reg, regType, None))

      case sel @ Select(obj, field, _) if obj.tpe == SBox =>
        (obj.asValue[SBox.type], field) match {
          case (box, SBox.Value) => eval(mkExtractAmount(box))
          case (box, SBox.PropositionBytes) => eval(mkExtractScriptBytes(box))
          case (box, SBox.Id) => eval(mkExtractId(box))
          case (box, SBox.Bytes) => eval(mkExtractBytes(box))
          case (box, SBox.BytesWithNoRef) => eval(mkExtractBytesWithNoRef(box))
//          case (box, _) if box.tpe.hasMethod(field) =>
//            (None, env)  // leave it as it is and handle on a level of parent node
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
        }

      case Select(obj: SigmaBoolean, field, _) =>
        field match {
          case SigmaBoolean.PropBytes => eval(ByteArrayConstant(obj.bytes))
          case SigmaBoolean.IsValid => eval(obj)
        }

      case Select(obj, "value", Some(SLong)) if obj.tpe == SBox =>
        eval(mkExtractAmount(obj.asValue[SBox.type]))

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        eval(mkSelectField(tuple.asTuple, index))

      case Terms.Apply(Select(col, "slice", _), Seq(from, until)) =>
        eval(mkSlice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

      case Terms.Apply(Select(col, "where", _), Seq(Terms.Lambda(Seq((n, t)), _, Some(body)))) =>
        val input = col.asValue[SCollection[SType]]
        val cond = body.asValue[SBoolean.type]
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val Pair(condCalc, condCost) = split(fun { x: Rep[Any] =>
          evalNode(ctx, env + (n -> CostedPrimRep(x, 0)), cond)
        })
        val res = inputC.value.filter(condCalc)
        val cost = inputC.cost + inputC.value.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(res, cost)

      case Terms.Apply(Select(col, method @ (SCollection.ExistsMethod.name | SCollection.ForallMethod.name), _),
                       Seq(Terms.Lambda(Seq((n, t)), _, Some(body)))) =>
        val input = col.asValue[SCollection[SType]]
        val cond = body.asValue[SBoolean.type]
        val eItem = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eItem == eAny, s"Types should be equal: but $eItem != $eAny")
        val Pair(condCalc, condCost) = split(fun { x: Rep[Any] =>
          evalNode(ctx, env + (n -> CostedPrimRep(x, 0)), cond)
        })
        val value = method match {
          case SCollection.ExistsMethod.name =>
            inputC.value.exists(condCalc)
          case SCollection.ForallMethod.name =>
            inputC.value.forall(condCalc)
        }
        val cost = inputC.cost + inputC.value.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(value, cost)

      case Terms.Apply(Select(col,"map", _), Seq(Terms.Lambda(Seq((n, t)), _, Some(mapper)))) =>
        val input = col.asValue[SCollection[SType]]
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val Pair(mapperCalc, mapperCost) = split(fun { x: Rep[Any] =>
          evalNode(ctx, env + (n -> CostedPrimRep(x, 0)), mapper)
        })
        val res = inputC.value.map(mapperCalc)
        val cost = inputC.cost + inputC.value.map(mapperCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(res, cost)

//      case Terms.Apply(Select(col,"fold", _), Seq(zero, Terms.Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
//        val taggedZero = mkTaggedVariable(21, tZero)
//        val taggedOp = mkTaggedVariable(22, tOp)
//        val env1 = env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp)
//        some(mkFold(col.asValue[SCollection[SType]], taggedZero.varId, zero, taggedOp.varId, body))(env1)

      case Terms.Apply(Select(col,"getOrElse", _), Seq(index, defaultValue)) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        eval(mkByIndex(col.asValue[SCollection[SType]], index1, Some(defaultValue1)))

      case opt: OptionValue[_] =>
        error(s"Option constructors are not supported: $opt")

//      case AND(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[AND]) =>
//        eval(mkAND(
//          mkConcreteCollection(
//            items.flatMap {
//              case AND(ConcreteCollection(innerItems, SBoolean)) => innerItems
//              case v => IndexedSeq(v)
//            }, SBoolean)))
//
//      case OR(ConcreteCollection(items, SBoolean)) if items.exists(_.isInstanceOf[OR]) =>
//        eval(mkOR(
//          mkConcreteCollection(
//            items.flatMap {
//              case OR(ConcreteCollection(innerItems, SBoolean)) => innerItems
//              case v => IndexedSeq(v)
//            }, SBoolean)))

      case SizeOf(xs) =>
        val xsC = evalNode(ctx, env, xs).asRep[Costed[Col[Any]]]
        CostedPrimRep(xsC.value.length, xsC.cost + SizeOfDeclaration)
      case SigmaPropIsValid(p) =>
        val pC = evalNode(ctx, env, p).asRep[Costed[Sigma]]
        CostedPrimRep(pC.value.isValid, pC.cost + SigmaPropIsValidDeclaration)
      case SigmaPropBytes(p) =>
        val pC = evalNode(ctx, env, p).asRep[Costed[Sigma]]
        CostedPrimRep(pC.value.propBytes, pC.cost + SigmaPropBytesDeclaration + pC.value.propBytes.length)
      case utxo.ExtractAmount(box) =>
        val boxC = evalNode(ctx, env, box).asRep[Costed[Box]]
        CostedPrimRep(boxC.value.value, boxC.cost + Cost.ExtractAmount)
      case utxo.ExtractScriptBytes(box) =>
        val boxC = evalNode(ctx, env, box).asRep[Costed[Box]]
        val bytes = boxC.value.propositionBytes
        CostedPrimRep(bytes, boxC.cost + Cost.ExtractScriptBytes + bytes.length)
      case utxo.ExtractRegisterAs(box, regId, tpe, default) =>
        val boxC = evalNode(ctx, env, box).asRep[Costed[Box]]
        val elem = stypeToElem(tpe)
        val valueOpt = boxC.value.getReg(regId.number.toInt)(elem)
        val baseCost = boxC.cost + Cost.ExtractRegister
        val (v, c) = if (default.isDefined) {
          val d = evalNode(ctx, env, default.get)
          (RWSpecialPredef.optionGetOrElse(valueOpt, d.value), baseCost + d.cost)
        } else {
          (valueOpt.get, baseCost)
        }
        CostedPrimRep(v, c)
      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(ctx, env, op.left)
        val y = evalNode(ctx, env, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(ApplyBinOp(binop, x.value, y.value), x.cost + y.cost + TripleDeclaration)
        }
      case OR(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(ctx, env, _))
          val res = sigmaDslBuilder.anyOf(colBuilder.apply(itemsC.map(_.value): _*))
          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + OrDeclaration
          CostedPrimRep(res, cost)
//          val headC = evalNode(ctx, env, cc.items(0))
//          val tailC = cc.items.iterator.drop(1).map(x => evalCostedBlock(evalNode(ctx, env, x)))
//          val itemsC = (Iterator.single(headC) ++ tailC).toIndexedSeq
//          val res = sigmaDslBuilder.anyOf(colBuilder.apply(new mutable.WrappedArray.ofRef(itemsC.map(_.value).toArray):_*))
//          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + OrDeclaration
//          CostedPrimRep(res, cost)
        },
        const => ???,
        tup => ???
      )
      case AND(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(ctx, env, _))
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
        val x = evalNode(ctx, env, rel.left)
        val y = evalNode(ctx, env, rel.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(
            binop.apply(x.value, y.value.asRep[t#WrappedType]),
            x.cost + y.cost + TripleDeclaration
          )
        }

      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    res.asRep[Costed[T#WrappedType]]
  }

  def buildCostedGraph[T <: SType](envVals: Map[String, SValue], tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = {
    fun { ctx: Rep[Context] =>
      val env = envVals.mapValues(v => evalNode(ctx, Map(), v))
      evalNode(ctx, env, tree)
    }
  }

  def cost(env: Map[String, Any], code: String) = {
    val typed = compiler.typecheck(env, code)
    val cg = buildCostedGraph[SType](env.mapValues(builder.liftAny(_).get), typed)
    cg
  }

  def error(msg: String) = throw new CosterException(msg, None)
}
