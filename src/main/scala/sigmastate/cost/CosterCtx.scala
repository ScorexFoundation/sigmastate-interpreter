package sigmastate.lang

import org.bouncycastle.math.ec.ECPoint

import scalan.{Lazy, SigmaLibrary}
import org.ergoplatform.{Height, Outputs, Self, Inputs}
import sigmastate._
import sigmastate.Values.{TaggedVariable, Value, Constant, SValue}
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo._

trait CosterCtx extends SigmaLibrary {
  val WA = WArrayMethods
  val CM = ColMethods
  val CBM = ColBuilderMethods
  val SCM = SigmaContractMethods

  override def rewriteDef[T](d: Def[T]) = d match {
    case _ =>
      if (currentPass.config.constantPropagation) {
        // additional constant propagation rules (see other similar cases)
        d match {
          case SCM.anyOf(c, CBM.apply_apply_items(b, items)) if (items.forall(_.isConst)) =>
            val bs = items.map { case Def(Const(b: Boolean)) => b }
            toRep(bs.exists(_ == true))
          case SCM.allOf(c, CBM.apply_apply_items(b, items)) if (items.forall(_.isConst)) =>
            val bs = items.map { case Def(Const(b: Boolean)) => b }
            toRep(bs.forall(_ == true))
          case _ =>
            super.rewriteDef(d)
        }
      }
      else
        super.rewriteDef(d)
  }
  override def toRep[A](x: A)(implicit eA: Elem[A]):Rep[A] = eA match {
    case EcPointElement => Const(x)
    case _ => super.toRep(x)
  }

  case class WEcPointNew(p: Rep[ECPoint]) extends Def[WECPoint] {
    def selfType: Elem[WECPoint] = wECPointElement
  }

  def plus(x: Rep[Int], n: Int) = {
    Range(0, n).foldLeft(x)((y, i) => y + i)
  }

  val colBuilder = ColOverArrayBuilder()
  val costedBuilder = ConcreteCostedBuilder()

  import Cost._

  def byteSize[T](eT: BaseElem[T]): Int = eT match {
    case BooleanElement => 1
    case ByteElement => 1
    case CharElement => 1
    case ShortElement => 2
    case IntElement => 4
    case LongElement => 8
  }

  def dataCost[T](x: Rep[T]): Rep[Costed[T]] = {
    val res: Rep[Any] = x.elem match {
      case be: BaseElem[_] => CostedPrimRep(x, byteSize(be))
      case pe: PairElem[a,b] =>
        val l = dataCost(x.asRep[(a,b)]._1)
        val r = dataCost(x.asRep[(a,b)]._2)
        CostedPairRep(l, r)
      case boxE: BoxElem[_] =>
        val box = x.asRep[Box]
        CostedPrimRep(box, BoxConstantDeclaration)
      case ae: WArrayElem[_,_] =>
        ae.eItem match {
          case be: BaseElem[a] =>
            val arr = x.asRep[WArray[a]]
            val values = colBuilder.fromArray(arr)
            val costs = ReplColRep(byteSize(be), values.length)
            CostedArrayRep(values, costs)
          case pe: PairElem[a,b] =>
            val arr = x.asRep[WArray[(a,b)]]
            implicit val ea = pe.eFst
            implicit val eb = pe.eSnd
            val ls = dataCost[WArray[a]](arr.map(fun[(a,b), a](_._1)(Lazy(pe))))
            val rs = dataCost[WArray[b]](arr.map(fun[(a,b), b](_._2)(Lazy(pe))))
            CostedPairArrayRep(ls, rs)
          case ae: WArrayElem[a,_] =>
            implicit val ea = ae.eItem
            val arr = x.asRep[WArray[WArray[a]]]
            val col = colBuilder.fromArray(arr)
            val rows = col.map(fun((r: Rep[WArray[a]]) => dataCost(r)))
            CostedNestedArrayRep(rows)
          case entE: EntityElem[a] => // fallback case
            val arr = x.asRep[WArray[a]]
            val col = colBuilder.fromArray(arr)
            val costs = col.map(fun((r: Rep[a]) => dataCost(r).cost)(Lazy(entE)))
            CostedArrayRep(col, costs)
        }
      case ce: ColElem[_,_] =>
        ce.eA match {
          case be: BaseElem[a] =>
            val values = x.asRep[Col[a]]
            val costs = ReplColRep(byteSize(be), values.length)
            CostedColRep(values, costs)
          case pe: PairElem[a,b] =>
            val arr = x.asRep[Col[(a,b)]]
            implicit val ea = pe.eFst
            implicit val eb = pe.eSnd
            val ls = dataCost[Col[a]](arr.map(fun[(a,b), a](_._1)(Lazy(pe))))
            val rs = dataCost[Col[b]](arr.map(fun[(a,b), b](_._2)(Lazy(pe))))
            CostedPairColRep(ls, rs)
          case ce: ColElem[a,_] =>
            implicit val ea = ce.eA
            val col = x.asRep[Col[Col[a]]]
            val rows = col.map(fun((r: Rep[Col[a]]) => dataCost(r)))
            CostedNestedColRep(rows)
          case entE: EntityElem[a] => // fallback case
            val col = x.asRep[Col[a]]
            val costs = col.map(fun((r: Rep[a]) => dataCost(r).cost)(Lazy(entE)))
            CostedColRep(col, costs)
        }
    }
    res.asRep[Costed[T]]
  }

  def result[T](dc: Rep[Costed[T]]): Rep[(T, Int)] = Pair(dc.value, dc.cost)

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
    case SProof => sigmaElement
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

  private def evalNode[SC <: SigmaContract, T <: SType](contr: Rep[SC], ctx: Rep[Context], vars: Map[Byte, Rep[_]], node: Value[T]): RCosted[T#WrappedType] = {
    val res: Rep[Any] = node match {
      case Constant(v, tpe) => v match {
        case p: scapi.sigma.DLogProtocol.ProveDlog =>
          val ge = evalNode(contr, ctx, vars, p.value).asRep[Costed[WECPoint]]
          CostedPrimRep(ProveDlogEvidence(ge.value), ge.cost + Dlog)
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
      case TaggedVariable(id, tpe) =>
        if (id >= 21) {
          vars.get(id) match {
            case Some(x: RCosted[a]@unchecked) => CostedPrimRep(x.value, x.cost + VariableAccess)
            case _ => !!!(s"Variable $node not found in environment $vars")
          }
        } else {
          CostedPrimRep(ctx.getVar(id)(stypeToElem(tpe)), VariableAccess)
        }
      case SizeOf(xs) =>
        val xsC = evalNode(contr, ctx, vars, xs).asRep[Costed[Col[Any]]]
        CostedPrimRep(xsC.value.length, xsC.cost + SizeOfDeclaration)
      case IsValid(p) =>
        val pC = evalNode(contr, ctx, vars, p).asRep[Costed[Sigma]]
        CostedPrim(pC.value.isValid, pC.cost + ProofIsValidDeclaration)
      case ProofBytes(p) =>
        val pC = evalNode(contr, ctx, vars, p).asRep[Costed[Sigma]]
        CostedPrim(pC.value.propBytes, pC.cost + ProofBytesDeclaration)
      case utxo.ExtractAmount(box) =>
        val boxC = evalNode(contr, ctx, vars, box).asRep[Costed[Box]]
        CostedPrimRep(boxC.value.value, boxC.cost + Cost.ExtractAmount)
      case utxo.ExtractScriptBytes(box) =>
        val boxC = evalNode(contr, ctx, vars, box).asRep[Costed[Box]]
        CostedPrimRep(boxC.value.propositionBytes, boxC.cost + Cost.ExtractScriptBytes)
      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(contr, ctx, vars, op.left)
        val y = evalNode(contr, ctx, vars, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(ApplyBinOp(binop, x.value, y.value), x.cost + y.cost + TripleDeclaration)
        }
      case OR(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(contr, ctx, vars, _))
          val res = contr.anyOf(colBuilder.apply(itemsC.map(_.value): _*))
          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + OrDeclaration
          CostedPrimRep(res, cost)
        },
        const => ???,
        tup => ???
      )
      case AND(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(contr, ctx, vars, _))
          val res = contr.allOf(colBuilder.apply(itemsC.map(_.value): _*))
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
        val x = evalNode(contr, ctx, vars, rel.left)
        val y = evalNode(contr, ctx, vars, rel.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(
            binop.apply(x.value, y.value.asRep[t#WrappedType]),
            x.cost + y.cost + TripleDeclaration
          )
        }

      case MapCollection(input, id, mapper) =>
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(contr, ctx, vars, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val Pair(mapperCalc, mapperCost) = split(fun { x: Rep[Any] =>
          evalNode(contr, ctx, vars + (id -> CostedPrimRep(x, 0)), mapper)
        })
        val res = inputC.value.map(mapperCalc)
        val cost = inputC.cost + inputC.value.map(mapperCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(res, cost)

      case Where(input, id, cond) =>
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(contr, ctx, vars, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val Pair(condCalc, condCost) = split(fun { x: Rep[Any] =>
          evalNode(contr, ctx, vars + (id -> CostedPrimRep(x, 0)), cond)
        })
        val res = inputC.value.filter(condCalc)
        val cost = inputC.cost + inputC.value.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(res, cost)

      case trans: BooleanTransformer[_] =>
        val eItem = stypeToElem(trans.input.tpe.elemType)
        val inputC = evalNode(contr, ctx, vars, trans.input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eItem == eAny, s"Types should be equal: but $eItem != $eAny")
        val Pair(condCalc, condCost) = split(fun { x: Rep[Any] =>
          evalNode(contr, ctx, vars + (trans.id -> CostedPrimRep(x, 0)), trans.condition)
        })
        val value = trans.opCode match {
          case OpCodes.ExistsCode =>
            inputC.value.exists(condCalc)
          case OpCodes.ForAllCode =>
            inputC.value.forall(condCalc)
        }
        val cost = inputC.cost + inputC.value.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        CostedPrimRep(value, cost)

      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    res.asRep[Costed[T#WrappedType]]
  }

  def buildCostedGraph[SC <: SigmaContract: Elem, T <: SType](ctxVars: Map[Byte, SValue], tree: Value[T]): Rep[((SC, Context)) => Costed[T#WrappedType]] = {
    fun { in: Rep[(SC, Context)] =>
      val Pair(contr, ctx) = in
      val vars = ctxVars.mapValues(v => evalNode(contr, ctx, Map(), v))
      evalNode(contr, ctx, vars, tree)
    }
  }

  def error(msg: String) = throw new CosterException(msg, None)
}
