package sigmastate.lang

import scalan.{Lazy, SigmaLibrary}
import org.ergoplatform.{Height, Outputs, Inputs, Self}
import sigmastate._
import sigmastate.Values.{Value, Constant}
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.SizeOf
import sigmastate.utxo.ExtractAmount

class CosterCtx extends SigmaLibrary {
  val WA = WArrayMethods
  object IsProjectFirst {
    def unapply[A,B](f: Rep[_]): Option[Rep[A=>B]] = f match {
      case Def(Lambda(_,_,x, Def(First(p)))) if p == x => Some(f.asRep[A=>B])
      case _ => None
    }
  }
  object IsProjectSecond {
    def unapply[A,B](f: Rep[_]): Option[Rep[A=>B]] = f match {
      case Def(Lambda(_,_,x, Def(Second(p)))) if p == x => Some(f.asRep[A=>B])
      case _ => None
    }
  }
  override def rewriteDef[T](d: Def[T]) = d match {
    case WA.length(WA.map(xs, _)) => xs.length
    case WA.zip(WA.map(xs, IsProjectFirst(_)), WA.map(ys, IsProjectSecond(_))) if xs == ys => xs
    case WA.map(WA.map(_xs, f: RFunc[a, b]), _g: RFunc[_,c]) =>
      implicit val ea = f.elem.eDom
      val xs = _xs.asRep[WArray[a]]
      val g  = _g.asRep[b => c]
      xs.map(fun { x: Rep[a] => g(f(x)) })
    case _ => super.rewriteDef(d)
  }

  def plus(x: Rep[Int], n: Int) = {
    Range(0, n).foldLeft(x)((y, i) => y + i)
  }

  val b = ColOverArrayBuilder()

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
      case be: BaseElem[_] => CostedPrimRep(x, byteSize(be).toLong)
      case pe: PairElem[a,b] =>
        val l = dataCost(x.asRep[(a,b)]._1)
        val r = dataCost(x.asRep[(a,b)]._2)
        CostedPairRep(l, r)
      case boxE: BoxElem[_] =>
        val box = x.asRep[Box]
        CostedPrimRep(box, BoxConstantDeclaration.toLong)
      case ae: WArrayElem[_,_] =>
        ae.eItem match {
          case be: BaseElem[a] =>
            val arr = x.asRep[WArray[a]]
            val values = b.fromArray(arr)
            val costs = ReplColRep(byteSize(be).toLong, values.length)
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
            val col = b.fromArray(arr)
            val rows = col.map(fun((r: Rep[WArray[a]]) => dataCost(r)))
            CostedNestedArrayRep(rows)
          case entE: EntityElem[a] => // fallback case
            val arr = x.asRep[WArray[a]]
            val col = b.fromArray(arr)
            val costs = col.map(fun((r: Rep[a]) => dataCost(r).cost)(Lazy(entE)))
            CostedArrayRep(col, costs)
        }
      case ce: ColElem[_,_] =>
        ce.eA match {
          case be: BaseElem[a] =>
            val values = x.asRep[Col[a]]
            val costs = ReplColRep(byteSize(be).toLong, values.length)
            CostedArrayRep(values, costs)
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

  def result[T](dc: Rep[Costed[T]]): Rep[(T, Long)] = Pair(dc.value, dc.cost)

  def split[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, T => Long)] = {
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
    case _ => error(s"Don't know how to convert SType $t to Elem")
  }).asElem[T#WrappedType]

  private val elemToNumericMap = Map[Elem[_], Numeric[_]](
    (ByteElement, numeric[Byte]),
    (ShortElement, numeric[Short]),
    (IntElement, numeric[Int]),
    (LongElement, numeric[Long])
  )

  def elemToNumeric[T](e: Elem[T]): Numeric[T] = elemToNumericMap(e).asInstanceOf[Numeric[T]]
  def opcodeToBinOp[T](opCode: Byte, eT: Elem[T]): BinOp[T,T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToNumeric(eT))(eT)
    case _ => error(s"Cannot find BinOp for opcode $opCode")
  }

  type RCosted[A] = Rep[Costed[A]]

  def flatMap[A,B](source: RCosted[A], f: (Rep[A], Rep[Long]) => RCosted[B]): RCosted[B] = {
    val v = source.value
    val c = source.cost
    f(v, c)
  }

//  def flatMap[A,B,C](ca: RCosted[A], cb: RCosted[A], f: (Rep[A], Rep[B], Rep[Long]) => RCosted[B]): RCosted[B] = {
//    val v = source.value
//    val c = source.cost
//    f(v, c)
//  }

  private def evalNode[T <: SType](ctx: Rep[Context], node: Value[T]): RCosted[T#WrappedType] = {
    val res: Rep[Any] = node match {
      case Constant(v, tpe) => CostedPrimRep(toRep(v)(stypeToElem(tpe)), ConstantNode.toLong)
      case Height => CostedPrimRep(ctx.HEIGHT, HeightAccess.toLong)
      case Inputs => CostedPrimRep(ctx.INPUTS, InputsAccess.toLong)
      case Outputs => CostedPrimRep(ctx.OUTPUTS, OutputsAccess.toLong)
      case Self => CostedPrimRep(ctx.SELF, SelfAccess.toLong)
      case SizeOf(xs) =>
        val xsC = evalNode(ctx, xs).asRep[Costed[Col[Any]]]
        CostedPrimRep(xsC.value.length, xsC.cost + SizeOfDeclaration.toLong)
      case utxo.ExtractAmount(box) =>
        val boxC = evalNode(ctx, box).asRep[Costed[Box]]
        CostedPrimRep(boxC.value.value, boxC.cost + Cost.ExtractAmount.toLong)
      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(op.opCode, et)
        val x = evalNode(ctx, op.left)
        val y = evalNode(ctx, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          CostedPrimRep(ApplyBinOp(binop, x.value, y.value), x.cost + y.cost + TripleDeclaration.toLong)
        }

      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    res.asRep[Costed[T#WrappedType]]
  }

  def buildCostedGraph[T <: SType](tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = {
    fun { ctx: Rep[Context] => evalNode(ctx, tree) }
  }

  def error(msg: String) = throw new CosterException(msg, None)
}
