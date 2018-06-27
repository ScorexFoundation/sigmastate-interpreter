package sigmastate.lang

import scalan.{SigmaLibrary, Lazy}

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

  def byteSize[T](eT: BaseElem[T]): Int = eT match {
    case BooleanElement => 1
    case ByteElement => 1
    case CharElement => 1
    case ShortElement => 2
    case IntElement => 4
    case LongElement => 8
  }

  def dataCost[T](x: Rep[T]): Rep[Costed[T]] = x.elem match {
    case be: BaseElem[_] => CostedPrim(x, byteSize(be).toLong)
    case pe: PairElem[a,b] =>
      val l = dataCost(x.asRep[(a,b)]._1)
      val r = dataCost(x.asRep[(a,b)]._2)
      CostedPair(l, r)
    case ae: WArrayElem[_,_] =>
      ae.eItem match {
        case be: BaseElem[a] =>
          val arr = x.asRep[WArray[a]]
          val values = b.fromArray(arr)
          val costs = ReplCol(byteSize(be).toLong, values.length)
          CostedArray(values, costs).asRep[Costed[T]]
        case pe: PairElem[a,b] =>
          val arr = x.asRep[WArray[(a,b)]]
          implicit val ea = pe.eFst
          implicit val eb = pe.eSnd
          val ls = dataCost[WArray[a]](arr.map(fun[(a,b), a](_._1)(Lazy(pairElement(ea, eb)))))
          val rs = dataCost[WArray[b]](arr.map(fun[(a,b), b](_._2)(Lazy(pairElement(ea, eb)))))
          CostedPairArray(ls, rs).asRep[Costed[T]]
        case ae: WArrayElem[a,_] =>
          implicit val ea = ae.eItem
          val arr = x.asRep[WArray[WArray[a]]]
          val col = b.fromArray(arr)
          val rows = col.map(fun((r: Rep[WArray[a]]) => dataCost(r)))
          CostedNestedArray(rows).asRep[Costed[T]]
      }
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
}
