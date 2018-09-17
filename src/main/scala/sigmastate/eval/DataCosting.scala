package sigmastate.eval

import sigmastate.Values.{Constant, CollectionConstant}
import sigmastate.lang.Costing

import scalan.Lazy
import sigmastate.utxo.CostTable.Cost.BoxConstantDeclaration

trait DataCosting { self: Costing =>
  import WArray._; import Col._
  import Box._
  import ColBuilder._;
  import ReplCol._;
  import Costed._;
  import CostedPrim._;
  import CostedPair._;
  import CostedArray._;
  import CostedNestedArray._;
  import CostedPairArray._
  import CostedCol._;
  import CostedNestedCol._; import CostedPairCol._

  def dataCost[T](x: Rep[T], optCost: Option[Rep[Int]]): Rep[Costed[T]] = {
    val res: Rep[Any] = x.elem match {
      case pe: PairElem[a,b] =>
        val l = dataCost(x.asRep[(a,b)]._1, None)
        val r = dataCost(x.asRep[(a,b)]._2, optCost)
        RCostedPair(l, r)
      case ce: ColElem[_,_] =>
        ce.eA match {
          case e: Elem[a] =>
            implicit val eA = e
            val xs = x.asRep[Col[a]]
            val costs = colBuilder.replicate(xs.length, 0)
            val tpe = elemToSType(e)
            val sizes = if (tpe.isConstantSize)
              colBuilder.replicate(xs.length, typeSize(tpe))
            else
              xs.map(fun(sizeOf(_)))
            val colCost = costOf(CollectionConstant(null, tpe))
            RCostedCol(xs, costs, sizes, optCost.fold(colCost)(c => c + colCost))
//          case pe: PairElem[a,b] =>
//            val arr = x.asRep[Col[(a,b)]]
//            implicit val ea = pe.eFst
//            implicit val eb = pe.eSnd
//            val ls = dataCost[Col[a]](arr.map(fun[(a,b), a](_._1)(Lazy(pe))))
//            val rs = dataCost[Col[b]](arr.map(fun[(a,b), b](_._2)(Lazy(pe))))
//            CostedPairColRep(ls, rs)
//          case ce: ColElem[a,_] =>
//            implicit val ea = ce.eA
//            val col = x.asRep[Col[Col[a]]]
//            val rows = col.map(fun((r: Rep[Col[a]]) => dataCost(r)))
//            CostedNestedColRep(rows)
//          case entE: EntityElem[a] => // fallback case
//            val col = x.asRep[Col[a]]
//            val costs = col.map(fun((r: Rep[a]) => dataCost(r).cost)(Lazy(entE)))
//            CostedColRep(col, costs)
        }
      case _ =>
        CostedPrimRep(x, optCost.getOrElse(0), sizeOf(x))
    }
    res.asRep[Costed[T]]
  }

}
