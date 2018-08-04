package sigmastate.eval

import sigmastate.lang.Costing

import scalan.Lazy
import sigmastate.utxo.CostTable.Cost.BoxConstantDeclaration

trait DataCosting extends Costing {
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

}
