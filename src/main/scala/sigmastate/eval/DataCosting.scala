package sigmastate.eval

import sigmastate.Values.{CollectionConstant, Constant, OptionConstant}
import scalan.SigmaLibrary
import scalan.Lazy
import sigmastate.utxo.CostTable.Cost.BoxConstantDeclaration

trait DataCosting extends SigmaLibrary { self: RuntimeCosting =>
  import WArray._; import Col._
  import WOption._
  import Box._
  import ColBuilder._;
  import ReplCol._;
  import Costed._;
  import CostedPrim._;
  import CCostedPrim._;
  import CostedBox._;
  import CCostedBox._;
  import CostedPair._;
  import CCostedPair._;
  import CostedOption._;
  import CostedCol._;
  import CCostedCol._;
  import CCostedOption._;
  import CostedNestedCol._; import CostedPairCol._
  import CostedBuilder._
  import WSpecialPredef._

  def dataCost[T](x: Rep[T], optCost: Option[Rep[Int]]): Rep[Costed[T]] = {
    val res: Rep[Any] = x.elem match {
      case box: BoxElem[_] =>
        RCCostedBox(asRep[Box](x), optCost.getOrElse(0))
      case pe: PairElem[a,b] =>
        val l = dataCost(asRep[(a,b)](x)._1, None)
        val r = dataCost(asRep[(a,b)](x)._2, optCost)
        RCCostedPair(l, r)
      case optE: WOptionElem[a,_] =>
        val optX = asRep[WOption[a]](x)
        implicit val e = optE.eItem
        val tpe = elemToSType(e)
        val costOpt = optX.map(fun(x => 0))
        val sizeOpt =
          if (tpe.isConstantSize) optX.map(fun(x => typeSize(tpe)))
          else optX.map(fun(sizeOf(_)))
        val cost = costOf(OptionConstant(null, tpe))
        RCCostedOption(optX, costOpt, sizeOpt, optCost.fold(cost)(c => c + cost))
      case ce: ColElem[_,_] =>
        ce.eA match {
          case e: Elem[a] =>
            implicit val eA = e
            val xs = asRep[Col[a]](x)
            val costs = colBuilder.replicate(xs.length, 0)
            val tpe = elemToSType(e)
            val sizes = if (tpe.isConstantSize)
              colBuilder.replicate(xs.length, typeSize(tpe))
            else
              xs.map(fun(sizeOf(_)))
            val colCost = costOf(CollectionConstant(null, tpe))
            RCCostedCol(xs, costs, sizes, optCost.fold(colCost)(c => c + colCost))
//          case pe: PairElem[a,b] =>
//            val arr = asRep[Coll[(a,b)]](x)
//            implicit val ea = pe.eFst
//            implicit val eb = pe.eSnd
//            val ls = dataCost[Coll[a]](arr.map(fun[(a,b), a](_._1)(Lazy(pe))))
//            val rs = dataCost[Coll[b]](arr.map(fun[(a,b), b](_._2)(Lazy(pe))))
//            CostedPairColRep(ls, rs)
//          case ce: ColElem[a,_] =>
//            implicit val ea = ce.eA
//            val col = asRep[Coll[Coll[a]]](x)
//            val rows = col.map(fun((r: Rep[Coll[a]]) => dataCost(r)))
//            CostedNestedColRep(rows)
//          case entE: EntityElem[a] => // fallback case
//            val col = asRep[Coll[a]](x)
//            val costs = col.map(fun((r: Rep[a]) => dataCost(r).cost)(Lazy(entE)))
//            CostedColRep(col, costs)
        }
      case _ =>
        RCCostedPrim(x, optCost.getOrElse(0), sizeOf(x))
    }
    asRep[Costed[T]](res)
  }

}
