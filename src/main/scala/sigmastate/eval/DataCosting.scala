package sigmastate.eval

import sigmastate.Values.{CollectionConstant, Constant}

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
  import CostedPair._;
  import CostedOption._;
  import CostedArray._;
  import CostedNestedArray._;
  import CostedPairArray._
  import CostedCol._;
  import CostedNestedCol._; import CostedPairCol._
  import ConcreteCostedBuilder._
  import WSpecialPredef._

  override def rewriteDef[T](d: Def[T]): Rep[_] = {
    val CCB = ConcreteCostedBuilderMethods
    val SPCM = WSpecialPredefCompanionMethods
    d match {
      case CCB.costedValue(b, x, SPCM.some(cost)) =>
        dataCost(x, Some(asRep[Int](cost)))
      case _ => super.rewriteDef(d)
    }
  }

  def dataCost[T](x: Rep[T], optCost: Option[Rep[Int]]): Rep[Costed[T]] = {
    val res: Rep[Any] = x.elem match {
      case pe: PairElem[a,b] =>
        val l = dataCost(asRep[(a,b)](x)._1, None)
        val r = dataCost(asRep[(a,b)](x)._2, optCost)
        RCostedPair(l, r)
//      case optE: WOptionElem[a,_] =>
//        val optX = asRep[WOption[a]](x)
//        val cost = optX.getOrElse()
//        RCostedOption(optX)
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
            RCostedCol(xs, costs, sizes, optCost.fold(colCost)(c => c + colCost))
//          case pe: PairElem[a,b] =>
//            val arr = asRep[Col[(a,b)]](x)
//            implicit val ea = pe.eFst
//            implicit val eb = pe.eSnd
//            val ls = dataCost[Col[a]](arr.map(fun[(a,b), a](_._1)(Lazy(pe))))
//            val rs = dataCost[Col[b]](arr.map(fun[(a,b), b](_._2)(Lazy(pe))))
//            CostedPairColRep(ls, rs)
//          case ce: ColElem[a,_] =>
//            implicit val ea = ce.eA
//            val col = asRep[Col[Col[a]]](x)
//            val rows = col.map(fun((r: Rep[Col[a]]) => dataCost(r)))
//            CostedNestedColRep(rows)
//          case entE: EntityElem[a] => // fallback case
//            val col = asRep[Col[a]](x)
//            val costs = col.map(fun((r: Rep[a]) => dataCost(r).cost)(Lazy(entE)))
//            CostedColRep(col, costs)
        }
      case _ =>
        CostedPrimRep(x, optCost.getOrElse(0), sizeOf(x))
    }
    asRep[Costed[T]](res)
  }

}
