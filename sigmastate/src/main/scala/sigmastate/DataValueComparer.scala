package sigmastate

import scalan.RType._
import sigmastate.Values.{PerItemCost, FixedCost}
import spire.sp
import spire.syntax.all.cfor
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.interpreter.ErgoTreeEvaluator.NamedDesc
import special.sigma.Box
import special.collection.{Coll, PairOfCols, CollOverArray, CollType}

object DataValueComparer {
  /** Equals two CollOverArray of Byte. */
  final val CostKind_EQ_COA_Byte = PerItemCost(1, 1, 64)

  /** Equals two CollOverArray of Int. */
  final val CostKind_EQ_COA_Int = PerItemCost(1, 1, 16)

  /** Equals two CollOverArray of Coll (nested collections). */
  final val CostKind_EQ_COA_Coll = PerItemCost(1, 1, 16)

  /** Equals two CollOverArray of AnyRef type (fallback case). */
  final val CostKind_EQ_COA_AnyRef = PerItemCost(1, 1, 16)

  final val CostKind_EQPerDataNode = FixedCost(1)

  final val OpDesc_EqualBaseCollsOfPrim = NamedDesc("EqualBaseCollsOfPrim")
  final val OpDesc_EqualBaseCollsOfColl = NamedDesc("EqualBaseCollsOfColl")
  final val OpDesc_EQPerDataNode = NamedDesc("EQPerDataNode")

  def equalBaseCollsOfPrim[@sp(Byte, Int) A]
                          (c1: CollOverArray[A],
                           c2: CollOverArray[A], costKind: PerItemCost)
                          (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costKind, OpDesc_EqualBaseCollsOfPrim) { () =>
      val len = c1.length
      var i = 0
      while (i < len && okEqual) {
        okEqual = c1(i) == c2(i)
        i += 1
      }
      i  // return the number of actually compared elements
    }
    okEqual
  }

  def equalCOA_COA[A](coll1: CollOverArray[A], coll2: CollOverArray[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1.tItem match {
      case ByteType =>
        equalBaseCollsOfPrim(
          coll1.asInstanceOf[CollOverArray[Byte]],
          coll2.asInstanceOf[CollOverArray[Byte]], CostKind_EQ_COA_Byte)

      case IntType =>
        equalBaseCollsOfPrim(
          coll1.asInstanceOf[CollOverArray[Int]],
          coll2.asInstanceOf[CollOverArray[Int]], CostKind_EQ_COA_Int)

      case _: CollType[a] =>
        var okEqual = true
        E.addSeqCost(CostKind_EQ_COA_Coll, OpDesc_EqualBaseCollsOfColl) { () =>
          val c1 = coll1.asInstanceOf[Coll[Coll[a]]]
          val c2 = coll2.asInstanceOf[Coll[Coll[a]]]
          val len = c1.length
          var i = 0
          while(i < len && okEqual) {
            okEqual = equalColls(c1(i), c2(i))
            i += 1
          }
          i  // return the number of actually compared elements
        }
        okEqual

      case _ =>
        equalBaseCollsOfPrim(coll1, coll2, CostKind_EQ_COA_AnyRef)
    }
  }

  def equalBaseAndPair[A,B](c1: CollOverArray[(A,B)], c2: PairOfCols[A,B])(implicit E: ErgoTreeEvaluator): Boolean = {
    false
  }


  def equalColls(coll1: Coll[_], coll2: Coll[_])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1 match {
      case coll1: CollOverArray[a] => coll2 match {
        case coll2: CollOverArray[_] =>
          equalCOA_COA(coll1, coll2.asInstanceOf[CollOverArray[a]])
        case coll2: PairOfCols[a,b] =>
          equalBaseAndPair(coll1.asInstanceOf[CollOverArray[(a,b)]], coll2)
      }
    }
  }

  // TODO v5.0: introduce a new limit on structural depth of data values
  def equalDataValues(l: Any, r: Any)(implicit E: ErgoTreeEvaluator): Boolean = {
    E.addCost(CostKind_EQPerDataNode, OpDesc_EQPerDataNode)
    l match {
      case coll1: Coll[_] if r.isInstanceOf[Coll[_]] =>
        val coll2 = r.asInstanceOf[Coll[_]]
        val len = coll1.length
        if (len != coll2.length || coll1.tItem != coll2.tItem)
          return false
        var okEqual = true
        cfor(0)(_ < len && okEqual, _ + 1) { i =>
          okEqual = equalDataValues(coll1(i), coll2(i))
        }
        okEqual
      case box1: Box if r.isInstanceOf[Box] =>
        val box2 = r.asInstanceOf[Box]
        // TODO JITC: E.addCost(BoxEqCost)
        box1 == box2
      case tup1: Tuple2[_,_] if r.isInstanceOf[Tuple2[_,_]] =>
        val tup2 = r.asInstanceOf[Tuple2[_,_]]
        equalDataValues(tup1._1, tup2._1) && equalDataValues(tup1._2, tup2._2)
      case _ => l == r
    }
  }

}
