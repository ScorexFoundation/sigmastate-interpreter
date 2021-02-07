package sigmastate

import scalan.RType._
import sigmastate.Values.{PerItemCost, FixedCost}
import spire.sp
import spire.syntax.all.cfor
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.interpreter.ErgoTreeEvaluator.NamedDesc
import special.sigma.Box
import special.collection.{Coll, PairOfCols, CollOverArray}

object DataValueComparer {
  final val CostKind_EQCollByte = PerItemCost(1, 1, 64)
  final val CostKind_EQCollInt = PerItemCost(1, 1, 16)
  final val CostKind_EQPerDataNode = FixedCost(1)

  final val OpDesc_EqualBaseCollsOfPrim = NamedDesc("EqualBaseCollsOfPrim")
  final val OpDesc_EQPerDataNode = NamedDesc("EQPerDataNode")

  def equalBaseCollsOfPrim[@sp(Byte, Int) A]
                          (c1: CollOverArray[A],
                           c2: CollOverArray[A], costKind: PerItemCost)
                          (implicit E: ErgoTreeEvaluator): Boolean = {
    val len = c1.length
    var okEqual = true
    var i = 0
    while (i < len && okEqual) {
      okEqual = c1(i) == c2(i)
      i += 1
    }
    // TODO JITC: measure time
    E.addSeqCost(costKind, i, OpDesc_EqualBaseCollsOfPrim)(null)

    okEqual
  }

  def equalBaseColls[A](c1: CollOverArray[A], c2: CollOverArray[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    val len = c1.length
    var okEqual = true
    cfor(0)(_ < len && okEqual, _ + 1) { i =>
      okEqual = equalDataValues(c1(i), c2(i))
    }
    okEqual
  }

  def equalBaseAndPair[A,B](c1: CollOverArray[(A,B)], c2: PairOfCols[A,B])(implicit E: ErgoTreeEvaluator): Boolean = {
    false
  }

  def equalColls(coll1: Coll[_], coll2: Coll[_])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1 match {
      case coll1: CollOverArray[a] => coll2 match {
        case coll2: CollOverArray[_] => coll1.tItem match {
          case ByteType =>
            equalBaseCollsOfPrim(
              coll1.asInstanceOf[CollOverArray[Byte]],
              coll2.asInstanceOf[CollOverArray[Byte]], CostKind_EQCollByte)
          case IntType =>
            equalBaseCollsOfPrim(
              coll1.asInstanceOf[CollOverArray[Int]],
              coll2.asInstanceOf[CollOverArray[Int]], CostKind_EQCollInt)
          case _ =>
            equalBaseColls(coll1, coll2.asInstanceOf[CollOverArray[a]])
        }
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
