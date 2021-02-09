package sigmastate

import scalan.RType._
import sigmastate.Values.{PerItemCost, FixedCost}
import spire.sp
import spire.syntax.all.cfor
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.interpreter.ErgoTreeEvaluator.{OperationDesc, NamedDesc}
import special.sigma.Box
import special.collection.{CollOverArray, CollType, PairOfCols, CReplColl, Coll}

object DataValueComparer {
  /** Equals two CollOverArray of primitive (unboxed) type. */
  final val CostKind_EQ_COA_Prim = PerItemCost(1, 1, 64)

  /** Equals two CollOverArray of Coll (nested collections). */
  final val CostKind_EQ_COA_Coll = PerItemCost(1, 1, 16)

  /** Equals two CollOverArray of AnyRef type (fallback case). */
  final val CostKind_EQ_COA_AnyRef = PerItemCost(1, 1, 16)

  /** Equals CollOverArray with PairOfCols of primitive (unboxed) type. */
  final val CostKind_EQ_COA_POC_Prim = PerItemCost(1, 1, 16)

  /** Equals CollOverArray with PairOfCols of AnyRef type (fallback type). */
  final val CostKind_EQ_COA_POC_AnyRef = PerItemCost(1, 1, 16)

  final val CostKind_EQPerDataNode = FixedCost(1)

  /** EQ of two CollOverArray of primitive (unboxed) type. */
  final val OpDesc_EQ_COA_Prim = NamedDesc("EQ_COA_Prim")

  /** Equals two CollOverArray of AnyRef type with constant comparison cost i.e.
    * GroupElement, BigInt etc. (fallback case).
    * Note: Box type is also falls in this category because its equality is based on
    * comparing `Box.id`. Those `Box.id` are computed by hashing which is costed
    * separately.
    */
  final val OpDesc_EQ_COA_AnyRef = NamedDesc("EQ_COA_AnyRef")

  /** Equals two CollOverArray of Coll (nested collections). */
  final val OpDesc_EQ_COA_Coll = NamedDesc("EQ_COA_Coll")

  /** Equals CollOverArray with PairOfCols of primitive (unboxed) type. */
  final val OpDesc_EQ_COA_POC_Prim = NamedDesc("EQ_COA_POC_Prim")

  /** Equals CollOverArray with PairOfCols of AnyRef type (fallback type). */
  final val OpDesc_EQ_COA_POC_AnyRef = NamedDesc("EQ_COA_POC_AnyRef")

  final val OpDesc_EQPerDataNode = NamedDesc("EQPerDataNode")

  type COA[A] = CollOverArray[A]
  type POC[A,B] = PairOfCols[A, B]

  def equalCOA_Prim[@sp(Byte, Int) A]
                          (c1: COA[A], c2: COA[A],
                           costKind: PerItemCost, opDesc: OperationDesc, sizeFactor: Int)
                          (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costKind, opDesc) { () =>
      val len = c1.length
      var i = 0
      while (i < len && okEqual) {
        okEqual = c1(i) == c2(i)
        i += 1
      }
      i * sizeFactor  // return the number of actually compared elements
    }
    okEqual
  }

  def equalCOA_Prim_Dispatch[A](coll1: COA[A], coll2: COA[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1.tItem match {
      case ByteType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Byte]],
          coll2.asInstanceOf[COA[Byte]],
          CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim, sizeFactor = 1)

      case IntType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Int]],
          coll2.asInstanceOf[COA[Int]],
          CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim, sizeFactor = 4)

      case _: CollType[a] =>
        var okEqual = true
        E.addSeqCost(CostKind_EQ_COA_Coll, OpDesc_EQ_COA_Coll) { () =>
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
        equalCOA_Prim(coll1, coll2,
          CostKind_EQ_COA_AnyRef, OpDesc_EQ_COA_AnyRef, sizeFactor = 4)
    }
  }

  def equalCOA_POC_1[@sp(Byte, Int) A, B]
                  (c1: COA[(A,B)], c2: Coll[A],
                   costKind: PerItemCost, opDesc: OperationDesc)
                  (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costKind, opDesc) { () =>
      val len = c1.length
      var i = 0
      while (i < len && okEqual) {
        val p: (A,_) = c1(i)
        okEqual = p._1 == c2(i)
        i += 1
      }
      i  // return the number of actually compared elements
    }
    okEqual
  }

  def equalCOA_POC_2[@sp(Byte, Int) B]
                  (c1: COA[(_,B)], c2: Coll[B],
                   costKind: PerItemCost, opDesc: OperationDesc)
                  (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costKind, opDesc) { () =>
      val len = c1.length
      var i = 0
      while (i < len && okEqual) {
        val p = c1(i)
        okEqual = p._2 == c2(i)
        i += 1
      }
      i  // return the number of actually compared elements
    }
    okEqual
  }

  def equalCOA_POC_Coll_1[A]
                  (c1: COA[(Coll[A],_)], c2: Coll[Coll[A]],
                   costKind: PerItemCost, opDesc: OperationDesc)
                  (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costKind, opDesc) { () =>
      val len = c1.length
      var i = 0
      while (i < len && okEqual) {
        val p = c1(i)
        okEqual = equalColls(p._1, c2(i))
        i += 1
      }
      i  // return the number of actually compared elements
    }
    okEqual
  }

  def equalCOA_POC_Coll_2[B]
                  (c1: COA[(_,Coll[B])], c2: Coll[Coll[B]],
                   costKind: PerItemCost, opDesc: OperationDesc)
                  (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costKind, opDesc) { () =>
      val len = c1.length
      var i = 0
      while (i < len && okEqual) {
        val p = c1(i)
        okEqual = equalColls(p._2, c2(i))
        i += 1
      }
      i  // return the number of actually compared elements
    }
    okEqual
  }

  def equalCOA_POC_Dispatch[A,B](c1: COA[(A,B)], c2: POC[A,B])
                                (implicit E: ErgoTreeEvaluator): Boolean = c2.tL match {
    case ByteType => c2.tR match {
      case ByteType =>
        equalCOA_POC_1(
          c1.asInstanceOf[COA[(Byte, B)]], c2.ls.asInstanceOf[Coll[Byte]],
          CostKind_EQ_COA_POC_Prim, OpDesc_EQ_COA_POC_Prim) &&
        equalCOA_POC_2(
          c1.asInstanceOf[COA[(_, Byte)]], c2.rs.asInstanceOf[Coll[Byte]],
          CostKind_EQ_COA_POC_Prim, OpDesc_EQ_COA_POC_Prim)
      case IntType =>
        equalCOA_POC_1(
          c1.asInstanceOf[COA[(Byte, B)]], c2.ls.asInstanceOf[Coll[Byte]],
          CostKind_EQ_COA_POC_Prim, OpDesc_EQ_COA_POC_Prim) &&
        equalCOA_POC_2(
          c1.asInstanceOf[COA[(_, Int)]], c2.ls.asInstanceOf[Coll[Int]],
          CostKind_EQ_COA_POC_Prim, OpDesc_EQ_COA_POC_Prim)
      case _: CollType[a] =>
        equalCOA_POC_1(
          c1.asInstanceOf[COA[(Byte, B)]], c2.ls.asInstanceOf[Coll[Byte]],
          CostKind_EQ_COA_POC_Prim, OpDesc_EQ_COA_POC_Prim) &&
        equalCOA_POC_Coll_2(
          c1.asInstanceOf[COA[(_, Coll[a])]], c2.ls.asInstanceOf[Coll[Coll[a]]],
          CostKind_EQ_COA_POC_Prim, OpDesc_EQ_COA_POC_Prim)
      case _ =>
        equalCOA_POC_1(c1, c2.ls, CostKind_EQ_COA_POC_AnyRef, OpDesc_EQ_COA_POC_AnyRef)
//        &&
//        equalCOA_POC_2(c1, c2.rs, CostKind_EQ_COA_POC_AnyRef, OpDesc_EQ_COA_POC_AnyRef)
    }
//    case IntType => c2.tItem match {
//      case ByteType =>
//        equalCOA_POC(c1.asInstanceOf[COA[Int]], c1.asInstanceOf[COA[Byte]])
//      case IntType =>
//        equalCOA_POC(c1.asInstanceOf[COA[Int]], c1.asInstanceOf[COA[Int]])
//    }
//    case _ => c2.tItem match {
//      case ByteType =>
//        equalCOA_POC(c1.asInstanceOf[COA[Int]], c1.asInstanceOf[COA[Byte]])
//      case IntType =>
//        equalCOA_POC(c1.asInstanceOf[COA[Int]], c1.asInstanceOf[COA[Int]])
//    }
  }
  
  def equalPOC_POC[A,B](c1: POC[A,B], c2: POC[A,B])(implicit E: ErgoTreeEvaluator): Boolean = {
    false
  }
  def equalPOC_Repl[A,B](c1: POC[A,B], c2: CReplColl[(A,B)])(implicit E: ErgoTreeEvaluator): Boolean = {
    false
  }
  def equalCOA_Repl[A](c1: COA[A], c2: CReplColl[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    false
  }
  def equalRepl_Repl[A](c1: CReplColl[A], c2: CReplColl[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    false
  }

  def equalColls(coll1: Coll[_], coll2: Coll[_])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1 match {
      case coll1: COA[a] => coll2 match {
        case coll2: COA[_] =>
          equalCOA_Prim_Dispatch(coll1, coll2.asInstanceOf[COA[a]])
        case coll2: POC[a,b] =>
          equalCOA_POC_Dispatch(coll1.asInstanceOf[COA[(a,b)]], coll2)
        case coll2: CReplColl[_] =>
          equalCOA_Repl(coll1, coll2.asInstanceOf[CReplColl[a]])
      }
      case coll1: POC[a,b] => coll2 match {
        case coll2: COA[_] =>
          equalCOA_POC_Dispatch(coll2.asInstanceOf[COA[(a,b)]], coll1) // EQ is commutative
        case coll2: POC[_,_] =>
          equalPOC_POC(coll1, coll2.asInstanceOf[POC[a, b]])
        case coll2: CReplColl[_] =>
          equalPOC_Repl(coll1, coll2.asInstanceOf[CReplColl[(a, b)]])
      }
      case coll1: CReplColl[a] => coll2 match {
        case coll2: COA[_] =>
          equalCOA_Repl(coll2.asInstanceOf[COA[a]], coll1) // EQ is commutative
        case coll2: POC[a,b] =>
          equalPOC_Repl(coll2, coll1.asInstanceOf[CReplColl[(a, b)]]) // EQ is commutative
        case coll2: CReplColl[_] =>
          equalRepl_Repl(coll1, coll2.asInstanceOf[CReplColl[a]])
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
