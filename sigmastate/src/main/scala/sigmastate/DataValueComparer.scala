package sigmastate

import scalan.{AVHashMap, Nullable, RType}
import scalan.RType._
import sigmastate.Values.{PerItemCost, FixedCost, CostKind}
import spire.sp
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.interpreter.ErgoTreeEvaluator.{OperationDesc, NamedDesc}
import special.sigma.{Box, GroupElementRType, GroupElement, AvlTreeRType, BigInt, BoxRType, AvlTree, BigIntRType}
import special.collection.{Coll, PairOfCols, CollOverArray}

object DataValueComparer {

  final val CostOf_TypeCheck = 5

  final val CostKind_EQ_Prim = FixedCost(2 + 6 * CostOf_TypeCheck)
  final val OpDesc_EQ_Prim = NamedDesc("EQ_Prim")
  final val EQ_Prim = OperationInfo(CostKind_EQ_Prim, OpDesc_EQ_Prim)

  /** Equals two Colls of non-primitive (boxed) types. */
  final val CostKind_EQ_Coll = PerItemCost(1, 1, 16)
  final val OpDesc_EQ_Coll = NamedDesc("EQ_Coll")
  final val EQ_Coll = OperationInfo(CostKind_EQ_Coll, OpDesc_EQ_Coll)

  final val CostKind_EQ_Tuple = FixedCost(3 + 2 * CostOf_TypeCheck)
  final val OpDesc_EQ_Tuple = NamedDesc("EQ_Tuple")
  final val EQ_Tuple = OperationInfo(CostKind_EQ_Tuple, OpDesc_EQ_Tuple)

  /** The cost depends on the position in `match` statement.
    * The cost of each type check == 10 units. */
  final val CostKind_EQ_BigInt = FixedCost(15 + 4 * CostOf_TypeCheck)
  final val OpDesc_EQ_BigInt = NamedDesc("EQ_BigInt")
  final val EQ_BigInt = OperationInfo(CostKind_EQ_BigInt, OpDesc_EQ_BigInt)

  final val CostKind_EQ_GroupElement = FixedCost(1)
  final val OpDesc_EQ_GroupElement = NamedDesc("EQ_GroupElement")
  final val EQ_GroupElement = OperationInfo(CostKind_EQ_GroupElement, OpDesc_EQ_GroupElement)

  final val CostKind_EQ_AvlTree = FixedCost(1)
  final val OpDesc_EQ_AvlTree = NamedDesc("EQ_AvlTree")
  final val EQ_AvlTree = OperationInfo(CostKind_EQ_AvlTree, OpDesc_EQ_AvlTree)

  final val CostKind_EQ_Box = FixedCost(1)
  final val OpDesc_EQ_Box = NamedDesc("EQ_Box")
  final val EQ_Box = OperationInfo(CostKind_EQ_Box, OpDesc_EQ_Box)

  /** Equals two CollOverArray of primitive (unboxed) type. */
  final val CostKind_EQ_COA_Prim = PerItemCost(1, 1, 64)
  final val OpDesc_EQ_COA_Prim = NamedDesc("EQ_COA_Prim")
  final val EQ_COA_Prim = OperationInfo(CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim)

  /** Equals two CollOverArray of GroupElement type. */
  final val CostKind_EQ_COA_GroupElement = PerItemCost(1, 1, 8)
  final val OpDesc_EQ_COA_GroupElement = NamedDesc("EQ_COA_GroupElement")
  final val EQ_COA_GroupElement = OperationInfo(CostKind_EQ_COA_GroupElement, OpDesc_EQ_COA_GroupElement)

  /** Equals two CollOverArray of BigInt type. */
  final val CostKind_EQ_COA_BigInt = PerItemCost(1, 1, 8)
  final val OpDesc_EQ_COA_BigInt = NamedDesc("EQ_COA_BigInt")
  final val EQ_COA_BigInt = OperationInfo(CostKind_EQ_COA_BigInt, OpDesc_EQ_COA_BigInt)

  /** Equals two CollOverArray of GroupElement type. */
  final val CostKind_EQ_COA_AvlTree = PerItemCost(1, 1, 8)
  final val OpDesc_EQ_COA_AvlTree = NamedDesc("EQ_COA_AvlTree")
  final val EQ_COA_AvlTree = OperationInfo(CostKind_EQ_COA_AvlTree, OpDesc_EQ_COA_AvlTree)

  /** Equals two CollOverArray of Box type. */
  final val CostKind_EQ_COA_Box = PerItemCost(1, 1, 8)
  final val OpDesc_EQ_COA_Box = NamedDesc("EQ_COA_Box")
  final val EQ_COA_Box = OperationInfo(CostKind_EQ_COA_Box, OpDesc_EQ_COA_Box)

  case class OperationInfo[C <: CostKind](costKind: C, opDesc: OperationDesc)

  val descriptors: AVHashMap[RType[_], (OperationInfo[FixedCost], OperationInfo[PerItemCost])] =
    AVHashMap.fromSeq(Array[(RType[_], (OperationInfo[FixedCost], OperationInfo[PerItemCost]))](
      (BigIntRType, (EQ_BigInt, EQ_COA_BigInt)),
      (GroupElementRType, (EQ_GroupElement, EQ_COA_GroupElement)),
      (AvlTreeRType, (EQ_AvlTree, EQ_COA_AvlTree)),
      (BoxRType, (EQ_Box, EQ_COA_Box))
    ))

  type COA[A] = CollOverArray[A]
  type POC[A,B] = PairOfCols[A, B]

  /** This method is specialized for numeric types and thus Scala generates four
    * specialized methods (one for each type) which implement unboxed comparison or arrays
    * in a most efficient way. This efficient implementation is reflected in the cost
    * parameters, which are part of the protocol. Thus any alternative protocol
    * implementation should implement comparison is the same way.
    */
  private def equalCOA_Prim[@sp(Byte, Short, Int, Long) A]
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

  def equalColls[A](c1: Coll[A], c2: Coll[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(CostKind_EQ_Coll, OpDesc_EQ_Coll) { () =>
      val len = c1.length
      var i = 0
      while(i < len && okEqual) {
        okEqual = equalDataValues(c1(i), c2(i))
        i += 1
      }
      i
    }
    okEqual
  }

  def equalColls_Dispatch[A](coll1: Coll[A], coll2: Coll[A])(implicit E: ErgoTreeEvaluator): Boolean = {
    coll1.tItem match {
      case ByteType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Byte]],
          coll2.asInstanceOf[COA[Byte]],
          CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim, sizeFactor = 1)

      case ShortType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Short]],
          coll2.asInstanceOf[COA[Short]],
          CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim, sizeFactor = 2)

      case IntType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Int]],
          coll2.asInstanceOf[COA[Int]],
          CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim, sizeFactor = 4)

      case LongType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Long]],
          coll2.asInstanceOf[COA[Long]],
          CostKind_EQ_COA_Prim, OpDesc_EQ_COA_Prim, sizeFactor = 8)

      case t =>
        descriptors.get(t) match {
          case Nullable((_, info)) =>
            equalCOA_Prim(
              coll1.asInstanceOf[COA[A]],
              coll2.asInstanceOf[COA[A]],
              info.costKind, info.opDesc, sizeFactor = 8)
          case _ =>
            equalColls(coll1, coll2)
        }
    }
  }

  // TODO v5.0: introduce a new limit on structural depth of data values
  def equalDataValues(l: Any, r: Any)(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual: Boolean = false
    l match {
      case coll1: Coll[a] =>
        okEqual = r match {
          case coll2: Coll[_] =>
            val len = coll1.length
            if (len != coll2.length || coll1.tItem != coll2.tItem)
              return false

            equalColls_Dispatch(coll1, coll2.asInstanceOf[Coll[a]])

          case _ => false
        }

      case tup1: Tuple2[_,_] =>
        E.addFixedCost(CostKind_EQ_Tuple, OpDesc_EQ_Tuple) {
          okEqual = r match {
            case tup2: Tuple2[_,_] =>
              equalDataValues(tup1._1, tup2._1) && equalDataValues(tup1._2, tup2._2)
            case _ => false
          }
        }

      case ge1: GroupElement =>
        E.addFixedCost(CostKind_EQ_GroupElement, OpDesc_EQ_GroupElement) {
          okEqual = ge1 == r
        }

      case bi: BigInt =>
        E.addFixedCost(CostKind_EQ_BigInt, OpDesc_EQ_BigInt) {
          okEqual = bi == r
        }

      case bi: AvlTree =>
        E.addFixedCost(CostKind_EQ_AvlTree, OpDesc_EQ_AvlTree) {
          okEqual = bi == r
        }

      case box: Box =>
        E.addFixedCost(CostKind_EQ_Box, OpDesc_EQ_Box) {
          okEqual = box == r
        }

      case _ =>
        E.addFixedCost(CostKind_EQ_Prim, OpDesc_EQ_Prim) {
          okEqual = l == r
        }
    }
    okEqual
  }

}
