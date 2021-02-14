package sigmastate

import scalan.{AVHashMap, Nullable, RType}
import scalan.RType._
import sigmastate.Values.{PerItemCost, FixedCost}
import spire.sp
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.interpreter.ErgoTreeEvaluator.{OperationCostInfo, NamedDesc}
import special.sigma.{Header, HeaderRType, Box, GroupElementRType, GroupElement, AvlTreeRType, BigInt, BoxRType, AvlTree, BigIntRType, PreHeader, PreHeaderRType}
import special.collection.{Coll, PairOfCols, CollOverArray}
/** Implementation of data equality for two arbitrary ErgoTree data types.
  * @see [[DataValueComparer.equalDataValues]]
  */
object DataValueComparer {

  /** NOTE: The cost of most equality operations depends on the position in `match` statement.
    * Thus the full cost to compare x and y equals DispatchCost * OperationCost, where
    * DispatchCost = CasePosition * CostOf_MatchType,
    * OperationCost is the type specific cost.
    * For this reason reordering of cases may lead to divergence between an estimated and
    * the actual execution cost (time).
    * The constants are part of the consensus procotol and cannot be changed without forking.
    */
  final val CostOf_MatchType = 1
  final val CostKind_MatchType = FixedCost(CostOf_MatchType)
  final val OpDesc_MatchType = NamedDesc("MatchType")
  final val MatchType = OperationCostInfo(CostKind_MatchType, OpDesc_MatchType)

  final val CostKind_EQ_Prim = FixedCost(3)         // case 1
  final val OpDesc_EQ_Prim = NamedDesc("EQ_Prim")
  final val EQ_Prim = OperationCostInfo(CostKind_EQ_Prim, OpDesc_EQ_Prim)


  /** Equals two Colls of non-primitive (boxed) types.
    */
  final val CostKind_EQ_Coll = PerItemCost(3, 0, 1)    // case 2
  final val OpDesc_EQ_Coll = NamedDesc("EQ_Coll")
  final val EQ_Coll = OperationCostInfo(CostKind_EQ_Coll, OpDesc_EQ_Coll)

  /** NOTE: In the formula `(2 + 1)` the 1 corresponds to the second type match. */
  final val CostKind_EQ_Tuple = FixedCost(3)  // case 3
  final val OpDesc_EQ_Tuple = NamedDesc("EQ_Tuple")
  final val EQ_Tuple = OperationCostInfo(CostKind_EQ_Tuple, OpDesc_EQ_Tuple)

  /** NOTE: the value is set based on benchmarking of SigmaDslSpecification. */
  final val CostKind_EQ_GroupElement = FixedCost(188) // case 4
  final val OpDesc_EQ_GroupElement = NamedDesc("EQ_GroupElement")
  final val EQ_GroupElement = OperationCostInfo(CostKind_EQ_GroupElement, OpDesc_EQ_GroupElement)

  final val CostKind_EQ_BigInt = FixedCost(4)       // case 5
  final val OpDesc_EQ_BigInt = NamedDesc("EQ_BigInt")
  final val EQ_BigInt = OperationCostInfo(CostKind_EQ_BigInt, OpDesc_EQ_BigInt)

  final val CostKind_EQ_AvlTree = FixedCost(2 + (6 * CostOf_MatchType) / 2)      // case 6
  final val OpDesc_EQ_AvlTree = NamedDesc("EQ_AvlTree")
  final val EQ_AvlTree = OperationCostInfo(CostKind_EQ_AvlTree, OpDesc_EQ_AvlTree)

  // TODO JITC: update value after serialization is avoided to compute ErgoBox.id
  final val CostKind_EQ_Box = FixedCost(6)          // case 7
  final val OpDesc_EQ_Box = NamedDesc("EQ_Box")
  final val EQ_Box = OperationCostInfo(CostKind_EQ_Box, OpDesc_EQ_Box)

  /** NOTE: In the formula `(7 + 1)` the 1 corresponds to the second type match. */
  final val CostKind_EQ_Option = FixedCost(1 + (7 + 1) * CostOf_MatchType / 2 - 1) // case 8
  final val OpDesc_EQ_Option = NamedDesc("EQ_Option")
  final val EQ_Option = OperationCostInfo(CostKind_EQ_Option, OpDesc_EQ_Option)

  final val CostKind_EQ_PreHeader = FixedCost(8) // case 9
  final val OpDesc_EQ_PreHeader = NamedDesc("EQ_PreHeader")
  final val EQ_PreHeader = OperationCostInfo(CostKind_EQ_PreHeader, OpDesc_EQ_PreHeader)

  final val CostKind_EQ_Header = FixedCost(12) // case 10
  final val OpDesc_EQ_Header = NamedDesc("EQ_Header")
  final val EQ_Header = OperationCostInfo(CostKind_EQ_Header, OpDesc_EQ_Header)

  /** Equals two CollOverArray of Byte type. */
  final val CostKind_EQ_COA_Byte = PerItemCost(1, 1, 128)
  final val OpDesc_EQ_COA_Byte = NamedDesc("EQ_COA_Byte")
  final val EQ_COA_Byte = OperationCostInfo(CostKind_EQ_COA_Byte, OpDesc_EQ_COA_Byte)
  
  /** Equals two CollOverArray of Short type. */
  final val CostKind_EQ_COA_Short = PerItemCost(1, 1, 96)
  final val OpDesc_EQ_COA_Short = NamedDesc("EQ_COA_Short")
  final val EQ_COA_Short = OperationCostInfo(CostKind_EQ_COA_Short, OpDesc_EQ_COA_Short)

  /** Equals two CollOverArray of Int type. */
  final val CostKind_EQ_COA_Int = PerItemCost(1, 1, 64)
  final val OpDesc_EQ_COA_Int = NamedDesc("EQ_COA_Int")
  final val EQ_COA_Int = OperationCostInfo(CostKind_EQ_COA_Int, OpDesc_EQ_COA_Int)

  /** Equals two CollOverArray of Long type. */
  final val CostKind_EQ_COA_Long = PerItemCost(1, 1, 48)
  final val OpDesc_EQ_COA_Long = NamedDesc("EQ_COA_Long")
  final val EQ_COA_Long = OperationCostInfo(CostKind_EQ_COA_Long, OpDesc_EQ_COA_Long)

  /** Equals two CollOverArray of GroupElement type. */
  final val CostKind_EQ_COA_GroupElement = PerItemCost(1, 1, 1)
  final val OpDesc_EQ_COA_GroupElement = NamedDesc("EQ_COA_GroupElement")
  final val EQ_COA_GroupElement = OperationCostInfo(CostKind_EQ_COA_GroupElement, OpDesc_EQ_COA_GroupElement)

  /** Equals two CollOverArray of BigInt type. */
  final val CostKind_EQ_COA_BigInt = PerItemCost(1, 1, 5)
  final val OpDesc_EQ_COA_BigInt = NamedDesc("EQ_COA_BigInt")
  final val EQ_COA_BigInt = OperationCostInfo(CostKind_EQ_COA_BigInt, OpDesc_EQ_COA_BigInt)

  /** Equals two CollOverArray of AvlTree type. */
  final val CostKind_EQ_COA_AvlTree = PerItemCost(1, 1, 2)
  final val OpDesc_EQ_COA_AvlTree = NamedDesc("EQ_COA_AvlTree")
  final val EQ_COA_AvlTree = OperationCostInfo(CostKind_EQ_COA_AvlTree, OpDesc_EQ_COA_AvlTree)

  // TODO JITC: update value after serialization is avoided to compute ErgoBox.id
  /** Equals two CollOverArray of Box type. */
  final val CostKind_EQ_COA_Box = PerItemCost(1, 1, 1)
  final val OpDesc_EQ_COA_Box = NamedDesc("EQ_COA_Box")
  final val EQ_COA_Box = OperationCostInfo(CostKind_EQ_COA_Box, OpDesc_EQ_COA_Box)

  /** Equals two CollOverArray of PreHeader type. */
  final val CostKind_EQ_COA_PreHeader = PerItemCost(1, 2, 1)
  final val OpDesc_EQ_COA_PreHeader = NamedDesc("EQ_COA_PreHeader")
  final val EQ_COA_PreHeader = OperationCostInfo(CostKind_EQ_COA_PreHeader, OpDesc_EQ_COA_PreHeader)

  /** Equals two CollOverArray of Header type. */
  final val CostKind_EQ_COA_Header = PerItemCost(1, 5, 1)
  final val OpDesc_EQ_COA_Header = NamedDesc("EQ_COA_Header")
  final val EQ_COA_Header = OperationCostInfo(CostKind_EQ_COA_Header, OpDesc_EQ_COA_Header)

  val descriptors: AVHashMap[RType[_], (OperationCostInfo[FixedCost], OperationCostInfo[PerItemCost])] =
    AVHashMap.fromSeq(Array[(RType[_], (OperationCostInfo[FixedCost], OperationCostInfo[PerItemCost]))](
      (BigIntRType, (EQ_BigInt, EQ_COA_BigInt)),
      (GroupElementRType, (EQ_GroupElement, EQ_COA_GroupElement)),
      (AvlTreeRType, (EQ_AvlTree, EQ_COA_AvlTree)),
      (BoxRType, (EQ_Box, EQ_COA_Box)),
      (PreHeaderRType, (EQ_PreHeader, EQ_COA_PreHeader)),
      (HeaderRType, (EQ_Header, EQ_COA_Header))
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
                   (c1: COA[A], c2: COA[A], costInfo: OperationCostInfo[PerItemCost])
                   (implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual = true
    E.addSeqCost(costInfo.costKind, costInfo.opDesc) { () =>
      val len = c1.length
      var i = 0
      val a1 = c1.toArray
      val a2 = c2.toArray
      while (i < len && okEqual) {
        okEqual = a1(i) == a2(i)
        i += 1
      }
      i  // return the number of actually compared items
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
          coll2.asInstanceOf[COA[Byte]], EQ_COA_Byte)

      case ShortType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Short]],
          coll2.asInstanceOf[COA[Short]], EQ_COA_Short)

      case IntType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Int]],
          coll2.asInstanceOf[COA[Int]], EQ_COA_Int)

      case LongType =>
        equalCOA_Prim(
          coll1.asInstanceOf[COA[Long]],
          coll2.asInstanceOf[COA[Long]], EQ_COA_Long)

      case t =>
        descriptors.get(t) match {
          case Nullable((_, info)) =>
            equalCOA_Prim(
              coll1.asInstanceOf[COA[A]],
              coll2.asInstanceOf[COA[A]], info)
          case _ =>
            equalColls(coll1, coll2)
        }
    }
  }

  // TODO v5.0: introduce a new limit on structural depth of data values
  def equalDataValues(l: Any, r: Any)(implicit E: ErgoTreeEvaluator): Boolean = {
    var okEqual: Boolean = false
    l match {
      case _: java.lang.Number => /** case 1 (see [[EQ_Prim]]) */
        E.addFixedCost(EQ_Prim) {
          okEqual = l == r
        }

      case coll1: Coll[a] =>  /** case 2 (see [[EQ_Coll]]) */
        E.addCost(MatchType) // for second match below
        okEqual = r match {
          case coll2: Coll[_] =>
            val len = coll1.length
            if (len != coll2.length || coll1.tItem != coll2.tItem)
              return false

            equalColls_Dispatch(coll1, coll2.asInstanceOf[Coll[a]])

          case _ => false
        }

      case tup1: Tuple2[_,_] => /** case 3 (see [[EQ_Tuple]]) */
        E.addFixedCost(EQ_Tuple) {
          okEqual = r match {
            case tup2: Tuple2[_,_] =>
              equalDataValues(tup1._1, tup2._1) && equalDataValues(tup1._2, tup2._2)
            case _ => false
          }
        }

      case ge1: GroupElement => /** case 4 (see [[EQ_GroupElement]]) */
        E.addFixedCost(EQ_GroupElement) {
          okEqual = ge1 == r
        }

      case bi: BigInt => /** case 5 (see [[EQ_BigInt]]) */
        E.addFixedCost(EQ_BigInt) {
          okEqual = bi == r
        }

      case bi: AvlTree =>  /** case 6 (see [[EQ_AvlTree]]) */
        E.addFixedCost(EQ_AvlTree) {
          okEqual = bi == r
        }

      case box: Box => /** case 7 (see [[EQ_Box]]) */
        E.addFixedCost(EQ_Box) {
          okEqual = box == r
        }

      case opt1: Option[_] => /** case 8 (see [[EQ_Option]]) */
        E.addFixedCost(EQ_Option) {
          okEqual = r match {
            case opt2: Option[_] =>
              if (opt1.isDefined) {
                if (opt2.isDefined) {
                  equalDataValues(opt1.get, opt2.get)
                } else
                  false // right is not Some
              } else {
                // here left in None
                opt2.isEmpty  // return if the right is also None
              }
            case _ =>
              false // right is not an Option
          }
        }
      case ph: PreHeader =>  /** case 9 (see [[EQ_PreHeader]]) */
        E.addFixedCost(EQ_PreHeader) {
          okEqual = ph == r
        }
      case h: Header =>  /** case 10 (see [[EQ_Header]]) */
        E.addFixedCost(EQ_Header) {
          okEqual = h == r
        }
      case _ =>
        ErgoTreeEvaluator.error(s"Cannot compare $l and $r: unknown type")
    }
    okEqual
  }

}
