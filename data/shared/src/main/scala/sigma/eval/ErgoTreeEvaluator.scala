package sigma.eval

import sigma.{AvlTree, Coll, Context}
import sigma.ast.{Constant, FixedCost, OperationCostInfo, OperationDesc, PerItemCost, SType, TypeBasedCost}

abstract class ErgoTreeEvaluator {
  def addSeqCost[R](costKind: PerItemCost, nItems: Int, opDesc: OperationDesc)(block: () => R): R
  def addSeqCost[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)(block: () => R): R
  def addSeqCost(costKind: PerItemCost, opDesc: OperationDesc)(block: () => Int): Unit
  def addSeqCost(costInfo: OperationCostInfo[PerItemCost])(block: () => Int): Unit
  def addCost(costKind: FixedCost, opDesc: OperationDesc): Unit
  def addCost(costInfo: OperationCostInfo[FixedCost]): Unit
  def addFixedCost(costKind: FixedCost, opDesc: OperationDesc)(block: => Unit): Unit
  def addFixedCost(costInfo: OperationCostInfo[FixedCost])(block: => Unit): Unit
  def addSeqCostNoOp(costKind: PerItemCost, nItems: Int, opDesc: OperationDesc): Unit
  def addTypeBasedCost[R](
      costKind: TypeBasedCost,
      tpe: SType, opDesc: OperationDesc)(block: () => R): R
  def settings: EvalSettings
  def profiler: Profiler
  def constants: Seq[Constant[SType]]
  def context: Context
  def createTreeVerifier(tree: AvlTree, proof: Coll[Byte]): AvlTreeVerifier
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]
}