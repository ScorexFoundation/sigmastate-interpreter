package sigma.eval

import sigma.{AvlTree, Coll, Context}
import sigma.ast.{Constant, FixedCost, MethodCall, OperationCostInfo, OperationDesc, PerItemCost, SType, TypeBasedCost}
import sigma.data.KeyValueColl

abstract class ErgoTreeEvaluator {
  def addSeqCost[R](costKind: PerItemCost, nItems: Int, opDesc: OperationDesc)
      (block: () => R): R

  def addSeqCost[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
      (block: () => R): R

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

  /** Implements evaluation of AvlTree.contains method call ErgoTree node. */
  def contains_eval(
      mc: MethodCall,
      tree: AvlTree,
      key: Coll[Byte],
      proof: Coll[Byte]): Boolean

  /** Implements evaluation of AvlTree.get method call ErgoTree node. */
  def get_eval(
      mc: MethodCall,
      tree: AvlTree,
      key: Coll[Byte],
      proof: Coll[Byte]): Option[Coll[Byte]]

  /** Implements evaluation of AvlTree.getMany method call ErgoTree node. */
  def getMany_eval(
      mc: MethodCall,
      tree: AvlTree,
      keys: Coll[Coll[Byte]],
      proof: Coll[Byte]): Coll[Option[Coll[Byte]]]

  /** Implements evaluation of AvlTree.insert method call ErgoTree node. */
  def insert_eval(
      mc: MethodCall,
      tree: AvlTree,
      entries: KeyValueColl,
      proof: Coll[Byte]): Option[AvlTree]

  /** Implements evaluation of AvlTree.update method call ErgoTree node. */
  def update_eval(
      mc: MethodCall, tree: AvlTree,
      operations: KeyValueColl, proof: Coll[Byte]): Option[AvlTree]

  /** Implements evaluation of AvlTree.remove method call ErgoTree node. */
  def remove_eval(
      mc: MethodCall, tree: AvlTree,
      operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree]
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]
}