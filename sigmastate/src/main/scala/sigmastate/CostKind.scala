package sigmastate

/** Cost descriptor of a single operation, usually associated with
  * [[sigmastate.interpreter.OperationDesc]].
  */
sealed abstract class CostKind

/** Descriptor of the simple fixed cost.
  * @param cost  given cost of the operation */
case class FixedCost(cost: Int) extends CostKind

/** Cost of operation over collection of the known length.
  * See for example [[Exists]], [[MapCollection]].
  * @param baseCost cost of operation factored out of the loop iterations
  * @param perChunkCost cost associated with each chunk of items
  * @param chunkSize number of items in a chunk
  */
case class PerItemCost(baseCost: Int, perChunkCost: Int, chunkSize: Int) extends CostKind {
  /** Compute number of chunks necessary to cover the given number of items. */
  def chunks(nItems: Int) = (nItems - 1) / chunkSize + 1

  /** Computes the cost for the given number of items. */
  def cost (nItems: Int): Int = {
    val nChunks = chunks(nItems)
    Math.addExact(baseCost, Math.multiplyExact(perChunkCost, nChunks))
  }
}

/** Descriptor of the cost which depends on type. */
abstract class TypeBasedCost extends CostKind {
  /** Returns cost value depending on the given type. */
  def costFunc(tpe: SType): Int
}

/** Cost of operation cannot be described using fixed set of parameters.
  * In this case the operation cost is a sum of sub-operation costs.
  * See [[EQ]], [[NEQ]]. */
case object DynamicCost extends CostKind



