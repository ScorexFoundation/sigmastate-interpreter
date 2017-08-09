package sigmastate.interpreter

/**
  * Accumulator for script cost. It has only addCost method, so cost could be increased on the way only.
  * Accumulator is providing a signal when cost is going over a limit provided, see addCost method description.
  *
  * @param initialValue - initial cost
  * @param limit - maximum
  */
@specialized
case class CostAccumulator(initialValue: Int, limit: Int) {
  require(initialValue <= limit)

  private var mutVal : Int = initialValue

  /**
    * Adds cost to a current hidden mutable value
    * @param delta to be added to the current value
    * @return either Right(value) or Left(value), where value is the updated current value after addition.
    *         Left(.) means value goes over the limit, Right(.) means okay.
    */
  def addCost(delta: Int): Either[Int, Int] = {
    mutVal = mutVal + delta
    if (mutVal <= limit) Right(limit) else Left(limit)
  }

  def value = mutVal
}
