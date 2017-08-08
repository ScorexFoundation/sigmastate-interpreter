package sigmastate.interpreter

@specialized
case class CostAccumulator(initialValue: Int, limit: Int) {
  require(initialValue <= limit)
  private var value: Int = initialValue

  def addCost(delta: Int): Either[Int, Int] = {
    value = value + delta
    if (value <= limit) Right(limit) else Left(limit)
  }
}
