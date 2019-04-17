package sigmastate

object Operations {

  object FoldInfo {
    private val method = SMethod.fromIds(SCollection.typeId, 5)
    val thisArg = method.argInfo("this")
    val zeroArg = method.argInfo("zero")
    val opArg = method.argInfo("op")
  }

}
