package sigmastate


case class COr(statements: SigmaProposition*) extends SigmaProposition {
  override val code = COr.Code
}

object COr {
  val Code = 101: Byte
}