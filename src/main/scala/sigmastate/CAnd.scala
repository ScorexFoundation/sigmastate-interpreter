package sigmastate


case class CAnd(props: SigmaProposition*) extends SigmaProposition {
  override val code = CAnd.Code
}

object CAnd {
  val Code = 100: Byte
}