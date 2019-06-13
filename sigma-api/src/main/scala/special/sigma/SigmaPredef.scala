package special.sigma

object SigmaPredef {

  // TODO cleanup: since it is not really used
  def dataSize[T](v: T): Long = v match {
    case _: Boolean => 1
    case _: Byte => 1
    case _: Short => 2
    case _: Int => 4
    case _: Long => 8
    case _ => sys.error(s"Cannot compute dataSize($v)")
  }
  
}


