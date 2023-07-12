package sigmastate

/** Descriptor of a runtime platform. Can be used to conditionally execute code. */
sealed trait RuntimePlatform {
  /** Platform code (unique for each platform) */
  def code: Int
}

object RuntimePlatform {
  case object JVM extends RuntimePlatform {
    val code = 1
  }

  case object JS extends RuntimePlatform {
    val code = 2
  }
}