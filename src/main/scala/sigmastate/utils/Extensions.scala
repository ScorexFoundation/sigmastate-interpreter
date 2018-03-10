package sigmastate.utils

object Extensions {

  implicit class OptionOps[T](opt: Option[T]) {
    /** Elvis operator for Option. See https://en.wikipedia.org/wiki/Elvis_operator*/
    def ?:(whenNone: => T): T = if (opt.isDefined) opt.get else whenNone
  }
}
