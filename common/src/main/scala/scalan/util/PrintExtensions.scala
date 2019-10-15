package scalan.util

/** Helper methods for pretty printing of collections and optional values. */
object PrintExtensions {
  implicit class AnyExtension[A](x: A) {
      def when(p: A => Boolean, show: A => String, default: String = "") = if (p(x)) show(x) else default
  }
  implicit class IterableExtensions[A](val it: Iterable[A]) extends AnyVal
  {
    def opt(show: Iterable[A] => String = _.mkString, default: String = ""): String =
      if (it.isEmpty) default else show(it)

    def rep(show: A => String = _.toString, sep: String = ", "): String = it.map(show).mkString(sep)

    def asTypeParams(show: A => String = _.toString) =
      if (it.nonEmpty) it.map(show).mkString("[", ", ", "]") else ""

    def optList(start: String, end: String, sep: String = ", ", show: A => String = _.toString) =
      if (it.nonEmpty) it.map(show).mkString(start, sep, end) else ""
  }

  implicit class OptionExtensions[A](val opt: Option[A]) extends AnyVal {
    def opt(show: A => String = _.toString, default: String = ""): String = opt match {
      case None => default
      case Some(a) => show(a)
    }
    def ifDefined(value: String): String = if (opt.isDefined) value else ""
  }

  implicit class BooleanExtensions(val opt: Boolean) extends AnyVal {
    def opt(show: => String, default: => String = ""): String = if(opt) show else default
  }

  def join(xs: Any*) = xs.map {
    case x: Iterable[_] => x.rep()
    case x => x.toString
  }.filter(_.nonEmpty).rep()
}
