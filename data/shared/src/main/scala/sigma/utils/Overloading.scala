package sigma.utils

object Overloading {
  class Overload1

  class Overload2

  class Overload3

  implicit val overload1: Overload1 = new Overload1

  implicit val overload2: Overload2 = new Overload2

  implicit val overload3: Overload3 = new Overload3
}
