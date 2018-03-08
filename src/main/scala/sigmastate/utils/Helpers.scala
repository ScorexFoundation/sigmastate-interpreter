package sigmastate.utils

object Helpers {
  def xor(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = ba1.zip(ba2).map(t => (t._1 ^ t._2).toByte)

  def xor(bas: Array[Byte]*): Array[Byte] =
    bas.reduce({case (ba, ba1) => xor(ba, ba1)}: ((Array[Byte], Array[Byte]) => Array[Byte]))
}

object Overloading {
  class Overload1
  class Overload2
  class Overload3

  implicit val overload1: Overload1 = new Overload1
  implicit val overload2: Overload2 = new Overload2
  implicit val overload3: Overload3 = new Overload3
}