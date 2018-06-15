package sigmastate.utils

import java.util

import scala.reflect.ClassTag

object Helpers {
  def xor(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = ba1.zip(ba2).map(t => (t._1 ^ t._2).toByte)

  def xor(bas: Array[Byte]*): Array[Byte] =
    bas.reduce({case (ba, ba1) => xor(ba, ba1)}: ((Array[Byte], Array[Byte]) => Array[Byte]))

  def anyOf(arr: Array[Boolean]): Boolean = arr.exists(identity)
  def allOf(arr: Array[Boolean]): Boolean = arr.forall(identity)

  def concatBytes(seq: Traversable[Array[Byte]]): Array[Byte] = {
    val length: Int = seq.foldLeft(0)((acc, arr) => acc + arr.length)
    val result: Array[Byte] = new Array[Byte](length)
    var pos: Int = 0
    seq.foreach{ array =>
      System.arraycopy(array, 0, result, pos, array.length)
      pos += array.length
    }
    result
  }

  def concatArrays[T:ClassTag](seq: Traversable[Array[T]]): Array[T] = {
    val length: Int = seq.foldLeft(0)((acc, arr) => acc + arr.length)
    val result: Array[T] = new Array[T](length)
    var pos: Int = 0
    seq.foreach{ array =>
      System.arraycopy(array, 0, result, pos, array.length)
      pos += array.length
    }
    result
  }

  def deepHashCode[T](arr: Array[T]): Int = arr match {
    case arr: Array[AnyRef] => util.Arrays.deepHashCode(arr)
    case arr: Array[Byte] => util.Arrays.hashCode(arr)
    case arr: Array[Short] => util.Arrays.hashCode(arr)
    case arr: Array[Int] => util.Arrays.hashCode(arr)
    case arr: Array[Long] => util.Arrays.hashCode(arr)
    case arr: Array[Char] => util.Arrays.hashCode(arr)
    case arr: Array[Float] => util.Arrays.hashCode(arr)
    case arr: Array[Double] => util.Arrays.hashCode(arr)
    case arr: Array[Boolean] => util.Arrays.hashCode(arr)
  }

  def optionArrayEquals[A](maybeA1: Option[Array[A]], maybeA2: Option[Array[A]]): Boolean =
    (maybeA1, maybeA2) match {
      case (None, None) => true
      case (Some(a1), Some(a2)) => deepHashCode(a1) == deepHashCode(a2)
      case _ => false
    }
}

object Overloading {
  class Overload1
  class Overload2
  class Overload3

  implicit val overload1: Overload1 = new Overload1
  implicit val overload2: Overload2 = new Overload2
  implicit val overload3: Overload3 = new Overload3
}
