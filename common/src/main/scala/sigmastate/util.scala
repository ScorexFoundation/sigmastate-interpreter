package sigmastate

import scala.reflect.ClassTag

object util {
  /** Maximum length of an allocatable array. */
  val MaxArrayLength: Int = 100000

  /** Allocates a new array with `len` items of type `A`. */
  final def safeNewArray[A](len: Int)(implicit tA: ClassTag[A]): Array[A] = {
    if (len > MaxArrayLength)
      throw new RuntimeException(
        s"Cannot allocate array of $tA with $len items: max limit is $MaxArrayLength")
    new Array[A](len)
  }
}
