package sigma

import debox.cfor
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigma.data.RType

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Declaration of extension methods introduced in `sigma-core` module.
  * See `implicit class ...` wrappers below.
  */
object Extensions {

  /** Extension methods for `Array[T]` where implicit descriptor `RType[T]` is also
    * required.
    */
  implicit class ArrayOps[T: RType](arr: Array[T]) {
    /** Wraps array into Coll instance. The source array in not cloned. */
    @inline def toColl: Coll[T] = Colls.fromArray(arr)
  }

  /** Extension methods for `Coll[T]`. */
  implicit class CollOps[T](val source: Coll[T]) extends AnyVal {
    /** Applies a function `f` to each element of the `source` collection. */
    def foreach(f: T => Unit) = {
      val limit = source.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(source(i))
      }
    }
  }

  /** Extension methods for `Coll[Byte]` not available for generic `Coll[T]`. */
  implicit class CollBytesOps(val source: Coll[Byte]) extends AnyVal {
    /** Returns a representation of this collection of bytes as a modifier id. */
    def toModifierId: ModifierId = {
      val bytes = source.toArray
      bytesToId(bytes)
    }

    /** Encodes this collection of bytes as a hex string. */
    def toHex: String = Base16.encode(source.toArray)
  }

  /** Extension methods for `Coll[(A,B)]`.
    * Has priority over the extensions in [[CollOps]].
    */
  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    /** Applies a function `f` to each pair of elements in the `source` collection. */
    def foreach(f: (A, B) => Unit) = {
      val b = source.builder
      val (as, bs) = b.unzip(source)
      val limit = source.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(as(i), bs(i))
      }
    }
  }

  /** Extensions methods on [[Try]]. */
  implicit class TryOps[+A](val source: Try[A]) extends AnyVal {

    /** This extension is required only for Scala 2.11 where this method is not available in the standard library.
      * Applies `fa` if this is a `Failure` or `fb` if this is a `Success`.
      * If `fb` is initially applied and throws an exception,
      * then `fa` is applied with this exception.
      *
      * @example {{{
      * val result: Try[Int] = Try { string.toInt }
      * log(result.fold(
      *   ex => "Operation failed with " + ex,
      *   v => "Operation produced value: " + v
      * ))
      * }}}
      *
      * @param fa the function to apply if this is a `Failure`
      * @param fb the function to apply if this is a `Success`
      * @return the results of applying the function
      */
    def fold[B](fa: Throwable => B, fb: A => B) = source match {
      case Success(value) =>
        try { fb(value) } catch { case NonFatal(e) => fa(e) }
      case Failure(t) => fa(t)
    }

    /**
      * Returns `Left` with `Throwable` if this is a `Failure`, otherwise returns `Right` with `Success` value.
      * This extension is required only for Scala 2.11 where this method is not available in the standard library.
      */
    def toEither: Either[Throwable, A] = source match {
      case Success(value) => Right(value)
      case Failure(t) => Left(t)
    }

    /** Maps [[Success]] or throws the exception in the `source` instance. */
    def mapOrThrow[B](f: A => B): B = source.fold(t => throw t, f)

    /** Gets the [[Success]] value or throws the exception in the `source` instance. */
    def getOrThrow: A = source.fold(t => throw t, identity)
  }

  implicit class EitherOps[+A, +B](val source: Either[A, B]) extends AnyVal {
    /** The given function is applied if this is a `Right`.
      *
      *  {{{
      *  Right(12).map(x => "flower") // Result: Right("flower")
      *  Left(12).map(x => "flower")  // Result: Left(12)
      *  }}}
      */
    def mapRight[B1](f: B => B1): Either[A, B1] = source match {
      case Right(b) => Right(f(b))
      case _        => this.asInstanceOf[Either[A, B1]]
    }

    /** Returns a `Some` containing the `Right` value
      * if it exists or a `None` if this is a `Left`.
      *
      * {{{
      * Right(12).toOption // Some(12)
      * Left(12).toOption  // None
      * }}}
      */
    def toOption: Option[B] = source match {
      case Right(value) => Some(value)
      case _ => None
    }
  }

}
