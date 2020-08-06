package sigmastate.utils

import java.util

import io.circe.Decoder
import org.ergoplatform.settings.ErgoAlgos
import sigmastate.eval.{Colls, SigmaDsl}
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.Coll
import special.sigma.GroupElement

import scala.reflect.ClassTag
import scala.util.{Failure, Try, Either, Success, Right}

object Helpers {
  def xor(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = ba1.zip(ba2).map(t => (t._1 ^ t._2).toByte)

  /** Same as `xor` but makes in-place update of the first argument (hence suffix `U`)
    * This is boxing-free version.
    * @return reference to the updated first argument to easy chaining of calls. */
  def xorU(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = {
    var i = 0
    while (i < ba1.length) {
      ba1(i) = (ba1(i) ^ ba2(i)).toByte
      i += 1
    }
    ba1
  }

  def xor(bas: Array[Byte]*): Array[Byte] =
    bas.reduce({case (ba, ba1) => xor(ba, ba1)}: ((Array[Byte], Array[Byte]) => Array[Byte]))

  /** Same as `xor` but makes in-place update of the first argument (hence suffix `U`)
    * This is boxing-free version.
    * @return reference to the updated first argument to easy chaining of calls. */
  def xorU(target: Array[Byte], xss: Seq[Array[Byte]]): Array[Byte] = {
    for (xs <- xss) {
      xorU(target, xs)
    }
    target
  }

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

  def concatArrays[T:ClassTag](arr1: Array[T], arr2: Array[T]): Array[T] = {
    val length: Int = arr1.length + arr2.length
    val result: Array[T] = new Array[T](length)
    var pos: Int = 0
    System.arraycopy(arr1, 0, result, 0, arr1.length)
    System.arraycopy(arr2, 0, result, arr1.length, arr2.length)
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

  def castArray[A, B >: A : ClassTag](array: Array[A]): Array[B] = {
    val result: Array[B] = new Array[B](array.length)
    System.arraycopy(array, 0, result, 0, array.length)
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

  implicit class TryOps[+A](val source: Try[A]) extends AnyVal {
    def fold[B](onError: Throwable => B, onSuccess: A => B) = source match {
      case Success(value) => onSuccess(value)
      case Failure(t) => onError(t)
    }
    def toEither: Either[Throwable, A] = source match {
      case Success(value) => Right(value)
      case Failure(t) => Left(t)
    }
    def mapOrThrow[B](f: A => B): B = source.fold(t => throw t, f)
    def getOrThrow: A = source.fold(t => throw t, identity)
  }

  implicit class DecoderResultOps[A](val source: Decoder.Result[A]) extends AnyVal {
    def toTry: Try[A] = source match {
      case Right(value) => Success(value)
      case Left(t) => Failure(t)
    }
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
  }

  /** Decodes the given hex string into byte array and then uses
    * [[SigmaDsl.decodePoint()]] to construct [[GroupElement]] instance.
    */
  def decodeGroupElement(hexString: String): GroupElement = {
    val bytes = ErgoAlgos.decodeUnsafe(hexString)
    SigmaDsl.decodePoint(Colls.fromArray(bytes))
  }

  /** Decodes the given hex string into [[GroupElement]] and then extracts the underlying
    * [[EcPointType]] instance
    */
  def decodeECPoint(hexString: String): EcPointType = {
    val ge = decodeGroupElement(hexString)
    SigmaDsl.toECPoint(ge).asInstanceOf[EcPointType]
  }

  /** Decodes the given hex string into a collection of bytes. */
  def decodeBytes(base16String: String): Coll[Byte] = {
    val bytes = ErgoAlgos.decodeUnsafe(base16String)
    Colls.fromArray(bytes)
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
