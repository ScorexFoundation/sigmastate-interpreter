package sigmastate.utils

import debox.cfor
import io.circe.Decoder
import org.ergoplatform.settings.ErgoAlgos
import scalan.{OverloadHack, RType}
import scorex.utils.Ints
import sigma.Environment
import sigmastate.eval.{CAnyValue, Colls, SigmaDsl}
import sigmastate.basics.CryptoConstants.EcPointType
import sigma.Coll
import sigma.GroupElement

import java.util
import java.util.concurrent.locks.Lock
import scala.reflect.ClassTag
import scala.util.{Either, Failure, Right, Success, Try}

object Helpers {

  /** Helper class which encapsulates a mutable value. */
  class MutableCell[T](var value: T)

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

  def xor(bas: Coll[Byte]*): Coll[Byte] = {
    require(bas.nonEmpty, "at least one argument is required")
    if (bas.length == 1) bas(0)
    else {
      val res = bas(0).toArray.clone()
      cfor(1)(_ < bas.length, _ + 1) { i =>
        xorU(res, bas(i).toArray)
      }
      Colls.fromArray(res)
    }
  }

  /** Same as `xor` but makes in-place update of the first argument (hence suffix `U`)
    * This is boxing-free version.
    * @return reference to the updated first argument to easy chaining of calls. */
  def xorU(target: Array[Byte], xss: Seq[Array[Byte]]): Array[Byte] = {
    for (xs <- xss) {
      xorU(target, xs)
    }
    target
  }

  /** Concatenates two arrays into a new resulting array.
    * All items of both arrays are copied to the result using System.arraycopy.
    */
  def concatArrays[T:ClassTag](arr1: Array[T], arr2: Array[T]): Array[T] = {
    val l1 = arr1.length
    val l2 = arr2.length
    val length: Int = l1 + l2
    val result: Array[T] = new Array[T](length)
    System.arraycopy(arr1, 0, result, 0, l1)
    System.arraycopy(arr2, 0, result, l1, l2)
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

  /** Optimized hashCode for array of bytes when it represents some hash thus it have
    * enough randomness and we can use only first 4 bytes.
    * @param id result of some hash function
    */
  @inline final def safeIdHashCode(id: Array[Byte]): Int =
    if (id != null && id.length >= 4) Ints.fromBytes(id(0), id(1), id(2), id(3))
    else util.Arrays.hashCode(id)

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

  /**
    * Executes the given block with a reentrant mutual exclusion Lock with the same basic
    * behavior and semantics as the implicit monitor lock accessed using synchronized
    * methods and statements in Java.
    *
    * Note, using this method has an advantage of having this method in a stack trace in case of
    * an exception in the block.
    * @param l lock object which should be acquired by the current thread before block can start executing
    * @param block block of code which will be executed retaining the lock
    * @return the value produced by the block
    */
  def withReentrantLock[A](l: Lock)(block: => A): A = {
    l.lock()
    val res = try
      block
    finally {
      l.unlock()
    }
    res
  }

  /** Encapsulate platform-specific logic of ensuring the value carries its precise type.
    * For JVM this is identity function.
    * For JS it can transform to AnyValue, if the type is numeric
    */
  def ensureTypeCarringValue(v: Any, tT: RType[Any]): Any =
    if (Environment.current.isJVM) v
    else { // JS
      v match {
        case _: Byte | _: Short | _: Int =>
          // this is necessary for JS where Byte, Short, Int have the same runtime class
          // and hence we need to pass the type information explicitly
          CAnyValue(v)(tT, OverloadHack.overloaded1)
        case _ => v
      }
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
