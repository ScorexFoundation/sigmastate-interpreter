package scalan.util

import java.math.BigInteger
import java.nio.ByteBuffer

import scala.language.higherKinds

object Extensions {
  implicit class BooleanOps(val b: Boolean) extends AnyVal {
    /** Convert true to 1 and false to 0
      * @since 2.0
      */
    def toByte: Byte = if (b) 1 else 0
  }

  /** @hotspot  it is used in deserialization so we avoid allocation by any means. */
  @inline final def toUByte(b: Byte) = b & 0xFF

  implicit class ByteOps(val b: Byte) extends AnyVal {
    @inline def toUByte: Int = Extensions.toUByte(b)

    def addExact(b2: Byte): Byte = {
      val r = b + b2
      if (r < Byte.MinValue || r > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      r.toByte
    }

    def subtractExact(b2: Byte): Byte = {
      val r = b - b2
      if (r < Byte.MinValue || r > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      r.toByte
    }

    def multiplyExact(b2: Byte): Byte = {
      val r = b * b2
      if (r < Byte.MinValue || r > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      r.toByte
    }

    def toShort: Short = b.toShort
    def toInt: Int = b.toInt
    def toLong: Long = b.toLong

    /** Absolute value of this numeric value.
      * @since 2.0
      */
    def toAbs: Byte = if (b < 0) (-b).toByte else b

    /** Compares this numeric with that numeric for order. Returns a negative integer, zero, or a positive integer as the
      * `this` is less than, equal to, or greater than `that`.
      */
    def compare(that: Byte): Byte = if (b < that) -1.toByte else if (b == that) 0.toByte else 1.toByte
  }

  implicit class ShortOps(val x: Short) extends AnyVal {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }

    def addExact(y: Short): Short = {
      val r = x + y
      if (r < Short.MinValue || r > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      r.toShort
    }

    def subtractExact(y: Short): Short = {
      val r = x - y
      if (r < Short.MinValue || r > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      r.toShort
    }

    def multiplyExact(y: Short): Short = {
      val r = x * y
      if (r < Short.MinValue || r > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      r.toShort
    }
  }

  implicit class IntOps(val x: Int) extends AnyVal {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }

    def toShortExact: Short = {
      if (x < Short.MinValue || x > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      x.toShort
    }

    /** Absolute value of this numeric value.
      * @since 2.0
      */
    def toAbs: Int = if (x < 0) -x else x

    /** Compares this numeric with that numeric for order. Returns a negative integer, zero, or a positive integer as the
      * `this` is less than, equal to, or greater than `that`.
      */
    def compare(that: Int): Int = if (x < that) -1 else if (x == that) 0 else 1
  }

  implicit class LongOps(val x: Long) extends AnyVal {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }

    def toShortExact: Short = {
      if (x < Short.MinValue || x > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      x.toShort
    }

    def toIntExact: Int = {
      if (x < Int.MinValue || x > Int.MaxValue)
        throw new ArithmeticException("Int overflow")
      x.toInt
    }
  }

  implicit class BigIntegerOps(val x: BigInteger) extends AnyVal {
    /** Returns this `mod` Q, i.e. remainder of division by Q, where Q is an order of the cryprographic group.
      * @since 2.0
      */
    def modQ: BigInt = ???

    /** Adds this number with `other` by module Q.
      * @since 2.0
      */
    def plusModQ(other: BigInt): BigInt = ???

    /** Subracts this number with `other` by module Q.
      * @since 2.0
      */
    def minusModQ(other: BigInt): BigInt = ???

    /** Multiply this number with `other` by module Q.
      * @since 2.0
      */
    def multModQ(other: BigInt): BigInt = ???

    /** Multiply this number with `other` by module Q.
      * @since Mainnet
      */
    def multInverseModQ: BigInt = ???

    /** Checks this {@code BigInteger} can be cust to 256 bit two's-compliment representation,
      * checking for lost information. If the value of this {@code BigInteger}
      * is out of the range of the 256 bits, then an {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} if the check is successful
      * @throws ArithmeticException if the value of {@code this} will
      * not exactly fit in a 256 bit range.
      * @see BigInteger#longValueExact
      */
    @inline final def to256BitValueExact: BigInteger = {
      if (x.bitLength() <= 255) x
      else
        throw new ArithmeticException("BigInteger out of 256 bit range");
    }
  }

  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal {
    /** Elvis operator for Option. See https://en.wikipedia.org/wiki/Elvis_operator*/
    def ?:(whenNone: => T): T = if (opt.isDefined) opt.get else whenNone
  }

  implicit class ProductOps(val source: Product) extends AnyVal {
    def toArray: Array[Any] = {
      val arity = source.productArity
      val res = new Array[Any](arity)
      var i = 0
      while (i < arity) {
        res(i) = source.productElement(i)
        i += 1
      }
      res
    }
  }

  implicit class ByteBufferOps(val buf: ByteBuffer) extends AnyVal {
    def toBytes: Array[Byte] = {
      val res = new Array[Byte](buf.position())
      buf.array().copyToArray(res, 0, res.length)
      res
    }
    def getBytes(size: Int): Array[Byte] = {
      val res = new Array[Byte](size)
      buf.get(res)
      res
    }
    def getOption[T](getValue: => T): Option[T] = {
      val tag = buf.get()
      if (tag != 0)
        Some(getValue)
      else
        None
    }
  }
}
