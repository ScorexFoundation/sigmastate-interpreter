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

  /** HOTSPOT:  it is used in deserialization so we avoid allocation by any means. */
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

    /** Absolute value of this numeric value.
      * @since 2.0
      */
    def toAbs: Byte = if (b < 0) (-b).toByte else b
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

    /** Absolute value of this numeric value.
      * @since 2.0
      */
    def toAbs: Short = if (x < 0) (-x).toShort else x
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

    /** Absolute value of this numeric value.
      * @since 2.0
      */
    def toAbs: Long = if (x < 0) -x else x
  }

  implicit class BigIntegerOps(val x: BigInteger) extends AnyVal {

    /**
      * Converts this {@code BigInteger} to a {@code long}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code long} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to a {@code long}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in a {@code long}.
      * @see BigInteger#longValue
      * @since 1.8
      */
    def toLongExact: Long = {
      if (x.bitLength <= 63) x.longValue
      else
        throw new ArithmeticException("BigInteger out of long range")
    }

    /**
      * Converts this {@code BigInteger} to an {@code int}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code int} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to an {@code int}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in an {@code int}.
      * @see BigInteger#intValue
      */
    def toIntExact: Int = {
      if (x.bitLength <= 31) x.intValue
      else
        throw new ArithmeticException("BigInteger out of int range")
    }

    /**
      * Converts this {@code BigInteger} to a {@code short}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code short} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to a {@code short}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in a {@code short}.
      * @see BigInteger#shortValue
      */
    def toShortExact: Short = {
      if (x.bitLength <= 31) {
        val value = x.intValue
        if (value >= Short.MinValue && value <= Short.MaxValue)
          return x.shortValue
      }
      throw new ArithmeticException("BigInteger out of short range")
    }

    /**
      * Converts this {@code BigInteger} to a {@code byte}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code byte} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to a {@code byte}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in a {@code byte}.
      * @see BigInteger#byteValue
      * @since 1.8
      */
    def toByteExact: Byte = {
      if (x.bitLength <= 31) {
        val value = x.intValue
        if (value >= Byte.MinValue && value <= Byte.MaxValue)
          return x.byteValue
      }
      throw new ArithmeticException("BigInteger out of byte range")
    }

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
      // Comparing with 255 is correct because bitLength() method excludes the sign bit.
      // For example, these are the boundary values:
      // (new BigInteger("80" + "00" * 31, 16)).bitLength() = 256
      // (new BigInteger("7F" + "ff" * 31, 16)).bitLength() = 255
      // (new BigInteger("-7F" + "ff" * 31, 16)).bitLength() = 255
      // (new BigInteger("-80" + "00" * 31, 16)).bitLength() = 255
      // (new BigInteger("-80" + "00" * 30 + "01", 16)).bitLength() = 256
      if (x.bitLength() <= 255) x
      else
        throw new ArithmeticException("BigInteger out of 256 bit range");
    }
  }

  implicit final class Ensuring[A](private val self: A) extends AnyVal {
    def ensuring(cond: A => Boolean, msg: A => Any): A = {
      assert(cond(self), msg(self))
      self
    }
  }
}
