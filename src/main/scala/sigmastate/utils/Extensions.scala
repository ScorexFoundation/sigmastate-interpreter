package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.SType
import sigmastate.Values.{SValue, Value}
import sigmastate.serialization.{TypeSerializer, ValueSerializer}

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

object Extensions {

  implicit class ByteOps(b: Byte) {
    @inline def toUByte: Int = b & 0xFF
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
  }

  implicit class ShortOps(x: Short) {
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

  implicit class IntOps(x: Int) {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }
  }

  implicit class LongOps(x: Long) {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }
  }

  implicit class OptionOps[T](opt: Option[T]) {
    /** Elvis operator for Option. See https://en.wikipedia.org/wiki/Elvis_operator*/
    def ?:(whenNone: => T): T = if (opt.isDefined) opt.get else whenNone
  }

  implicit class TraversableOps[A, Source[X] <: Traversable[X]](xs: Source[A]) {

    /** Applies 'f' to elements of 'xs' until 'f' returns Some(b),
      * which is immediately returned as result of this method.
      * If not such element found, returns None as result. */
    def findMap[B](f: A => Option[B]): Option[B] = {
      for (x <- xs) {
        val y = f(x)
        if (y.isDefined) return y
      }
      None
    }

    def cast[B:ClassTag](implicit cbf: CanBuildFrom[Source[A], B, Source[B]]): Source[B] = {
      for (x <- xs) {
        assert(x match { case _: B => true case _ => false}, s"Value $x doesn't conform to type ${reflect.classTag[B]}")
      }
      xs.asInstanceOf[Source[B]]
    }

    def filterMap[B](f: A => Option[B])(implicit cbf: CanBuildFrom[Source[A], B, Source[B]]): Source[B] = {
      val b = cbf()
      for (x <- xs) {
        f(x) match {
          case Some(y) =>
            b += y
          case None =>
        }
      }
      b.result()
    }
  }

  implicit class ByteArrayBuilderOps(b: ByteArrayBuilder) {
    def appendOption[T](opt: Option[T])(putValue: T => Unit): ByteArrayBuilder = {
      opt match {
        case Some(v) =>
          b.append(1.toByte)
          putValue(v)
          b
        case None =>
          b.append(0.toByte)
      }
    }
  }

  implicit class ByteBufferOps(buf: ByteBuffer) {
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

  implicit class ByteWriterOps(w: ByteWriter) {
    @inline def putType[T <: SType](x: T): ByteWriter = { TypeSerializer.serialize(x, w); w }
    @inline def putValue[T <: SType](x: Value[T]): ByteWriter = { ValueSerializer.serialize(x, w); w }
  }

  implicit class ByteReaderOps(r: ByteReader) {
    @inline def getType(): SType = TypeSerializer.deserialize(r)
    @inline def getValue(): SValue = ValueSerializer.deserialize(r)
  }

}

