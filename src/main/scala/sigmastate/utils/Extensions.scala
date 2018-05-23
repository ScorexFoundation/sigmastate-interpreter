package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer
import sigmastate.lang.Terms._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

object Extensions {

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
        assert(x match { case _: B => true case _ => false})
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
    def appendValue[T <: SType](v: Value[T]): ByteArrayBuilder = {
      val bytes = ValueSerializer.serialize(v)
      b.append(bytes)
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
    def getValue[T <: SType]: Value[T] = {
      val (obj, consumed) = ValueSerializer.deserialize(buf.array(), buf.position())
      buf.position(buf.position() + consumed)
      obj.asValue[T]
    }
  }

}
