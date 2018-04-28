package sigmastate.utils

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
}
