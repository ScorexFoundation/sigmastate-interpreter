package special

import scala.reflect.ClassTag
import scalan.{Reified, RType}

object SpecialPredef {
  def loopUntil[A](s1: A, isMatch: A => Boolean, step: A => A): A = {
    var res = s1
    while (!isMatch(res))
      res = step(res)
    res
  }

  def cast[T:ClassTag](v: Any): Option[T] = v match { case _: T => Some(v.asInstanceOf[T]) case _ =>  None }

  def some[A](x: A): Option[A] = Some(x)

  @Reified("A") def none[A](implicit tA: RType[A]): Option[A] = Option.empty[A]

  def optionGetOrElse[A](opt: Option[A], default: A): A = opt.getOrElse(default)

  def rewritableMethod =
    sys.error(s"Shouldn't be called, instead it should be either handled in rewrite rule, or overridden in derived class.")
}
