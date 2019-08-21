package special.wrappers

import scala.reflect.ClassTag
import special.SpecialPredef
import scalan.{NeverInline, RType, WrapSpec}

trait WrapSpecBase extends WrapSpec {
}

/** Wrappers spec for Option */
trait OptionWrapSpec extends WrapSpecBase {
  def get[A](xs: Option[A]): A = xs.get
  @NeverInline // TODO codegen: convertion to Thunk is required
  def getOrElse[A](xs: Option[A], default: => A): A = xs.getOrElse(default)
  def map[A,B](xs: Option[A], f: A => B): Option[B] = xs.map(f)
  def flatMap[A,B](xs: Option[A], f: A => Option[B]): Option[B] = xs.flatMap(f)
  def filter[A](xs: Option[A], f: A => Boolean): Option[A] = xs.filter(f)
  def isDefined[A](xs: Option[A]): Boolean  = xs.isDefined
  def isEmpty[A](xs: Option[A]): Boolean  = xs.isEmpty
  @NeverInline // TODO codegen: fold should have single section, and convertion to Thunk is required
  def fold[A,B](xs: Option[A], ifEmpty: =>B, f: A => B): B = xs.fold(ifEmpty)(f)
};

trait SpecialPredefWrapSpec extends WrapSpecBase {
  def loopUntil[A](s1: A, isMatch: A => Boolean, step: A => A): A = SpecialPredef.loopUntil(s1, isMatch, step)
  def cast[A](v: Any)(implicit cA: ClassTag[A]): Option[A] = SpecialPredef.cast[A](v)
  def some[A](x: A): Option[A] = SpecialPredef.some(x)
  def none[A](implicit cA: RType[A]): Option[A] = SpecialPredef.none[A]
  def optionGetOrElse[A](opt: Option[A], default: A): A = SpecialPredef.optionGetOrElse(opt, default)
}

trait RTypeWrapSpec extends WrapSpecBase {
  def name[T](d: RType[T]): String = d.name
}
