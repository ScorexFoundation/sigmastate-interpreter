package wrappers.special

import scalan._
import impl._
import special.wrappers.WrappersModule
import special.wrappers.SpecialPredefWrapSpec
import scala.collection.mutable.WrappedArray
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSpecialPredefsDefs extends scalan.Scalan with WSpecialPredefs {
  self: WrappersModule =>
import WOption._
import WSpecialPredef._

object WSpecialPredef extends EntityObject("WSpecialPredef") {
  private val WSpecialPredefClass = classOf[WSpecialPredef]

  // entityAdapter for WSpecialPredef trait
  case class WSpecialPredefAdapter(source: Ref[WSpecialPredef])
      extends Node with WSpecialPredef
      with Def[WSpecialPredef] {
    val resultType: Elem[WSpecialPredef] = element[WSpecialPredef]
    override def transform(t: Transformer) = WSpecialPredefAdapter(t(source))
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefWSpecialPredef(p: Ref[WSpecialPredef]): WSpecialPredef = {
    if (p.node.isInstanceOf[WSpecialPredef]) p.node.asInstanceOf[WSpecialPredef]
    else
      WSpecialPredefAdapter(p)
  }

  // familyElem
  class WSpecialPredefElem[To <: WSpecialPredef]
    extends EntityElem[To] {
  }

  implicit lazy val wSpecialPredefElement: Elem[WSpecialPredef] =
    new WSpecialPredefElem[WSpecialPredef]

  implicit case object WSpecialPredefCompanionElem extends CompanionElem[WSpecialPredefCompanionCtor]

  abstract class WSpecialPredefCompanionCtor extends CompanionDef[WSpecialPredefCompanionCtor] with WSpecialPredefCompanion {
    def resultType = WSpecialPredefCompanionElem
    override def toString = "WSpecialPredef"
  }
  implicit final def unrefWSpecialPredefCompanionCtor(p: Ref[WSpecialPredefCompanionCtor]): WSpecialPredefCompanionCtor =
    p.node.asInstanceOf[WSpecialPredefCompanionCtor]

  lazy val RWSpecialPredef: MutableLazy[WSpecialPredefCompanionCtor] = MutableLazy(new WSpecialPredefCompanionCtor {
    private val thisClass = classOf[WSpecialPredefCompanion]

    def optionGetOrElse[A](opt: Ref[WOption[A]], default: Ref[A]): Ref[A] = {
      implicit val eA = opt.eA
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("optionGetOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](opt, default),
        true, false, element[A]))
    }

    def none[A](implicit emA: Elem[A]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        thisClass.getMethod("none", classOf[Elem[_]]),
        Array[AnyRef](emA),
        true, false, element[WOption[A]]))
    }

    def some[A](x: Ref[A]): Ref[WOption[A]] = {
      implicit val eA = x.elem
      asRep[WOption[A]](mkMethodCall(self,
        thisClass.getMethod("some", classOf[Sym]),
        Array[AnyRef](x),
        true, false, element[WOption[A]]))
    }

    def cast[T](v: Ref[Any])(implicit emT: Elem[T]): Ref[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        thisClass.getMethod("cast", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](v, emT),
        true, false, element[WOption[T]]))
    }

    def loopUntil[A](s1: Ref[A], isMatch: Ref[A => Boolean], step: Ref[A => A]): Ref[A] = {
      implicit val eA = s1.elem
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("loopUntil", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](s1, isMatch, step),
        true, false, element[A]))
    }
  })

  object WSpecialPredefMethods {
  }

  object WSpecialPredefCompanionMethods {
    object optionGetOrElse {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "optionGetOrElse" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[A]) forSome {type A}] = unapply(exp.node)
    }

    object none {
      def unapply(d: Def[_]): Nullable[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "none" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Elem[A] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Elem[A] forSome {type A}] = unapply(exp.node)
    }

    object some {
      def unapply(d: Def[_]): Nullable[Ref[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "some" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Ref[A] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[A] forSome {type A}] = unapply(exp.node)
    }

    object cast {
      def unapply(d: Def[_]): Nullable[(Ref[Any], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "cast" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Any], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Any], Elem[T]) forSome {type T}] = unapply(exp.node)
    }

    object loopUntil {
      def unapply(d: Def[_]): Nullable[(Ref[A], Ref[A => Boolean], Ref[A => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "loopUntil" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = (args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Ref[A], Ref[A => Boolean], Ref[A => A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[A], Ref[A => Boolean], Ref[A => A]) forSome {type A}] = unapply(exp.node)
    }
  }
} // of object WSpecialPredef
  registerEntityObject("WSpecialPredef", WSpecialPredef)

  override def resetContext(): Unit = {
    super.resetContext()
    RWSpecialPredef.reset()
  }

  registerModule(WSpecialPredefsModule)
}

object WSpecialPredefsModule extends scalan.ModuleInfo("wrappers.special", "WSpecialPredefs")
}

trait WSpecialPredefsModule extends wrappers.special.impl.WSpecialPredefsDefs {self: WrappersModule =>}
