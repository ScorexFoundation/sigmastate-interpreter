package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait MonoidsDefs extends scalan.Scalan with Monoids {
  self: Library =>
import Monoid._
import MonoidBuilder._

object Monoid extends EntityObject("Monoid") {
  private val MonoidClass = classOf[Monoid[_]]

  // entityAdapter for Monoid trait
  case class MonoidAdapter[T](source: Ref[Monoid[T]])
      extends Node with Monoid[T]
      with Def[Monoid[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asInstanceOf[Elem[T]]

    val resultType: Elem[Monoid[T]] = element[Monoid[T]]
    override def transform(t: Transformer) = MonoidAdapter[T](t(source))

    def zero: Ref[T] = {
      asRep[T](mkMethodCall(source,
        MonoidClass.getMethod("zero"),
        WrappedArray.empty,
        true, true, element[T]))
    }

    def plus(x: Ref[T], y: Ref[T]): Ref[T] = {
      asRep[T](mkMethodCall(source,
        MonoidClass.getMethod("plus", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, y),
        true, true, element[T]))
    }

    def power(x: Ref[T], n: Ref[Int]): Ref[T] = {
      asRep[T](mkMethodCall(source,
        MonoidClass.getMethod("power", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, n),
        true, true, element[T]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefMonoid[T](p: Ref[Monoid[T]]): Monoid[T] = {
    if (p.node.isInstanceOf[Monoid[T]@unchecked]) p.node.asInstanceOf[Monoid[T]]
    else
      MonoidAdapter(p)
  }

  // familyElem
  class MonoidElem[T, To <: Monoid[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }

  implicit final def monoidElement[T](implicit eT: Elem[T]): Elem[Monoid[T]] =
    cachedElemByClass(eT)(classOf[MonoidElem[T, Monoid[T]]])

  implicit case object MonoidCompanionElem extends CompanionElem[MonoidCompanionCtor]

  abstract class MonoidCompanionCtor extends CompanionDef[MonoidCompanionCtor] with MonoidCompanion {
    def resultType = MonoidCompanionElem
    override def toString = "Monoid"
  }
  implicit final def unrefMonoidCompanionCtor(p: Ref[MonoidCompanionCtor]): MonoidCompanionCtor =
    p.node.asInstanceOf[MonoidCompanionCtor]

  lazy val RMonoid: MutableLazy[MonoidCompanionCtor] = MutableLazy(new MonoidCompanionCtor {
    private val thisClass = classOf[MonoidCompanion]
  })
} // of object Monoid
  registerEntityObject("Monoid", Monoid)

object MonoidBuilder extends EntityObject("MonoidBuilder") {
  private val MonoidBuilderClass = classOf[MonoidBuilder]

  // entityAdapter for MonoidBuilder trait
  case class MonoidBuilderAdapter(source: Ref[MonoidBuilder])
      extends Node with MonoidBuilder
      with Def[MonoidBuilder] {
    val resultType: Elem[MonoidBuilder] = element[MonoidBuilder]
    override def transform(t: Transformer) = MonoidBuilderAdapter(t(source))

    def intPlusMonoid: Ref[Monoid[Int]] = {
      asRep[Monoid[Int]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("intPlusMonoid"),
        WrappedArray.empty,
        true, true, element[Monoid[Int]]))
    }

    def intMaxMonoid: Ref[Monoid[Int]] = {
      asRep[Monoid[Int]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("intMaxMonoid"),
        WrappedArray.empty,
        true, true, element[Monoid[Int]]))
    }

    def intMinMonoid: Ref[Monoid[Int]] = {
      asRep[Monoid[Int]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("intMinMonoid"),
        WrappedArray.empty,
        true, true, element[Monoid[Int]]))
    }

    def longPlusMonoid: Ref[Monoid[Long]] = {
      asRep[Monoid[Long]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("longPlusMonoid"),
        WrappedArray.empty,
        true, true, element[Monoid[Long]]))
    }

    def longMaxMonoid: Ref[Monoid[Long]] = {
      asRep[Monoid[Long]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("longMaxMonoid"),
        WrappedArray.empty,
        true, true, element[Monoid[Long]]))
    }

    def longMinMonoid: Ref[Monoid[Long]] = {
      asRep[Monoid[Long]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("longMinMonoid"),
        WrappedArray.empty,
        true, true, element[Monoid[Long]]))
    }

    def pairMonoid[A, B](m1: Ref[Monoid[A]], m2: Ref[Monoid[B]]): Ref[Monoid[(A, B)]] = {
      implicit val eA = m1.eT
implicit val eB = m2.eT
      asRep[Monoid[(A, B)]](mkMethodCall(source,
        MonoidBuilderClass.getMethod("pairMonoid", classOf[Sym], classOf[Sym]),
        Array[AnyRef](m1, m2),
        true, true, element[Monoid[(A, B)]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefMonoidBuilder(p: Ref[MonoidBuilder]): MonoidBuilder = {
    if (p.node.isInstanceOf[MonoidBuilder]) p.node.asInstanceOf[MonoidBuilder]
    else
      MonoidBuilderAdapter(p)
  }

  // familyElem
  class MonoidBuilderElem[To <: MonoidBuilder]
    extends EntityElem[To] {
  }

  implicit lazy val monoidBuilderElement: Elem[MonoidBuilder] =
    new MonoidBuilderElem[MonoidBuilder]

  implicit case object MonoidBuilderCompanionElem extends CompanionElem[MonoidBuilderCompanionCtor]

  abstract class MonoidBuilderCompanionCtor extends CompanionDef[MonoidBuilderCompanionCtor] with MonoidBuilderCompanion {
    def resultType = MonoidBuilderCompanionElem
    override def toString = "MonoidBuilder"
  }
  implicit final def unrefMonoidBuilderCompanionCtor(p: Ref[MonoidBuilderCompanionCtor]): MonoidBuilderCompanionCtor =
    p.node.asInstanceOf[MonoidBuilderCompanionCtor]

  lazy val RMonoidBuilder: MutableLazy[MonoidBuilderCompanionCtor] = MutableLazy(new MonoidBuilderCompanionCtor {
    private val thisClass = classOf[MonoidBuilderCompanion]
  })

  object MonoidBuilderMethods {
    object intPlusMonoid {
      def unapply(d: Def[_]): Nullable[Ref[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "intPlusMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[MonoidBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[MonoidBuilder]] = unapply(exp.node)
    }

    object intMaxMonoid {
      def unapply(d: Def[_]): Nullable[Ref[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "intMaxMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[MonoidBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[MonoidBuilder]] = unapply(exp.node)
    }

    object intMinMonoid {
      def unapply(d: Def[_]): Nullable[Ref[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "intMinMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[MonoidBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[MonoidBuilder]] = unapply(exp.node)
    }

    object longPlusMonoid {
      def unapply(d: Def[_]): Nullable[Ref[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "longPlusMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[MonoidBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[MonoidBuilder]] = unapply(exp.node)
    }

    object longMaxMonoid {
      def unapply(d: Def[_]): Nullable[Ref[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "longMaxMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[MonoidBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[MonoidBuilder]] = unapply(exp.node)
    }

    object longMinMonoid {
      def unapply(d: Def[_]): Nullable[Ref[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "longMinMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[MonoidBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[MonoidBuilder]] = unapply(exp.node)
    }

    object pairMonoid {
      def unapply(d: Def[_]): Nullable[(Ref[MonoidBuilder], Ref[Monoid[A]], Ref[Monoid[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "pairMonoid" && receiver.elem.isInstanceOf[MonoidBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[MonoidBuilder], Ref[Monoid[A]], Ref[Monoid[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[MonoidBuilder], Ref[Monoid[A]], Ref[Monoid[B]]) forSome {type A; type B}] = unapply(exp.node)
    }
  }

  object MonoidBuilderCompanionMethods {
  }
} // of object MonoidBuilder
  registerEntityObject("MonoidBuilder", MonoidBuilder)

  override def resetContext(): Unit = {
    super.resetContext()
    RMonoid.reset()
    RMonoidBuilder.reset()
  }

  registerModule(MonoidsModule)
}

object MonoidsModule extends scalan.ModuleInfo("special.collection", "Monoids")
}

trait MonoidsModule extends special.collection.impl.MonoidsDefs {self: Library =>}
