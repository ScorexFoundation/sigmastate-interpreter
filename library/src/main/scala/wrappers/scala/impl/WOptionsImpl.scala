package wrappers.scala

import scalan._
import impl._
import special.wrappers.WrappersModule
import special.wrappers.OptionWrapSpec
import scala.collection.mutable.WrappedArray
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WOptionsDefs extends scalan.Scalan with WOptions {
  self: WrappersModule =>
import WOption._

object WOption extends EntityObject("WOption") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WOptionConst[SA, A](
        constValue: Option[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[Option[SA], WOption[A]] with WOption[A]
        with Def[WOption[A]] with WOptionConstMethods[A] {
    implicit final def eA: Elem[A] = lA.eW

    val liftable: Liftable[Option[SA], WOption[A]] = liftableOption(lA)
    val resultType: Elem[WOption[A]] = liftable.eW
  }

  trait WOptionConstMethods[A] extends WOption[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val WOptionClass = classOf[WOption[A]]

    override def fold[B](ifEmpty: Ref[Thunk[B]], f: Ref[A => B]): Ref[B] = {
      implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(self,
        WOptionClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        Array[AnyRef](ifEmpty, f),
        true, false, element[B]))
    }

    override def isEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WOptionClass.getMethod("isEmpty"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def isDefined: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WOptionClass.getMethod("isDefined"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def filter(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        WOptionClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[WOption[A]]))
    }

    override def flatMap[B](f: Ref[A => WOption[B]]): Ref[WOption[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[WOption[B]](mkMethodCall(self,
        WOptionClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[WOption[B]]))
    }

    override def map[B](f: Ref[A => B]): Ref[WOption[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WOption[B]](mkMethodCall(self,
        WOptionClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[WOption[B]]))
    }

    override def getOrElse[B](default: Ref[Thunk[B]]): Ref[B] = {
      implicit val eB = default.elem.eItem
      asRep[B](mkMethodCall(self,
        WOptionClass.getMethod("getOrElse", classOf[Sym]),
        Array[AnyRef](default),
        true, false, element[B]))
    }

    override def get: Ref[A] = {
      asRep[A](mkMethodCall(self,
        WOptionClass.getMethod("get"),
        WrappedArray.empty,
        true, false, element[A]))
    }
  }

  case class LiftableOption[SA, A](lA: Liftable[SA, A])
    extends Liftable[Option[SA], WOption[A]] {
    lazy val eW: Elem[WOption[A]] = wOptionElement(lA.eW)
    lazy val sourceType: RType[Option[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[Option[SA]]
    }
    def lift(x: Option[SA]): Ref[WOption[A]] = WOptionConst(x, lA)
    def unlift(w: Ref[WOption[A]]): Option[SA] = w match {
      case Def(WOptionConst(x: Option[_], _lA))
            if _lA == lA => x.asInstanceOf[Option[SA]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableOption[SA, A](implicit lA: Liftable[SA,A]): Liftable[Option[SA], WOption[A]] =
    LiftableOption(lA)

  private val _OptionWrapSpec = new OptionWrapSpec {}

  private val WOptionClass = classOf[WOption[_]]

  // entityAdapter for WOption trait
  case class WOptionAdapter[A](source: Ref[WOption[A]])
      extends Node with WOption[A]
      with Def[WOption[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[WOption[A]] = element[WOption[A]]
    override def transform(t: Transformer) = WOptionAdapter[A](t(source))

    def fold[B](ifEmpty: Ref[Thunk[B]], f: Ref[A => B]): Ref[B] = {
      implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(source,
        WOptionClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        Array[AnyRef](ifEmpty, f),
        true, true, element[B]))
    }

    def isEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        WOptionClass.getMethod("isEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def isDefined: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        WOptionClass.getMethod("isDefined"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def filter(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        WOptionClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[WOption[A]]))
    }

    def flatMap[B](f: Ref[A => WOption[B]]): Ref[WOption[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[WOption[B]](mkMethodCall(source,
        WOptionClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[WOption[B]]))
    }

    def map[B](f: Ref[A => B]): Ref[WOption[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WOption[B]](mkMethodCall(source,
        WOptionClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[WOption[B]]))
    }

    def getOrElse[B](default: Ref[Thunk[B]]): Ref[B] = {
      implicit val eB = default.elem.eItem
      asRep[B](mkMethodCall(source,
        WOptionClass.getMethod("getOrElse", classOf[Sym]),
        Array[AnyRef](default),
        true, true, element[B]))
    }

    def get: Ref[A] = {
      asRep[A](mkMethodCall(source,
        WOptionClass.getMethod("get"),
        WrappedArray.empty,
        true, true, element[A]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefWOption[A](p: Ref[WOption[A]]): WOption[A] = {
    if (p.node.isInstanceOf[WOption[A]@unchecked]) p.node.asInstanceOf[WOption[A]]
    else
      WOptionAdapter(p)
  }

  implicit final def castWOptionElement[A](elem: Elem[WOption[A]]): WOptionElem[A, WOption[A]] =
    elem.asInstanceOf[WOptionElem[A, WOption[A]]]

  implicit lazy val containerWOption: Functor[WOption] = new Functor[WOption] {
    def lift[A](implicit evA: Elem[A]) = element[WOption[A]]
    def unlift[A](implicit eFT: Elem[WOption[A]]) =
      castWOptionElement(eFT).eA
    def unapply[T](e: Elem[_]) = e match {
      case e: WOptionElem[_,_] => Some(asElem[WOption[T]](e))
      case _ => None
    }
    def map[A,B](xs: Ref[WOption[A]])(f: Ref[A] => Ref[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  // manual fix: WOptionIso, wOptionIso

  // familyElem
  class WOptionElem[A, To <: WOption[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, WOption](_eA, container[WOption]) {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[Option[_], To](liftableOption(_eA.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_OptionWrapSpec, classOf[WOption[A]], Set(
        "fold", "isEmpty", "isDefined", "filter", "flatMap", "map", "getOrElse", "get"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }

  implicit final def wOptionElement[A](implicit eA: Elem[A]): Elem[WOption[A]] =
    cachedElemByClass(eA)(classOf[WOptionElem[A, WOption[A]]])

  implicit case object WOptionCompanionElem extends CompanionElem[WOptionCompanionCtor]

  abstract class WOptionCompanionCtor extends CompanionDef[WOptionCompanionCtor] with WOptionCompanion {
    def resultType = WOptionCompanionElem
    override def toString = "WOption"
  }
  implicit final def unrefWOptionCompanionCtor(p: Ref[WOptionCompanionCtor]): WOptionCompanionCtor =
    p.node.asInstanceOf[WOptionCompanionCtor]

  lazy val RWOption: MutableLazy[WOptionCompanionCtor] = MutableLazy(new WOptionCompanionCtor {
    private val thisClass = classOf[WOptionCompanion]
  })

  // manual fix: ViewWOption

  object WOptionMethods {
    object fold {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[Thunk[B]], Ref[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "fold" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[Thunk[B]], Ref[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[Thunk[B]], Ref[A => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object isEmpty {
      def unapply(d: Def[_]): Nullable[Ref[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "isEmpty" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[WOption[A]] forSome {type A}] = unapply(exp.node)
    }

    object isDefined {
      def unapply(d: Def[_]): Nullable[Ref[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "isDefined" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[WOption[A]] forSome {type A}] = unapply(exp.node)
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "filter" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[A => WOption[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "flatMap" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[A => WOption[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[A => WOption[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "map" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[A => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Ref[WOption[A]], Ref[Thunk[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getOrElse" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[WOption[A]], Ref[Thunk[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[WOption[A]], Ref[Thunk[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object get {
      def unapply(d: Def[_]): Nullable[Ref[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "get" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[WOption[A]] forSome {type A}] = unapply(exp.node)
    }
  }

  object WOptionCompanionMethods {
  }
} // of object WOption
  registerEntityObject("WOption", WOption)

  // manual fix: UserTypeWOption removed
  // manual fix: unapplyViews removed
  // manual fix: RepWOption removed
  // manual fix: rewriteDef removed

  registerModule(WOptionsModule)
}

object WOptionsModule extends scalan.ModuleInfo("wrappers.scala", "WOptions")
}

trait WOptionsModule extends wrappers.scala.impl.WOptionsDefs {self: WrappersModule =>}
