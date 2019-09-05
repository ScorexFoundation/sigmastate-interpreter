package wrappers.scalan

import scalan._
import impl._
import scalan.RType
import special.wrappers.WrappersModule
import special.wrappers.RTypeWrapSpec
import scala.collection.mutable.WrappedArray
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WRTypesDefs extends scalan.Scalan with WRTypes {
  self: WrappersModule =>
import WRType._

object WRType extends EntityObject("WRType") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WRTypeConst[SA, A](
        constValue: RType[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[RType[SA], WRType[A]] with WRType[A]
        with Def[WRType[A]] with WRTypeConstMethods[A] {
    implicit final def eA: Elem[A] = lA.eW

    val liftable: Liftable[RType[SA], WRType[A]] = liftableRType(lA)
    val resultType: Elem[WRType[A]] = liftable.eW
  }

  trait WRTypeConstMethods[A] extends WRType[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val WRTypeClass = classOf[WRType[A]]

    override def name: Ref[String] = {
      asRep[String](mkMethodCall(self,
        WRTypeClass.getMethod("name"),
        WrappedArray.empty,
        true, false, element[String]))
    }
  }

  case class LiftableRType[SA, A](lA: Liftable[SA, A])
    extends Liftable[RType[SA], WRType[A]] {
    lazy val eW: Elem[WRType[A]] = wRTypeElement(lA.eW)
    lazy val sourceType: RType[RType[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[RType[SA]]
    }
    def lift(x: RType[SA]): Ref[WRType[A]] = WRTypeConst(x, lA)
    def unlift(w: Ref[WRType[A]]): RType[SA] = w match {
      case Def(WRTypeConst(x: RType[_], _lA))
            if _lA == lA => x.asInstanceOf[RType[SA]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableRType[SA, A](implicit lA: Liftable[SA,A]): Liftable[RType[SA], WRType[A]] =
    LiftableRType(lA)

  private val _RTypeWrapSpec = new RTypeWrapSpec {}

  private val WRTypeClass = classOf[WRType[_]]

  // entityAdapter for WRType trait
  case class WRTypeAdapter[A](source: Ref[WRType[A]])
      extends Node with WRType[A]
      with Def[WRType[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[WRType[A]] = element[WRType[A]]
    override def transform(t: Transformer) = WRTypeAdapter[A](t(source))

    def name: Ref[String] = {
      asRep[String](mkMethodCall(source,
        WRTypeClass.getMethod("name"),
        WrappedArray.empty,
        true, true, element[String]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefWRType[A](p: Ref[WRType[A]]): WRType[A] = {
    if (p.node.isInstanceOf[WRType[A]@unchecked]) p.node.asInstanceOf[WRType[A]]
    else
      WRTypeAdapter(p)
  }

  // familyElem
  class WRTypeElem[A, To <: WRType[A]](implicit _eA: Elem[A])
    extends EntityElem[To] {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[RType[_], To](liftableRType(_eA.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_RTypeWrapSpec, classOf[WRType[A]], Set(
        "name"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }

  implicit final def wRTypeElement[A](implicit eA: Elem[A]): Elem[WRType[A]] =
    cachedElemByClass(eA)(classOf[WRTypeElem[A, WRType[A]]])

  implicit case object WRTypeCompanionElem extends CompanionElem[WRTypeCompanionCtor]

  abstract class WRTypeCompanionCtor extends CompanionDef[WRTypeCompanionCtor] with WRTypeCompanion {
    def resultType = WRTypeCompanionElem
    override def toString = "WRType"
  }
  implicit final def unrefWRTypeCompanionCtor(p: Ref[WRTypeCompanionCtor]): WRTypeCompanionCtor =
    p.node.asInstanceOf[WRTypeCompanionCtor]

  lazy val RWRType: MutableLazy[WRTypeCompanionCtor] = MutableLazy(new WRTypeCompanionCtor {
    private val thisClass = classOf[WRTypeCompanion]
  })

  object WRTypeMethods {
    object name {
      def unapply(d: Def[_]): Nullable[Ref[WRType[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "name" && receiver.elem.isInstanceOf[WRTypeElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[WRType[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[WRType[A]] forSome {type A}] = unapply(exp.node)
    }
  }

  object WRTypeCompanionMethods {
  }
} // of object WRType
  registerEntityObject("WRType", WRType)

  override def resetContext(): Unit = {
    super.resetContext()
    RWRType.reset()
  }

  registerModule(WRTypesModule)
}

object WRTypesModule extends scalan.ModuleInfo("wrappers.scalan", "WRTypes")
}

trait WRTypesModule extends wrappers.scalan.impl.WRTypesDefs {self: WrappersModule =>}
