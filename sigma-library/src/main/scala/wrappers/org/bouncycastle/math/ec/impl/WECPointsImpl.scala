package wrappers.org.bouncycastle.math.ec

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import special.sigma.wrappers.ECPointWrapSpec
import java.lang.reflect.Method  // manual fix
import java.math.BigInteger  // manual fix

import org.bouncycastle.math.ec.ECPoint  // manual fix
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WECPointsDefs extends scalan.Scalan with WECPoints {
  self: WrappersModule =>
  import special.sigma._ // manual fix
import IsoUR._
import Converter._
import WArray._
import WBigInteger._
import WECPoint._

object WECPoint extends EntityObject("WECPoint") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WECPointConst(
        constValue: ECPoint
      ) extends WECPoint with LiftedConst[ECPoint, WECPoint]
        with Def[WECPoint] with WECPointConstMethods {
    val liftable: Liftable[ECPoint, WECPoint] = LiftableECPoint
    val selfType: Elem[WECPoint] = liftable.eW
  }

  trait WECPointConstMethods extends WECPoint  { thisConst: Def[_] =>

    private val WECPointClass = classOf[WECPoint]

    override def add(x$1: Rep[WECPoint]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        WECPointClass.getMethod("add", classOf[Sym]),
        List(x$1),
        true, false, element[WECPoint]))
    }

    override def multiply(x$1: Rep[WBigInteger]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        WECPointClass.getMethod("multiply", classOf[Sym]),
        List(x$1),
        true, false, element[WECPoint]))
    }

    override def getEncoded(x$1: Rep[Boolean]): Rep[WArray[Byte]] = {
      asRep[WArray[Byte]](mkMethodCall(self,
        WECPointClass.getMethod("getEncoded", classOf[Sym]),
        List(x$1),
        true, false, element[WArray[Byte]]))
    }
  }

  implicit object LiftableECPoint
    extends Liftable[ECPoint, WECPoint] {
    lazy val eW: Elem[WECPoint] = wECPointElement
    lazy val sourceType: RType[ECPoint] = {
      RType[ECPoint]
    }
    def lift(x: ECPoint): Rep[WECPoint] = WECPointConst(x)
    def unlift(w: Rep[WECPoint]): ECPoint = w match {
      case Def(WECPointConst(x: ECPoint))
            => x.asInstanceOf[ECPoint]
      case _ => unliftError(w)
    }
  }

  private val _ECPointWrapSpec = new ECPointWrapSpec {}
  // entityAdapter for WECPoint trait
  case class WECPointAdapter(source: Rep[WECPoint])
      extends WECPoint with Def[WECPoint] {
    val selfType: Elem[WECPoint] = element[WECPoint]
    override def transform(t: Transformer) = WECPointAdapter(t(source))
    private val thisClass = classOf[WECPoint]

    def add(x$1: Rep[WECPoint]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(source,
        thisClass.getMethod("add", classOf[Sym]),
        List(x$1),
        true, true, element[WECPoint]))
    }

    def multiply(x$1: Rep[WBigInteger]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(source,
        thisClass.getMethod("multiply", classOf[Sym]),
        List(x$1),
        true, true, element[WECPoint]))
    }

    def getEncoded(x$1: Rep[Boolean]): Rep[WArray[Byte]] = {
      asRep[WArray[Byte]](mkMethodCall(source,
        thisClass.getMethod("getEncoded", classOf[Sym]),
        List(x$1),
        true, true, element[WArray[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWECPoint(p: Rep[WECPoint]): WECPoint = {
    if (p.rhs.isInstanceOf[WECPoint@unchecked]) p.rhs.asInstanceOf[WECPoint]
    else
      WECPointAdapter(p)
  }

  // familyElem
  class WECPointElem[To <: WECPoint]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[ECPoint, To](LiftableECPoint)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_ECPointWrapSpec, classOf[WECPoint], Set(
        "add", "multiply", "getEncoded"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WECPoint].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WECPoint] => convertWECPoint(x) }
      tryConvert(element[WECPoint], this, x, conv)
    }

    def convertWECPoint(x: Rep[WECPoint]): Rep[To] = {
      x.elem match {
        case _: WECPointElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have WECPointElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val wECPointElement: Elem[WECPoint] =
    new WECPointElem[WECPoint]

  implicit case object WECPointCompanionElem extends CompanionElem[WECPointCompanionCtor] {
    lazy val tag = weakTypeTag[WECPointCompanionCtor]
    protected def getDefaultRep = RWECPoint
  }

  abstract class WECPointCompanionCtor extends CompanionDef[WECPointCompanionCtor] with WECPointCompanion {
    def selfType = WECPointCompanionElem
    override def toString = "WECPoint"
  }
  implicit def proxyWECPointCompanionCtor(p: Rep[WECPointCompanionCtor]): WECPointCompanionCtor =
    proxyOps[WECPointCompanionCtor](p)

  lazy val RWECPoint: Rep[WECPointCompanionCtor] = new WECPointCompanionCtor {
    private val thisClass = classOf[WECPointCompanion]
  }

  object WECPointMethods {
    object add {
      def unapply(d: Def[_]): Nullable[(Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WECPointElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WECPoint], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WECPointElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WECPoint], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getEncoded {
      def unapply(d: Def[_]): Nullable[(Rep[WECPoint], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WECPointElem[_]] && method.getName == "getEncoded" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WECPoint], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WECPoint], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object WECPointCompanionMethods {
  }
} // of object WECPoint
  registerEntityObject("WECPoint", WECPoint)

  registerModule(WECPointsModule)
}

object WECPointsModule extends scalan.ModuleInfo("wrappers.org.bouncycastle.math.ec", "WECPoints")
}

trait WECPointsModule extends wrappers.org.bouncycastle.math.ec.impl.WECPointsDefs {self: WrappersModule =>}
