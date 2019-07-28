package wrappers.special.sigma

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import special.sigma.wrappers.SigmaPredefWrapSpec
import scala.collection.mutable.WrappedArray
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSigmaPredefsDefs extends scalan.Scalan with WSigmaPredefs {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WSigmaPredef._

object WSigmaPredef extends EntityObject("WSigmaPredef") {
  private val WSigmaPredefClass = classOf[WSigmaPredef]

  // entityAdapter for WSigmaPredef trait
  case class WSigmaPredefAdapter(source: Rep[WSigmaPredef])
      extends WSigmaPredef
      with Def[WSigmaPredef] {
    val selfType: Elem[WSigmaPredef] = element[WSigmaPredef]
    override def transform(t: Transformer) = WSigmaPredefAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWSigmaPredef(p: Rep[WSigmaPredef]): WSigmaPredef = {
    if (p.rhs.isInstanceOf[WSigmaPredef]) p.rhs.asInstanceOf[WSigmaPredef]
    else
      WSigmaPredefAdapter(p)
  }

  // familyElem
  class WSigmaPredefElem[To <: WSigmaPredef]
    extends EntityElem[To] {
  }

  implicit lazy val wSigmaPredefElement: Elem[WSigmaPredef] =
    new WSigmaPredefElem[WSigmaPredef]

  implicit case object WSigmaPredefCompanionElem extends CompanionElem[WSigmaPredefCompanionCtor]

  abstract class WSigmaPredefCompanionCtor extends CompanionDef[WSigmaPredefCompanionCtor] with WSigmaPredefCompanion {
    def selfType = WSigmaPredefCompanionElem
    override def toString = "WSigmaPredef"
  }
  implicit def proxyWSigmaPredefCompanionCtor(p: Rep[WSigmaPredefCompanionCtor]): WSigmaPredefCompanionCtor =
    p.rhs.asInstanceOf[WSigmaPredefCompanionCtor]

  lazy val RWSigmaPredef: Rep[WSigmaPredefCompanionCtor] = new WSigmaPredefCompanionCtor {
    private val thisClass = classOf[WSigmaPredefCompanion]

    def dataSize[T](v: Rep[T]): Rep[Long] = {
      implicit val eT = v.elem
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize", classOf[Sym]),
        Array[AnyRef](v),
        true, false, element[Long]))
    }
  }

  object WSigmaPredefMethods {
  }

  object WSigmaPredefCompanionMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[T] forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "dataSize" && receiver.elem == WSigmaPredefCompanionElem =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Rep[T] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[T] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object WSigmaPredef
  registerEntityObject("WSigmaPredef", WSigmaPredef)

  registerModule(WSigmaPredefsModule)
}

object WSigmaPredefsModule extends scalan.ModuleInfo("wrappers.special.sigma", "WSigmaPredefs")
}

trait WSigmaPredefsModule extends wrappers.special.sigma.impl.WSigmaPredefsDefs {self: WrappersModule =>}
