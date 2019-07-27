package special.sigma.wrappers

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WrappersSpecDefs extends scalan.Scalan with WrappersSpec {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import WSigmaPredef._
import WrapSpecBase._
import SigmaPredefWrapSpec._

object SigmaPredefWrapSpec extends EntityObject("SigmaPredefWrapSpec") {
  // entityAdapter for SigmaPredefWrapSpec trait
  case class SigmaPredefWrapSpecAdapter(source: Rep[SigmaPredefWrapSpec])
      extends SigmaPredefWrapSpec with Def[SigmaPredefWrapSpec] {
    val selfType: Elem[SigmaPredefWrapSpec] = element[SigmaPredefWrapSpec]
    override def transform(t: Transformer) = SigmaPredefWrapSpecAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaPredefWrapSpec(p: Rep[SigmaPredefWrapSpec]): SigmaPredefWrapSpec = {
    if (p.rhs.isInstanceOf[SigmaPredefWrapSpec@unchecked]) p.rhs.asInstanceOf[SigmaPredefWrapSpec]
    else
      SigmaPredefWrapSpecAdapter(p)
  }

  // familyElem
  class SigmaPredefWrapSpecElem[To <: SigmaPredefWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaPredefWrapSpec] => convertSigmaPredefWrapSpec(x) }
      tryConvert(element[SigmaPredefWrapSpec], this, x, conv)
    }

    def convertSigmaPredefWrapSpec(x: Rep[SigmaPredefWrapSpec]): Rep[To] = {
      x.elem match {
        case _: SigmaPredefWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaPredefWrapSpecElem[_], but got $e", x)
      }
    }
  }

  implicit lazy val sigmaPredefWrapSpecElement: Elem[SigmaPredefWrapSpec] =
    new SigmaPredefWrapSpecElem[SigmaPredefWrapSpec]

  implicit case object SigmaPredefWrapSpecCompanionElem extends CompanionElem[SigmaPredefWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaPredefWrapSpecCompanionCtor]
  }

  abstract class SigmaPredefWrapSpecCompanionCtor extends CompanionDef[SigmaPredefWrapSpecCompanionCtor] with SigmaPredefWrapSpecCompanion {
    def selfType = SigmaPredefWrapSpecCompanionElem
    override def toString = "SigmaPredefWrapSpec"
  }
  implicit def proxySigmaPredefWrapSpecCompanionCtor(p: Rep[SigmaPredefWrapSpecCompanionCtor]): SigmaPredefWrapSpecCompanionCtor =
    proxyOps[SigmaPredefWrapSpecCompanionCtor](p)

  lazy val RSigmaPredefWrapSpec: Rep[SigmaPredefWrapSpecCompanionCtor] = new SigmaPredefWrapSpecCompanionCtor {
    private val thisClass = classOf[SigmaPredefWrapSpecCompanion]
  }

  object SigmaPredefWrapSpecMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaPredefWrapSpec], Rep[Any])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPredefWrapSpecElem[_]] && method.getName == "dataSize" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaPredefWrapSpec], Rep[Any])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaPredefWrapSpec], Rep[Any])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaPredefWrapSpecCompanionMethods {
  }
} // of object SigmaPredefWrapSpec
  registerEntityObject("SigmaPredefWrapSpec", SigmaPredefWrapSpec)

  registerModule(WrappersSpecModule)
}

object WrappersSpecModule extends scalan.ModuleInfo("special.sigma.wrappers", "WrappersSpec")
}

trait WrappersSpecModule extends special.sigma.wrappers.impl.WrappersSpecDefs {self: SigmaLibrary =>}
