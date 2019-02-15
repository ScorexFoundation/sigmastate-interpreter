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
import WArray._
import WBigInteger._
import WECPoint._
import WSigmaPredef._
import WrapSpecBase._
import BigIntegerWrapSpec._
import ECPointWrapSpec._
import SigmaPredefWrapSpec._

object ECPointWrapSpec extends EntityObject("ECPointWrapSpec") {
  // entityAdapter for ECPointWrapSpec trait
  case class ECPointWrapSpecAdapter(source: Rep[ECPointWrapSpec])
      extends ECPointWrapSpec with Def[ECPointWrapSpec] {
    val selfType: Elem[ECPointWrapSpec] = element[ECPointWrapSpec]
    override def transform(t: Transformer) = ECPointWrapSpecAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxyECPointWrapSpec(p: Rep[ECPointWrapSpec]): ECPointWrapSpec = {
    if (p.rhs.isInstanceOf[ECPointWrapSpec@unchecked]) p.rhs.asInstanceOf[ECPointWrapSpec]
    else
      ECPointWrapSpecAdapter(p)
  }

  // familyElem
  class ECPointWrapSpecElem[To <: ECPointWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[ECPointWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ECPointWrapSpec] => convertECPointWrapSpec(x) }
      tryConvert(element[ECPointWrapSpec], this, x, conv)
    }

    def convertECPointWrapSpec(x: Rep[ECPointWrapSpec]): Rep[To] = {
      x.elem match {
        case _: ECPointWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have ECPointWrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val eCPointWrapSpecElement: Elem[ECPointWrapSpec] =
    new ECPointWrapSpecElem[ECPointWrapSpec]

  implicit case object ECPointWrapSpecCompanionElem extends CompanionElem[ECPointWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[ECPointWrapSpecCompanionCtor]
    protected def getDefaultRep = RECPointWrapSpec
  }

  abstract class ECPointWrapSpecCompanionCtor extends CompanionDef[ECPointWrapSpecCompanionCtor] with ECPointWrapSpecCompanion {
    def selfType = ECPointWrapSpecCompanionElem
    override def toString = "ECPointWrapSpec"
  }
  implicit def proxyECPointWrapSpecCompanionCtor(p: Rep[ECPointWrapSpecCompanionCtor]): ECPointWrapSpecCompanionCtor =
    proxyOps[ECPointWrapSpecCompanionCtor](p)

  lazy val RECPointWrapSpec: Rep[ECPointWrapSpecCompanionCtor] = new ECPointWrapSpecCompanionCtor {
    private val thisClass = classOf[ECPointWrapSpecCompanion]
  }

  object ECPointWrapSpecMethods {
    object getEncoded {
      def unapply(d: Def[_]): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem[_]] && method.getName == "getEncoded" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[Boolean], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ECPointWrapSpecElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ECPointWrapSpec], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ECPointWrapSpecCompanionMethods {
  }
} // of object ECPointWrapSpec
  registerEntityObject("ECPointWrapSpec", ECPointWrapSpec)

object BigIntegerWrapSpec extends EntityObject("BigIntegerWrapSpec") {
  // entityAdapter for BigIntegerWrapSpec trait
  case class BigIntegerWrapSpecAdapter(source: Rep[BigIntegerWrapSpec])
      extends BigIntegerWrapSpec with Def[BigIntegerWrapSpec] {
    val selfType: Elem[BigIntegerWrapSpec] = element[BigIntegerWrapSpec]
    override def transform(t: Transformer) = BigIntegerWrapSpecAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBigIntegerWrapSpec(p: Rep[BigIntegerWrapSpec]): BigIntegerWrapSpec = {
    if (p.rhs.isInstanceOf[BigIntegerWrapSpec@unchecked]) p.rhs.asInstanceOf[BigIntegerWrapSpec]
    else
      BigIntegerWrapSpecAdapter(p)
  }

  // familyElem
  class BigIntegerWrapSpecElem[To <: BigIntegerWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[BigIntegerWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[BigIntegerWrapSpec] => convertBigIntegerWrapSpec(x) }
      tryConvert(element[BigIntegerWrapSpec], this, x, conv)
    }

    def convertBigIntegerWrapSpec(x: Rep[BigIntegerWrapSpec]): Rep[To] = {
      x.elem match {
        case _: BigIntegerWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have BigIntegerWrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val bigIntegerWrapSpecElement: Elem[BigIntegerWrapSpec] =
    new BigIntegerWrapSpecElem[BigIntegerWrapSpec]

  implicit case object BigIntegerWrapSpecCompanionElem extends CompanionElem[BigIntegerWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[BigIntegerWrapSpecCompanionCtor]
    protected def getDefaultRep = RBigIntegerWrapSpec
  }

  abstract class BigIntegerWrapSpecCompanionCtor extends CompanionDef[BigIntegerWrapSpecCompanionCtor] with BigIntegerWrapSpecCompanion {
    def selfType = BigIntegerWrapSpecCompanionElem
    override def toString = "BigIntegerWrapSpec"
  }
  implicit def proxyBigIntegerWrapSpecCompanionCtor(p: Rep[BigIntegerWrapSpecCompanionCtor]): BigIntegerWrapSpecCompanionCtor =
    proxyOps[BigIntegerWrapSpecCompanionCtor](p)

  lazy val RBigIntegerWrapSpec: Rep[BigIntegerWrapSpecCompanionCtor] = new BigIntegerWrapSpecCompanionCtor {
    private val thisClass = classOf[BigIntegerWrapSpecCompanion]
  }

  object BigIntegerWrapSpecMethods {
    object fromString {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "fromString" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "fromArray" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[Int], Rep[WArray[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueOf {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "valueOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "toByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "subtract" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "mod" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modInverse {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "modInverse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modPow {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "modPow" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "remainder" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "divide" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "compareTo" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "min" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "max" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object gcd {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "gcd" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "and" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "or" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "xor" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object not {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "not" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object andNot {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "andNot" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object pow {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "pow" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object testBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "testBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object setBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "setBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object clearBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "clearBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flipBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "flipBit" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getLowestSetBit {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "getLowestSetBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitCount {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "bitCount" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitLength {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "bitLength" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isProbablePrime {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "isProbablePrime" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftLeft {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "shiftLeft" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftRight {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "shiftRight" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object abs {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "abs" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object negate {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "negate" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object signum {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "signum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "byteValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "shortValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "intValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longValue {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "longValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "byteValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "shortValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "intValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longValueExact {
      def unapply(d: Def[_]): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntegerWrapSpecElem[_]] && method.getName == "longValueExact" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigIntegerWrapSpec], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object BigIntegerWrapSpecCompanionMethods {
  }
} // of object BigIntegerWrapSpec
  registerEntityObject("BigIntegerWrapSpec", BigIntegerWrapSpec)

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
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaPredefWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sigmaPredefWrapSpecElement: Elem[SigmaPredefWrapSpec] =
    new SigmaPredefWrapSpecElem[SigmaPredefWrapSpec]

  implicit case object SigmaPredefWrapSpecCompanionElem extends CompanionElem[SigmaPredefWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaPredefWrapSpecCompanionCtor]
    protected def getDefaultRep = RSigmaPredefWrapSpec
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
