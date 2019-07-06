package wrappers.java.math

import scalan._
import impl._
import special.sigma.wrappers.WrappersModule
import special.sigma.wrappers.BigIntegerWrapSpec
import scala.reflect.runtime.universe._
import scala.reflect._
import java.lang.reflect.Method  // manual fix
import java.math.BigInteger  // manual fix
import org.bouncycastle.math.ec.ECPoint  // manual fix

package impl {
// Abs -----------------------------------
trait WBigIntegersDefs extends scalan.Scalan with WBigIntegers {
  self: WrappersModule =>
import special.sigma._  // manual fix
import IsoUR._
import Converter._
import WArray._
import WBigInteger._

object WBigInteger extends EntityObject("WBigInteger") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WBigIntegerConst(
        constValue: BigInteger
      ) extends WBigInteger with LiftedConst[BigInteger, WBigInteger]
        with Def[WBigInteger] with WBigIntegerConstMethods {
    val liftable: Liftable[BigInteger, WBigInteger] = LiftableBigInteger
    val selfType: Elem[WBigInteger] = liftable.eW
  }

  trait WBigIntegerConstMethods extends WBigInteger  { thisConst: Def[_] =>

    private val WBigIntegerClass = classOf[WBigInteger]

    override def longValueExact: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        WBigIntegerClass.getMethod("longValueExact"),
        List(),
        true, false, element[Long]))
    }

    override def intValueExact: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("intValueExact"),
        List(),
        true, false, element[Int]))
    }

    override def shortValueExact: Rep[Short] = {
      asRep[Short](mkMethodCall(self,
        WBigIntegerClass.getMethod("shortValueExact"),
        List(),
        true, false, element[Short]))
    }

    override def byteValueExact: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        WBigIntegerClass.getMethod("byteValueExact"),
        List(),
        true, false, element[Byte]))
    }

    override def longValue: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        WBigIntegerClass.getMethod("longValue"),
        List(),
        true, false, element[Long]))
    }

    override def intValue: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("intValue"),
        List(),
        true, false, element[Int]))
    }

    override def shortValue: Rep[Short] = {
      asRep[Short](mkMethodCall(self,
        WBigIntegerClass.getMethod("shortValue"),
        List(),
        true, false, element[Short]))
    }

    override def byteValue: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        WBigIntegerClass.getMethod("byteValue"),
        List(),
        true, false, element[Byte]))
    }

    override def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("signum"),
        List(),
        true, false, element[Int]))
    }

    override def negate: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("negate"),
        List(),
        true, false, element[WBigInteger]))
    }

    override def abs: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("abs"),
        List(),
        true, false, element[WBigInteger]))
    }

    override def shiftRight(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("shiftRight", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def shiftLeft(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("shiftLeft", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def isProbablePrime(x$1: Rep[Int]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WBigIntegerClass.getMethod("isProbablePrime", classOf[Sym]),
        List(x$1),
        true, false, element[Boolean]))
    }

    override def bitLength: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("bitLength"),
        List(),
        true, false, element[Int]))
    }

    override def bitCount: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("bitCount"),
        List(),
        true, false, element[Int]))
    }

    override def getLowestSetBit: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("getLowestSetBit"),
        List(),
        true, false, element[Int]))
    }

    override def flipBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("flipBit", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def clearBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("clearBit", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def setBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("setBit", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def testBit(x$1: Rep[Int]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WBigIntegerClass.getMethod("testBit", classOf[Sym]),
        List(x$1),
        true, false, element[Boolean]))
    }

    override def pow(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("pow", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def andNot(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("andNot", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def not: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("not"),
        List(),
        true, false, element[WBigInteger]))
    }

    override def xor(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("xor", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def or(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("or", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def and(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("and", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def gcd(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("gcd", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def max(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("max", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def min(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("min", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def compareTo(x$1: Rep[WBigInteger]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        WBigIntegerClass.getMethod("compareTo", classOf[Sym]),
        List(x$1),
        true, false, element[Int]))
    }

    override def divide(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("divide", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def remainder(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("remainder", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def modPow(x$1: Rep[WBigInteger], x$2: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("modPow", classOf[Sym], classOf[Sym]),
        List(x$1, x$2),
        true, false, element[WBigInteger]))
    }

    override def modInverse(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("modInverse", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def mod(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("mod", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def multiply(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("multiply", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def subtract(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("subtract", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def add(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        WBigIntegerClass.getMethod("add", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }

    override def toByteArray: Rep[WArray[Byte]] = {
      asRep[WArray[Byte]](mkMethodCall(self,
        WBigIntegerClass.getMethod("toByteArray"),
        List(),
        true, false, element[WArray[Byte]]))
    }
  }

  implicit object LiftableBigInteger
    extends Liftable[BigInteger, WBigInteger] {
    lazy val eW: Elem[WBigInteger] = wBigIntegerElement
    lazy val sourceType: RType[BigInteger] = {
      RType[BigInteger]
    }
    def lift(x: BigInteger): Rep[WBigInteger] = WBigIntegerConst(x)
    def unlift(w: Rep[WBigInteger]): BigInteger = w match {
      case Def(WBigIntegerConst(x: BigInteger))
            => x.asInstanceOf[BigInteger]
      case _ => unliftError(w)
    }
  }

  private val _BigIntegerWrapSpec = new BigIntegerWrapSpec {}
  // entityAdapter for WBigInteger trait
  case class WBigIntegerAdapter(source: Rep[WBigInteger])
      extends WBigInteger with Def[WBigInteger] {
    val selfType: Elem[WBigInteger] = element[WBigInteger]
    override def transform(t: Transformer) = WBigIntegerAdapter(t(source))
    private val thisClass = classOf[WBigInteger]

    def longValueExact: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("longValueExact"),
        List(),
        true, true, element[Long]))
    }

    def intValueExact: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("intValueExact"),
        List(),
        true, true, element[Int]))
    }

    def shortValueExact: Rep[Short] = {
      asRep[Short](mkMethodCall(source,
        thisClass.getMethod("shortValueExact"),
        List(),
        true, true, element[Short]))
    }

    def byteValueExact: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        thisClass.getMethod("byteValueExact"),
        List(),
        true, true, element[Byte]))
    }

    def longValue: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("longValue"),
        List(),
        true, true, element[Long]))
    }

    def intValue: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("intValue"),
        List(),
        true, true, element[Int]))
    }

    def shortValue: Rep[Short] = {
      asRep[Short](mkMethodCall(source,
        thisClass.getMethod("shortValue"),
        List(),
        true, true, element[Short]))
    }

    def byteValue: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        thisClass.getMethod("byteValue"),
        List(),
        true, true, element[Byte]))
    }

    def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("signum"),
        List(),
        true, true, element[Int]))
    }

    def negate: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("negate"),
        List(),
        true, true, element[WBigInteger]))
    }

    def abs: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("abs"),
        List(),
        true, true, element[WBigInteger]))
    }

    def shiftRight(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("shiftRight", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def shiftLeft(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("shiftLeft", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def isProbablePrime(x$1: Rep[Int]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isProbablePrime", classOf[Sym]),
        List(x$1),
        true, true, element[Boolean]))
    }

    def bitLength: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("bitLength"),
        List(),
        true, true, element[Int]))
    }

    def bitCount: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("bitCount"),
        List(),
        true, true, element[Int]))
    }

    def getLowestSetBit: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("getLowestSetBit"),
        List(),
        true, true, element[Int]))
    }

    def flipBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("flipBit", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def clearBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("clearBit", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def setBit(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("setBit", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def testBit(x$1: Rep[Int]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("testBit", classOf[Sym]),
        List(x$1),
        true, true, element[Boolean]))
    }

    def pow(x$1: Rep[Int]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("pow", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def andNot(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("andNot", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def not: Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("not"),
        List(),
        true, true, element[WBigInteger]))
    }

    def xor(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("xor", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def or(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("or", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def and(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("and", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def gcd(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("gcd", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def max(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("max", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def min(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("min", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def compareTo(x$1: Rep[WBigInteger]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("compareTo", classOf[Sym]),
        List(x$1),
        true, true, element[Int]))
    }

    def divide(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("divide", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def remainder(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("remainder", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def modPow(x$1: Rep[WBigInteger], x$2: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("modPow", classOf[Sym], classOf[Sym]),
        List(x$1, x$2),
        true, true, element[WBigInteger]))
    }

    def modInverse(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("modInverse", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def mod(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("mod", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def multiply(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("multiply", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def subtract(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("subtract", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def add(x$1: Rep[WBigInteger]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("add", classOf[Sym]),
        List(x$1),
        true, true, element[WBigInteger]))
    }

    def toByteArray: Rep[WArray[Byte]] = {
      asRep[WArray[Byte]](mkMethodCall(source,
        thisClass.getMethod("toByteArray"),
        List(),
        true, true, element[WArray[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWBigInteger(p: Rep[WBigInteger]): WBigInteger = {
    if (p.rhs.isInstanceOf[WBigInteger@unchecked]) p.rhs.asInstanceOf[WBigInteger]
    else
      WBigIntegerAdapter(p)
  }

  // familyElem
  class WBigIntegerElem[To <: WBigInteger]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[BigInteger, To](LiftableBigInteger)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_BigIntegerWrapSpec, classOf[WBigInteger], Set(
        "longValueExact", "intValueExact", "shortValueExact", "byteValueExact", "longValue", "intValue", "shortValue", "byteValue", "signum", "negate", "abs", "shiftRight", "shiftLeft", "isProbablePrime", "bitLength", "bitCount", "getLowestSetBit", "flipBit", "clearBit", "setBit", "testBit", "pow", "andNot", "not", "xor", "or", "and", "gcd", "max", "min", "compareTo", "divide", "remainder", "modPow", "modInverse", "mod", "multiply", "subtract", "add", "toByteArray"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WBigInteger].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WBigInteger] => convertWBigInteger(x) }
      tryConvert(element[WBigInteger], this, x, conv)
    }

    def convertWBigInteger(x: Rep[WBigInteger]): Rep[To] = {
      x.elem match {
        case _: WBigIntegerElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have WBigIntegerElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val wBigIntegerElement: Elem[WBigInteger] =
    new WBigIntegerElem[WBigInteger]

  implicit case object WBigIntegerCompanionElem extends CompanionElem[WBigIntegerCompanionCtor] {
    lazy val tag = weakTypeTag[WBigIntegerCompanionCtor]
    protected def getDefaultRep = RWBigInteger
  }

  abstract class WBigIntegerCompanionCtor extends CompanionDef[WBigIntegerCompanionCtor] with WBigIntegerCompanion {
    def selfType = WBigIntegerCompanionElem
    override def toString = "WBigInteger"
  }
  implicit def proxyWBigIntegerCompanionCtor(p: Rep[WBigIntegerCompanionCtor]): WBigIntegerCompanionCtor =
    proxyOps[WBigIntegerCompanionCtor](p)

  lazy val RWBigInteger: Rep[WBigIntegerCompanionCtor] = new WBigIntegerCompanionCtor {
    private val thisClass = classOf[WBigIntegerCompanion]

    def apply(x$1: Rep[Int], x$2: Rep[WArray[Byte]]): Rep[WBigInteger] =
      newObjEx[WBigInteger](x$1, x$2)

    def apply(x$1: Rep[String]): Rep[WBigInteger] =
      newObjEx[WBigInteger](x$1)

    def valueOf(x$1: Rep[Long]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("valueOf", classOf[Sym]),
        List(x$1),
        true, false, element[WBigInteger]))
    }
  }

  object WBigIntegerMethods {
    object longValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "longValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "intValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shortValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValueExact {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "byteValueExact" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "longValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "intValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shortValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shortValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteValue {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "byteValue" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object signum {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "signum" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object negate {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "negate" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object abs {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "abs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftRight {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shiftRight" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object shiftLeft {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "shiftLeft" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isProbablePrime {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "isProbablePrime" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitLength {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "bitLength" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bitCount {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "bitCount" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getLowestSetBit {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "getLowestSetBit" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flipBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "flipBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object clearBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "clearBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object setBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "setBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object testBit {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "testBit" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object pow {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "pow" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object andNot {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "andNot" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object not {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "not" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "xor" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "or" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "and" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object gcd {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "gcd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "max" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "min" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "compareTo" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "divide" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "remainder" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modPow {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "modPow" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modInverse {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "modInverse" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "mod" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "subtract" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WBigInteger], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WBigInteger], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toByteArray {
      def unapply(d: Def[_]): Nullable[Rep[WBigInteger]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WBigIntegerElem[_]] && method.getName == "toByteArray" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WBigInteger]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WBigInteger]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object WBigIntegerCompanionMethods {
    object apply_constructor_1 {
      def unapply(d: Def[_]): Nullable[(Rep[Int], Rep[WArray[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "constructor_1" } =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Int], Rep[WArray[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Int], Rep[WArray[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply_constructor_2 {
      def unapply(d: Def[_]): Nullable[Rep[String]] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "constructor_2" } =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Rep[String]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[String]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueOf {
      def unapply(d: Def[_]): Nullable[Rep[Long]] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WBigIntegerCompanionElem && method.getName == "valueOf" =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Rep[Long]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Long]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object WBigInteger
  registerEntityObject("WBigInteger", WBigInteger)

  registerModule(WBigIntegersModule)
}

object WBigIntegersModule extends scalan.ModuleInfo("wrappers.java.math", "WBigIntegers")
}

trait WBigIntegersModule extends wrappers.java.math.impl.WBigIntegersDefs {self: WrappersModule =>}
