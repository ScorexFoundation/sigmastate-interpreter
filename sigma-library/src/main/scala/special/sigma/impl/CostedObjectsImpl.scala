package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait CostedObjectsDefs extends scalan.Scalan with CostedObjects {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AnyValue._
import AvlTree._
import Box._
import Coll._
import Context._
import Costed._
import CostedAvlTree._
import CostedBox._
import CostedBuilder._
import CostedColl._
import CostedOption._
import CostedSigmaObject._
import SigmaDslBuilder._
import CostedContext._

object CostedSigmaObject extends EntityObject("CostedSigmaObject") {
  // entityAdapter for CostedSigmaObject trait
  case class CostedSigmaObjectAdapter[Val](source: Rep[CostedSigmaObject[Val]])
      extends CostedSigmaObject[Val] with Def[CostedSigmaObject[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[CostedSigmaObject[Val]] = element[CostedSigmaObject[Val]]
    override def transform(t: Transformer) = CostedSigmaObjectAdapter[Val](t(source))
    private val thisClass = classOf[CostedSigmaObject[Val]]

    def dsl: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("dsl"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def value: Rep[Val] = {
      asRep[Val](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Val]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedSigmaObject[Val](p: Rep[CostedSigmaObject[Val]]): CostedSigmaObject[Val] = {
    if (p.rhs.isInstanceOf[CostedSigmaObject[Val]@unchecked]) p.rhs.asInstanceOf[CostedSigmaObject[Val]]
    else
      CostedSigmaObjectAdapter(p)
  }

  // familyElem
  class CostedSigmaObjectElem[Val, To <: CostedSigmaObject[Val]](implicit _eVal: Elem[Val])
    extends CostedElem[Val, To] {
    override def eVal = _eVal

    override lazy val parent: Option[Elem[_]] = Some(costedElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CostedSigmaObject[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedSigmaObject[Val]] => convertCostedSigmaObject(x) }
      tryConvert(element[CostedSigmaObject[Val]], this, x, conv)
    }

    def convertCostedSigmaObject(x: Rep[CostedSigmaObject[Val]]): Rep[To] = {
      x.elem match {
        case _: CostedSigmaObjectElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedSigmaObjectElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedSigmaObjectElement[Val](implicit eVal: Elem[Val]): Elem[CostedSigmaObject[Val]] =
    cachedElem[CostedSigmaObjectElem[Val, CostedSigmaObject[Val]]](eVal)

  implicit case object CostedSigmaObjectCompanionElem extends CompanionElem[CostedSigmaObjectCompanionCtor] {
    lazy val tag = weakTypeTag[CostedSigmaObjectCompanionCtor]
    protected def getDefaultRep = RCostedSigmaObject
  }

  abstract class CostedSigmaObjectCompanionCtor extends CompanionDef[CostedSigmaObjectCompanionCtor] with CostedSigmaObjectCompanion {
    def selfType = CostedSigmaObjectCompanionElem
    override def toString = "CostedSigmaObject"
  }
  implicit def proxyCostedSigmaObjectCompanionCtor(p: Rep[CostedSigmaObjectCompanionCtor]): CostedSigmaObjectCompanionCtor =
    proxyOps[CostedSigmaObjectCompanionCtor](p)

  lazy val RCostedSigmaObject: Rep[CostedSigmaObjectCompanionCtor] = new CostedSigmaObjectCompanionCtor {
    private val thisClass = classOf[CostedSigmaObjectCompanion]
  }

  object CostedSigmaObjectMethods {
    object dsl {
      def unapply(d: Def[_]): Nullable[Rep[CostedSigmaObject[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSigmaObjectElem[_, _]] && method.getName == "dsl" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedSigmaObject[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedSigmaObject[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CostedSigmaObject[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSigmaObjectElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedSigmaObject[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedSigmaObject[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedSigmaObjectCompanionMethods {
  }
} // of object CostedSigmaObject
  registerEntityObject("CostedSigmaObject", CostedSigmaObject)

object CostedContext extends EntityObject("CostedContext") {
  // entityAdapter for CostedContext trait
  case class CostedContextAdapter(source: Rep[CostedContext])
      extends CostedContext with Def[CostedContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    val selfType: Elem[CostedContext] = element[CostedContext]
    override def transform(t: Transformer) = CostedContextAdapter(t(source))
    private val thisClass = classOf[CostedContext]

    def OUTPUTS: Rep[CostedColl[Box]] = {
      asRep[CostedColl[Box]](mkMethodCall(source,
        thisClass.getMethod("OUTPUTS"),
        List(),
        true, true, element[CostedColl[Box]]))
    }

    def INPUTS: Rep[CostedColl[Box]] = {
      asRep[CostedColl[Box]](mkMethodCall(source,
        thisClass.getMethod("INPUTS"),
        List(),
        true, true, element[CostedColl[Box]]))
    }

    def HEIGHT: Rep[Costed[Int]] = {
      asRep[Costed[Int]](mkMethodCall(source,
        thisClass.getMethod("HEIGHT"),
        List(),
        true, true, element[Costed[Int]]))
    }

    def SELF: Rep[CostedBox] = {
      asRep[CostedBox](mkMethodCall(source,
        thisClass.getMethod("SELF"),
        List(),
        true, true, element[CostedBox]))
    }

    def LastBlockUtxoRootHash: Rep[CostedAvlTree] = {
      asRep[CostedAvlTree](mkMethodCall(source,
        thisClass.getMethod("LastBlockUtxoRootHash"),
        List(),
        true, true, element[CostedAvlTree]))
    }

    def MinerPubKey: Rep[CostedColl[Byte]] = {
      asRep[CostedColl[Byte]](mkMethodCall(source,
        thisClass.getMethod("MinerPubKey"),
        List(),
        true, true, element[CostedColl[Byte]]))
    }

    def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[CostedOption[T]] = {
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, true, element[CostedOption[T]]))
    }

    def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[Costed[T]] = {
      asRep[Costed[T]](mkMethodCall(source,
        thisClass.getMethod("getConstant", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, true, element[Costed[T]]))
    }

    def dsl: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("dsl"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def value: Rep[Context] = {
      asRep[Context](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Context]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedContext(p: Rep[CostedContext]): CostedContext = {
    if (p.rhs.isInstanceOf[CostedContext@unchecked]) p.rhs.asInstanceOf[CostedContext]
    else
      CostedContextAdapter(p)
  }

  // familyElem
  class CostedContextElem[To <: CostedContext]
    extends CostedSigmaObjectElem[Context, To] {
    override lazy val parent: Option[Elem[_]] = Some(costedSigmaObjectElement(contextElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CostedContext].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedContext] => convertCostedContext(x) }
      tryConvert(element[CostedContext], this, x, conv)
    }

    def convertCostedContext(x: Rep[CostedContext]): Rep[To] = {
      x.elem match {
        case _: CostedContextElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedContextElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val costedContextElement: Elem[CostedContext] =
    new CostedContextElem[CostedContext]

  implicit case object CostedContextCompanionElem extends CompanionElem[CostedContextCompanionCtor] {
    lazy val tag = weakTypeTag[CostedContextCompanionCtor]
    protected def getDefaultRep = RCostedContext
  }

  abstract class CostedContextCompanionCtor extends CompanionDef[CostedContextCompanionCtor] with CostedContextCompanion {
    def selfType = CostedContextCompanionElem
    override def toString = "CostedContext"
  }
  implicit def proxyCostedContextCompanionCtor(p: Rep[CostedContextCompanionCtor]): CostedContextCompanionCtor =
    proxyOps[CostedContextCompanionCtor](p)

  lazy val RCostedContext: Rep[CostedContextCompanionCtor] = new CostedContextCompanionCtor {
    private val thisClass = classOf[CostedContextCompanion]
  }

  object CostedContextMethods {
    object OUTPUTS {
      def unapply(d: Def[_]): Nullable[Rep[CostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "OUTPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Nullable[Rep[CostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "INPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Nullable[Rep[CostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "HEIGHT" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Nullable[Rep[CostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "SELF" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[CostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "LastBlockUtxoRootHash" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object MinerPubKey {
      def unapply(d: Def[_]): Nullable[Rep[CostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "MinerPubKey" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[CostedContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "getVar" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getConstant {
      def unapply(d: Def[_]): Nullable[(Rep[CostedContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedContextElem[_]] && method.getName == "getConstant" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedContextCompanionMethods {
  }
} // of object CostedContext
  registerEntityObject("CostedContext", CostedContext)

object CostedBox extends EntityObject("CostedBox") {
  // entityAdapter for CostedBox trait
  case class CostedBoxAdapter(source: Rep[CostedBox])
      extends CostedBox with Def[CostedBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    val selfType: Elem[CostedBox] = element[CostedBox]
    override def transform(t: Transformer) = CostedBoxAdapter(t(source))
    private val thisClass = classOf[CostedBox]

    def id: Rep[CostedColl[Byte]] = {
      asRep[CostedColl[Byte]](mkMethodCall(source,
        thisClass.getMethod("id"),
        List(),
        true, true, element[CostedColl[Byte]]))
    }

    def valueCosted: Rep[Costed[Long]] = {
      asRep[Costed[Long]](mkMethodCall(source,
        thisClass.getMethod("valueCosted"),
        List(),
        true, true, element[Costed[Long]]))
    }

    def bytes: Rep[CostedColl[Byte]] = {
      asRep[CostedColl[Byte]](mkMethodCall(source,
        thisClass.getMethod("bytes"),
        List(),
        true, true, element[CostedColl[Byte]]))
    }

    def bytesWithoutRef: Rep[CostedColl[Byte]] = {
      asRep[CostedColl[Byte]](mkMethodCall(source,
        thisClass.getMethod("bytesWithoutRef"),
        List(),
        true, true, element[CostedColl[Byte]]))
    }

    def propositionBytes: Rep[CostedColl[Byte]] = {
      asRep[CostedColl[Byte]](mkMethodCall(source,
        thisClass.getMethod("propositionBytes"),
        List(),
        true, true, element[CostedColl[Byte]]))
    }

    def registers: Rep[CostedColl[AnyValue]] = {
      asRep[CostedColl[AnyValue]](mkMethodCall(source,
        thisClass.getMethod("registers"),
        List(),
        true, true, element[CostedColl[AnyValue]]))
    }

    def getReg[T](id: Rep[Int])(implicit cT: Elem[T]): Rep[CostedOption[T]] = {
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, true, element[CostedOption[T]]))
    }

    def creationInfo: Rep[Costed[(Int, Coll[Byte])]] = {
      asRep[Costed[(Int, Coll[Byte])]](mkMethodCall(source,
        thisClass.getMethod("creationInfo"),
        List(),
        true, true, element[Costed[(Int, Coll[Byte])]]))
    }

    def dsl: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("dsl"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def value: Rep[Box] = {
      asRep[Box](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Box]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedBox(p: Rep[CostedBox]): CostedBox = {
    if (p.rhs.isInstanceOf[CostedBox@unchecked]) p.rhs.asInstanceOf[CostedBox]
    else
      CostedBoxAdapter(p)
  }

  // familyElem
  class CostedBoxElem[To <: CostedBox]
    extends CostedSigmaObjectElem[Box, To] {
    override lazy val parent: Option[Elem[_]] = Some(costedSigmaObjectElement(boxElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CostedBox].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedBox] => convertCostedBox(x) }
      tryConvert(element[CostedBox], this, x, conv)
    }

    def convertCostedBox(x: Rep[CostedBox]): Rep[To] = {
      x.elem match {
        case _: CostedBoxElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedBoxElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val costedBoxElement: Elem[CostedBox] =
    new CostedBoxElem[CostedBox]

  implicit case object CostedBoxCompanionElem extends CompanionElem[CostedBoxCompanionCtor] {
    lazy val tag = weakTypeTag[CostedBoxCompanionCtor]
    protected def getDefaultRep = RCostedBox
  }

  abstract class CostedBoxCompanionCtor extends CompanionDef[CostedBoxCompanionCtor] with CostedBoxCompanion {
    def selfType = CostedBoxCompanionElem
    override def toString = "CostedBox"
  }
  implicit def proxyCostedBoxCompanionCtor(p: Rep[CostedBoxCompanionCtor]): CostedBoxCompanionCtor =
    proxyOps[CostedBoxCompanionCtor](p)

  lazy val RCostedBox: Rep[CostedBoxCompanionCtor] = new CostedBoxCompanionCtor {
    private val thisClass = classOf[CostedBoxCompanion]
  }

  object CostedBoxMethods {
    object id {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "id" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueCosted {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "valueCosted" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "bytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "bytesWithoutRef" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "propositionBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "registers" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBox], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "getReg" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBox], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBox], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object creationInfo {
      def unapply(d: Def[_]): Nullable[Rep[CostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBoxElem[_]] && method.getName == "creationInfo" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedBoxCompanionMethods {
  }
} // of object CostedBox
  registerEntityObject("CostedBox", CostedBox)

object CostedAvlTree extends EntityObject("CostedAvlTree") {
  // entityAdapter for CostedAvlTree trait
  case class CostedAvlTreeAdapter(source: Rep[CostedAvlTree])
      extends CostedAvlTree with Def[CostedAvlTree] {
    override lazy val eVal: Elem[AvlTree] = implicitly[Elem[AvlTree]]
    val selfType: Elem[CostedAvlTree] = element[CostedAvlTree]
    override def transform(t: Transformer) = CostedAvlTreeAdapter(t(source))
    private val thisClass = classOf[CostedAvlTree]

    def startingDigest: Rep[CostedColl[Byte]] = {
      asRep[CostedColl[Byte]](mkMethodCall(source,
        thisClass.getMethod("startingDigest"),
        List(),
        true, true, element[CostedColl[Byte]]))
    }

    def keyLength: Rep[Costed[Int]] = {
      asRep[Costed[Int]](mkMethodCall(source,
        thisClass.getMethod("keyLength"),
        List(),
        true, true, element[Costed[Int]]))
    }

    def valueLengthOpt: Rep[CostedOption[Int]] = {
      asRep[CostedOption[Int]](mkMethodCall(source,
        thisClass.getMethod("valueLengthOpt"),
        List(),
        true, true, element[CostedOption[Int]]))
    }

    def maxNumOperations: Rep[CostedOption[Int]] = {
      asRep[CostedOption[Int]](mkMethodCall(source,
        thisClass.getMethod("maxNumOperations"),
        List(),
        true, true, element[CostedOption[Int]]))
    }

    def maxDeletes: Rep[CostedOption[Int]] = {
      asRep[CostedOption[Int]](mkMethodCall(source,
        thisClass.getMethod("maxDeletes"),
        List(),
        true, true, element[CostedOption[Int]]))
    }

    def dsl: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("dsl"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def value: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[AvlTree]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedAvlTree(p: Rep[CostedAvlTree]): CostedAvlTree = {
    if (p.rhs.isInstanceOf[CostedAvlTree@unchecked]) p.rhs.asInstanceOf[CostedAvlTree]
    else
      CostedAvlTreeAdapter(p)
  }

  // familyElem
  class CostedAvlTreeElem[To <: CostedAvlTree]
    extends CostedSigmaObjectElem[AvlTree, To] {
    override lazy val parent: Option[Elem[_]] = Some(costedSigmaObjectElement(avlTreeElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CostedAvlTree].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedAvlTree] => convertCostedAvlTree(x) }
      tryConvert(element[CostedAvlTree], this, x, conv)
    }

    def convertCostedAvlTree(x: Rep[CostedAvlTree]): Rep[To] = {
      x.elem match {
        case _: CostedAvlTreeElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedAvlTreeElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val costedAvlTreeElement: Elem[CostedAvlTree] =
    new CostedAvlTreeElem[CostedAvlTree]

  implicit case object CostedAvlTreeCompanionElem extends CompanionElem[CostedAvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[CostedAvlTreeCompanionCtor]
    protected def getDefaultRep = RCostedAvlTree
  }

  abstract class CostedAvlTreeCompanionCtor extends CompanionDef[CostedAvlTreeCompanionCtor] with CostedAvlTreeCompanion {
    def selfType = CostedAvlTreeCompanionElem
    override def toString = "CostedAvlTree"
  }
  implicit def proxyCostedAvlTreeCompanionCtor(p: Rep[CostedAvlTreeCompanionCtor]): CostedAvlTreeCompanionCtor =
    proxyOps[CostedAvlTreeCompanionCtor](p)

  lazy val RCostedAvlTree: Rep[CostedAvlTreeCompanionCtor] = new CostedAvlTreeCompanionCtor {
    private val thisClass = classOf[CostedAvlTreeCompanion]
  }

  object CostedAvlTreeMethods {
    object startingDigest {
      def unapply(d: Def[_]): Nullable[Rep[CostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedAvlTreeElem[_]] && method.getName == "startingDigest" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object keyLength {
      def unapply(d: Def[_]): Nullable[Rep[CostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedAvlTreeElem[_]] && method.getName == "keyLength" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueLengthOpt {
      def unapply(d: Def[_]): Nullable[Rep[CostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedAvlTreeElem[_]] && method.getName == "valueLengthOpt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object maxNumOperations {
      def unapply(d: Def[_]): Nullable[Rep[CostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedAvlTreeElem[_]] && method.getName == "maxNumOperations" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object maxDeletes {
      def unapply(d: Def[_]): Nullable[Rep[CostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedAvlTreeElem[_]] && method.getName == "maxDeletes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedAvlTreeCompanionMethods {
  }
} // of object CostedAvlTree
  registerEntityObject("CostedAvlTree", CostedAvlTree)

  registerModule(CostedObjectsModule)
}

object CostedObjectsModule extends scalan.ModuleInfo("special.sigma", "CostedObjects")
}

trait CostedObjectsModule extends special.sigma.impl.CostedObjectsDefs {self: SigmaLibrary =>}
