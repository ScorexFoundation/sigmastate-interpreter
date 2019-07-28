package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

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
import Header._
import PreHeader._
import SigmaProp._
import Size._
import SizeAnyValue._
import SizeBox._
import SizeBuilder._
import SizeContext._
import WOption._
import WRType._
import SizeSigmaProp._

object SizeAnyValue extends EntityObject("SizeAnyValue") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeAnyValue = special.sigma.SizeAnyValue
  case class SizeAnyValueConst(
        constValue: SSizeAnyValue
      ) extends SizeAnyValue with LiftedConst[SSizeAnyValue, SizeAnyValue]
        with Def[SizeAnyValue] with SizeAnyValueConstMethods {
// manual fix
    def eVal: Elem[AnyValue] = element[AnyValue]

    val liftable: Liftable[SSizeAnyValue, SizeAnyValue] = LiftableSizeAnyValue
    val selfType: Elem[SizeAnyValue] = liftable.eW
  }

  trait SizeAnyValueConstMethods extends SizeAnyValue with SizeConstMethods[AnyValue] { thisConst: Def[_] =>

    private val SizeAnyValueClass = classOf[SizeAnyValue]

    // manual fix
    override def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(self,
        SizeAnyValueClass.getMethod("tVal"),
        WrappedArray.empty,
        true, false, wRTypeElement(AnyElement)))
    }

    // manual fix
    override def valueSize: Rep[Size[Any]] = {
      asRep[Size[Any]](mkMethodCall(self,
        SizeAnyValueClass.getMethod("valueSize"),
        WrappedArray.empty,
        true, false, sizeElement(AnyElement)))
    }
  }

  implicit object LiftableSizeAnyValue
    extends Liftable[SSizeAnyValue, SizeAnyValue] {
    lazy val eW: Elem[SizeAnyValue] = sizeAnyValueElement
    lazy val sourceType: RType[SSizeAnyValue] = {
      RType[SSizeAnyValue]
    }
    def lift(x: SSizeAnyValue): Rep[SizeAnyValue] = SizeAnyValueConst(x)
    def unlift(w: Rep[SizeAnyValue]): SSizeAnyValue = w match {
      case Def(SizeAnyValueConst(x: SSizeAnyValue))
            => x.asInstanceOf[SSizeAnyValue]
      case _ => unliftError(w)
    }
  }

  private val SizeAnyValueClass = classOf[SizeAnyValue]

  // entityAdapter for SizeAnyValue trait
  case class SizeAnyValueAdapter(source: Rep[SizeAnyValue])
      extends SizeAnyValue
      with Def[SizeAnyValue] {
    override lazy val eVal: Elem[AnyValue] = implicitly[Elem[AnyValue]]
    val selfType: Elem[SizeAnyValue] = element[SizeAnyValue]
    override def transform(t: Transformer) = SizeAnyValueAdapter(t(source))

    // manual fix
    def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(source,
        SizeAnyValueClass.getMethod("tVal"),
        WrappedArray.empty,
        true, true, wRTypeElement(AnyElement)))
    }

    // manual fix
    def valueSize: Rep[Size[Any]] = {
      asRep[Size[Any]](mkMethodCall(source,
        SizeAnyValueClass.getMethod("valueSize"),
        WrappedArray.empty,
        true, true, sizeElement(AnyElement)))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeAnyValueClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeAnyValue(p: Rep[SizeAnyValue]): SizeAnyValue = {
    if (p.rhs.isInstanceOf[SizeAnyValue]) p.rhs.asInstanceOf[SizeAnyValue]
    else
      SizeAnyValueAdapter(p)
  }

  // familyElem
  class SizeAnyValueElem[To <: SizeAnyValue]
    extends SizeElem[AnyValue, To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeAnyValue, To](LiftableSizeAnyValue)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeAnyValue], classOf[SSizeAnyValue], Set(
        "tVal", "valueSize"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(anyValueElement))
  }

  implicit lazy val sizeAnyValueElement: Elem[SizeAnyValue] =
    new SizeAnyValueElem[SizeAnyValue]

  implicit case object SizeAnyValueCompanionElem extends CompanionElem[SizeAnyValueCompanionCtor]

  abstract class SizeAnyValueCompanionCtor extends CompanionDef[SizeAnyValueCompanionCtor] with SizeAnyValueCompanion {
    def selfType = SizeAnyValueCompanionElem
    override def toString = "SizeAnyValue"
  }
  implicit def proxySizeAnyValueCompanionCtor(p: Rep[SizeAnyValueCompanionCtor]): SizeAnyValueCompanionCtor =
    p.rhs.asInstanceOf[SizeAnyValueCompanionCtor]

  lazy val RSizeAnyValue: Rep[SizeAnyValueCompanionCtor] = new SizeAnyValueCompanionCtor {
    private val thisClass = classOf[SizeAnyValueCompanion]
  }

  object SizeAnyValueMethods {
    object tVal {
      def unapply(d: Def[_]): Nullable[Rep[SizeAnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tVal" && receiver.elem.isInstanceOf[SizeAnyValueElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeAnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeAnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueSize {
      def unapply(d: Def[_]): Nullable[Rep[SizeAnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "valueSize" && receiver.elem.isInstanceOf[SizeAnyValueElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeAnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeAnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeAnyValueCompanionMethods {
  }
} // of object SizeAnyValue
  registerEntityObject("SizeAnyValue", SizeAnyValue)

object SizeSigmaProp extends EntityObject("SizeSigmaProp") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeSigmaProp = special.sigma.SizeSigmaProp
  case class SizeSigmaPropConst(
        constValue: SSizeSigmaProp
      ) extends SizeSigmaProp with LiftedConst[SSizeSigmaProp, SizeSigmaProp]
        with Def[SizeSigmaProp] with SizeSigmaPropConstMethods {
    // manual fix
    def eVal: Elem[SigmaProp] = element[SigmaProp]

    val liftable: Liftable[SSizeSigmaProp, SizeSigmaProp] = LiftableSizeSigmaProp
    val selfType: Elem[SizeSigmaProp] = liftable.eW
  }

  trait SizeSigmaPropConstMethods extends SizeSigmaProp with SizeConstMethods[SigmaProp] { thisConst: Def[_] =>

    private val SizeSigmaPropClass = classOf[SizeSigmaProp]

    override def propBytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeSigmaPropClass.getMethod("propBytes"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }
  }

  implicit object LiftableSizeSigmaProp
    extends Liftable[SSizeSigmaProp, SizeSigmaProp] {
    lazy val eW: Elem[SizeSigmaProp] = sizeSigmaPropElement
    lazy val sourceType: RType[SSizeSigmaProp] = {
      RType[SSizeSigmaProp]
    }
    def lift(x: SSizeSigmaProp): Rep[SizeSigmaProp] = SizeSigmaPropConst(x)
    def unlift(w: Rep[SizeSigmaProp]): SSizeSigmaProp = w match {
      case Def(SizeSigmaPropConst(x: SSizeSigmaProp))
            => x.asInstanceOf[SSizeSigmaProp]
      case _ => unliftError(w)
    }
  }

  private val SizeSigmaPropClass = classOf[SizeSigmaProp]

  // entityAdapter for SizeSigmaProp trait
  case class SizeSigmaPropAdapter(source: Rep[SizeSigmaProp])
      extends SizeSigmaProp
      with Def[SizeSigmaProp] {
    override lazy val eVal: Elem[SigmaProp] = implicitly[Elem[SigmaProp]]
    val selfType: Elem[SizeSigmaProp] = element[SizeSigmaProp]
    override def transform(t: Transformer) = SizeSigmaPropAdapter(t(source))

    def propBytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeSigmaPropClass.getMethod("propBytes"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeSigmaPropClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeSigmaProp(p: Rep[SizeSigmaProp]): SizeSigmaProp = {
    if (p.rhs.isInstanceOf[SizeSigmaProp]) p.rhs.asInstanceOf[SizeSigmaProp]
    else
      SizeSigmaPropAdapter(p)
  }

  // familyElem
  class SizeSigmaPropElem[To <: SizeSigmaProp]
    extends SizeElem[SigmaProp, To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeSigmaProp, To](LiftableSizeSigmaProp)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeSigmaProp], classOf[SSizeSigmaProp], Set(
        "propBytes"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(sigmaPropElement))
  }

  implicit lazy val sizeSigmaPropElement: Elem[SizeSigmaProp] =
    new SizeSigmaPropElem[SizeSigmaProp]

  implicit case object SizeSigmaPropCompanionElem extends CompanionElem[SizeSigmaPropCompanionCtor]

  abstract class SizeSigmaPropCompanionCtor extends CompanionDef[SizeSigmaPropCompanionCtor] with SizeSigmaPropCompanion {
    def selfType = SizeSigmaPropCompanionElem
    override def toString = "SizeSigmaProp"
  }
  implicit def proxySizeSigmaPropCompanionCtor(p: Rep[SizeSigmaPropCompanionCtor]): SizeSigmaPropCompanionCtor =
    p.rhs.asInstanceOf[SizeSigmaPropCompanionCtor]

  lazy val RSizeSigmaProp: Rep[SizeSigmaPropCompanionCtor] = new SizeSigmaPropCompanionCtor {
    private val thisClass = classOf[SizeSigmaPropCompanion]
  }

  object SizeSigmaPropMethods {
    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[SizeSigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "propBytes" && receiver.elem.isInstanceOf[SizeSigmaPropElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeSigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeSigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeSigmaPropCompanionMethods {
  }
} // of object SizeSigmaProp
  registerEntityObject("SizeSigmaProp", SizeSigmaProp)

object SizeBox extends EntityObject("SizeBox") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeBox = special.sigma.SizeBox
  case class SizeBoxConst(
        constValue: SSizeBox
      ) extends SizeBox with LiftedConst[SSizeBox, SizeBox]
        with Def[SizeBox] with SizeBoxConstMethods {
    // manual fix
    def eVal: Elem[Box] = element[Box]

    val liftable: Liftable[SSizeBox, SizeBox] = LiftableSizeBox
    val selfType: Elem[SizeBox] = liftable.eW
  }

  trait SizeBoxConstMethods extends SizeBox with SizeConstMethods[Box] { thisConst: Def[_] =>

    private val SizeBoxClass = classOf[SizeBox]

    override def propositionBytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("propositionBytes"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }

    override def bytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("bytes"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }

    override def bytesWithoutRef: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("bytesWithoutRef"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }

    override def registers: Rep[Size[Coll[WOption[AnyValue]]]] = {
      asRep[Size[Coll[WOption[AnyValue]]]](mkMethodCall(self,
        SizeBoxClass.getMethod("registers"),
        WrappedArray.empty,
        true, false, element[Size[Coll[WOption[AnyValue]]]]))
    }

    override def getReg[T](id: Rep[Byte])(implicit tT: Elem[T]): Rep[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(self,
        SizeBoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, false, element[Size[WOption[T]]]))
    }

    override def tokens: Rep[Size[Coll[(Coll[Byte], Long)]]] = {
      asRep[Size[Coll[(Coll[Byte], Long)]]](mkMethodCall(self,
        SizeBoxClass.getMethod("tokens"),
        WrappedArray.empty,
        true, false, element[Size[Coll[(Coll[Byte], Long)]]]))
    }
  }

  implicit object LiftableSizeBox
    extends Liftable[SSizeBox, SizeBox] {
    lazy val eW: Elem[SizeBox] = sizeBoxElement
    lazy val sourceType: RType[SSizeBox] = {
      RType[SSizeBox]
    }
    def lift(x: SSizeBox): Rep[SizeBox] = SizeBoxConst(x)
    def unlift(w: Rep[SizeBox]): SSizeBox = w match {
      case Def(SizeBoxConst(x: SSizeBox))
            => x.asInstanceOf[SSizeBox]
      case _ => unliftError(w)
    }
  }

  private val SizeBoxClass = classOf[SizeBox]

  // entityAdapter for SizeBox trait
  case class SizeBoxAdapter(source: Rep[SizeBox])
      extends SizeBox
      with Def[SizeBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    val selfType: Elem[SizeBox] = element[SizeBox]
    override def transform(t: Transformer) = SizeBoxAdapter(t(source))

    def propositionBytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeBoxClass.getMethod("propositionBytes"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def bytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeBoxClass.getMethod("bytes"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def bytesWithoutRef: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeBoxClass.getMethod("bytesWithoutRef"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def registers: Rep[Size[Coll[WOption[AnyValue]]]] = {
      asRep[Size[Coll[WOption[AnyValue]]]](mkMethodCall(source,
        SizeBoxClass.getMethod("registers"),
        WrappedArray.empty,
        true, true, element[Size[Coll[WOption[AnyValue]]]]))
    }

    def getReg[T](id: Rep[Byte])(implicit tT: Elem[T]): Rep[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        SizeBoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, true, element[Size[WOption[T]]]))
    }

    def tokens: Rep[Size[Coll[(Coll[Byte], Long)]]] = {
      asRep[Size[Coll[(Coll[Byte], Long)]]](mkMethodCall(source,
        SizeBoxClass.getMethod("tokens"),
        WrappedArray.empty,
        true, true, element[Size[Coll[(Coll[Byte], Long)]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeBoxClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeBox(p: Rep[SizeBox]): SizeBox = {
    if (p.rhs.isInstanceOf[SizeBox]) p.rhs.asInstanceOf[SizeBox]
    else
      SizeBoxAdapter(p)
  }

  // familyElem
  class SizeBoxElem[To <: SizeBox]
    extends SizeElem[Box, To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeBox, To](LiftableSizeBox)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeBox], classOf[SSizeBox], Set(
        "propositionBytes", "bytes", "bytesWithoutRef", "registers", "getReg", "tokens"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(boxElement))
  }

  implicit lazy val sizeBoxElement: Elem[SizeBox] =
    new SizeBoxElem[SizeBox]

  implicit case object SizeBoxCompanionElem extends CompanionElem[SizeBoxCompanionCtor]

  abstract class SizeBoxCompanionCtor extends CompanionDef[SizeBoxCompanionCtor] with SizeBoxCompanion {
    def selfType = SizeBoxCompanionElem
    override def toString = "SizeBox"
  }
  implicit def proxySizeBoxCompanionCtor(p: Rep[SizeBoxCompanionCtor]): SizeBoxCompanionCtor =
    p.rhs.asInstanceOf[SizeBoxCompanionCtor]

  lazy val RSizeBox: Rep[SizeBoxCompanionCtor] = new SizeBoxCompanionCtor {
    private val thisClass = classOf[SizeBoxCompanion]
  }

  object SizeBoxMethods {
    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "propositionBytes" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Rep[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "bytes" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Rep[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "bytesWithoutRef" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Rep[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "registers" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[SizeBox], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getReg" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeBox], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeBox], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object tokens {
      def unapply(d: Def[_]): Nullable[Rep[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tokens" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeBoxCompanionMethods {
  }
} // of object SizeBox
  registerEntityObject("SizeBox", SizeBox)

object SizeContext extends EntityObject("SizeContext") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeContext = special.sigma.SizeContext
  case class SizeContextConst(
        constValue: SSizeContext
      ) extends SizeContext with LiftedConst[SSizeContext, SizeContext]
        with Def[SizeContext] with SizeContextConstMethods {
    // manual fix
    def eVal: Elem[Context] = element[Context]

    val liftable: Liftable[SSizeContext, SizeContext] = LiftableSizeContext
    val selfType: Elem[SizeContext] = liftable.eW
  }

  trait SizeContextConstMethods extends SizeContext with SizeConstMethods[Context] { thisConst: Def[_] =>

    private val SizeContextClass = classOf[SizeContext]

    override def outputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("outputs"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Box]]]))
    }

    override def inputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("inputs"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Box]]]))
    }

    override def dataInputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("dataInputs"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Box]]]))
    }

    override def selfBox: Rep[Size[Box]] = {
      asRep[Size[Box]](mkMethodCall(self,
        SizeContextClass.getMethod("selfBox"),
        WrappedArray.empty,
        true, false, element[Size[Box]]))
    }

    override def lastBlockUtxoRootHash: Rep[Size[AvlTree]] = {
      asRep[Size[AvlTree]](mkMethodCall(self,
        SizeContextClass.getMethod("lastBlockUtxoRootHash"),
        WrappedArray.empty,
        true, false, element[Size[AvlTree]]))
    }

    override def headers: Rep[Size[Coll[Header]]] = {
      asRep[Size[Coll[Header]]](mkMethodCall(self,
        SizeContextClass.getMethod("headers"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Header]]]))
    }

    override def preHeader: Rep[Size[PreHeader]] = {
      asRep[Size[PreHeader]](mkMethodCall(self,
        SizeContextClass.getMethod("preHeader"),
        WrappedArray.empty,
        true, false, element[Size[PreHeader]]))
    }

    override def getVar[T](id: Rep[Byte])(implicit tT: Elem[T]): Rep[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(self,
        SizeContextClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, false, element[Size[WOption[T]]]))
    }
  }

  implicit object LiftableSizeContext
    extends Liftable[SSizeContext, SizeContext] {
    lazy val eW: Elem[SizeContext] = sizeContextElement
    lazy val sourceType: RType[SSizeContext] = {
      RType[SSizeContext]
    }
    def lift(x: SSizeContext): Rep[SizeContext] = SizeContextConst(x)
    def unlift(w: Rep[SizeContext]): SSizeContext = w match {
      case Def(SizeContextConst(x: SSizeContext))
            => x.asInstanceOf[SSizeContext]
      case _ => unliftError(w)
    }
  }

  private val SizeContextClass = classOf[SizeContext]

  // entityAdapter for SizeContext trait
  case class SizeContextAdapter(source: Rep[SizeContext])
      extends SizeContext
      with Def[SizeContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    val selfType: Elem[SizeContext] = element[SizeContext]
    override def transform(t: Transformer) = SizeContextAdapter(t(source))

    def outputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        SizeContextClass.getMethod("outputs"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Box]]]))
    }

    def inputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        SizeContextClass.getMethod("inputs"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Box]]]))
    }

    def dataInputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        SizeContextClass.getMethod("dataInputs"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Box]]]))
    }

    def selfBox: Rep[Size[Box]] = {
      asRep[Size[Box]](mkMethodCall(source,
        SizeContextClass.getMethod("selfBox"),
        WrappedArray.empty,
        true, true, element[Size[Box]]))
    }

    def lastBlockUtxoRootHash: Rep[Size[AvlTree]] = {
      asRep[Size[AvlTree]](mkMethodCall(source,
        SizeContextClass.getMethod("lastBlockUtxoRootHash"),
        WrappedArray.empty,
        true, true, element[Size[AvlTree]]))
    }

    def headers: Rep[Size[Coll[Header]]] = {
      asRep[Size[Coll[Header]]](mkMethodCall(source,
        SizeContextClass.getMethod("headers"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Header]]]))
    }

    def preHeader: Rep[Size[PreHeader]] = {
      asRep[Size[PreHeader]](mkMethodCall(source,
        SizeContextClass.getMethod("preHeader"),
        WrappedArray.empty,
        true, true, element[Size[PreHeader]]))
    }

    def getVar[T](id: Rep[Byte])(implicit tT: Elem[T]): Rep[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        SizeContextClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, true, element[Size[WOption[T]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeContextClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeContext(p: Rep[SizeContext]): SizeContext = {
    if (p.rhs.isInstanceOf[SizeContext]) p.rhs.asInstanceOf[SizeContext]
    else
      SizeContextAdapter(p)
  }

  // familyElem
  class SizeContextElem[To <: SizeContext]
    extends SizeElem[Context, To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeContext, To](LiftableSizeContext)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeContext], classOf[SSizeContext], Set(
        "outputs", "inputs", "dataInputs", "selfBox", "lastBlockUtxoRootHash", "headers", "preHeader", "getVar"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(contextElement))
  }

  implicit lazy val sizeContextElement: Elem[SizeContext] =
    new SizeContextElem[SizeContext]

  implicit case object SizeContextCompanionElem extends CompanionElem[SizeContextCompanionCtor]

  abstract class SizeContextCompanionCtor extends CompanionDef[SizeContextCompanionCtor] with SizeContextCompanion {
    def selfType = SizeContextCompanionElem
    override def toString = "SizeContext"
  }
  implicit def proxySizeContextCompanionCtor(p: Rep[SizeContextCompanionCtor]): SizeContextCompanionCtor =
    p.rhs.asInstanceOf[SizeContextCompanionCtor]

  lazy val RSizeContext: Rep[SizeContextCompanionCtor] = new SizeContextCompanionCtor {
    private val thisClass = classOf[SizeContextCompanion]
  }

  object SizeContextMethods {
    object outputs {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "outputs" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object inputs {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "inputs" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataInputs {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "dataInputs" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object selfBox {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "selfBox" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "lastBlockUtxoRootHash" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object headers {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "headers" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object preHeader {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "preHeader" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[SizeContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getVar" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeContextCompanionMethods {
  }
} // of object SizeContext
  registerEntityObject("SizeContext", SizeContext)

object SizeBuilder extends EntityObject("SizeBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeBuilder = special.sigma.SizeBuilder
  case class SizeBuilderConst(
        constValue: SSizeBuilder
      ) extends SizeBuilder with LiftedConst[SSizeBuilder, SizeBuilder]
        with Def[SizeBuilder] with SizeBuilderConstMethods {
    val liftable: Liftable[SSizeBuilder, SizeBuilder] = LiftableSizeBuilder
    val selfType: Elem[SizeBuilder] = liftable.eW
  }

  trait SizeBuilderConstMethods extends SizeBuilder  { thisConst: Def[_] =>

    private val SizeBuilderClass = classOf[SizeBuilder]

    override def mkSizeAnyValue(tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[SizeAnyValue] = {
      asRep[SizeAnyValue](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeAnyValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](tVal, valueSize),
        true, false, element[SizeAnyValue]))
    }

    override def mkSizeBox(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]], tokens: Rep[Size[Coll[(Coll[Byte], Long)]]]): Rep[SizeBox] = {
      asRep[SizeBox](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeBox", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](propositionBytes, bytes, bytesWithoutRef, registers, tokens),
        true, false, element[SizeBox]))
    }

    override def mkSizeContext(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]], vars: Rep[Coll[Size[AnyValue]]]): Rep[SizeContext] = {
      asRep[SizeContext](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeContext", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars),
        true, false, element[SizeContext]))
    }
  }

  implicit object LiftableSizeBuilder
    extends Liftable[SSizeBuilder, SizeBuilder] {
    lazy val eW: Elem[SizeBuilder] = sizeBuilderElement
    lazy val sourceType: RType[SSizeBuilder] = {
      RType[SSizeBuilder]
    }
    def lift(x: SSizeBuilder): Rep[SizeBuilder] = SizeBuilderConst(x)
    def unlift(w: Rep[SizeBuilder]): SSizeBuilder = w match {
      case Def(SizeBuilderConst(x: SSizeBuilder))
            => x.asInstanceOf[SSizeBuilder]
      case _ => unliftError(w)
    }
  }

  private val SizeBuilderClass = classOf[SizeBuilder]

  // entityAdapter for SizeBuilder trait
  case class SizeBuilderAdapter(source: Rep[SizeBuilder])
      extends SizeBuilder
      with Def[SizeBuilder] {
    val selfType: Elem[SizeBuilder] = element[SizeBuilder]
    override def transform(t: Transformer) = SizeBuilderAdapter(t(source))

    def mkSizeAnyValue(tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[SizeAnyValue] = {
      asRep[SizeAnyValue](mkMethodCall(source,
        SizeBuilderClass.getMethod("mkSizeAnyValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](tVal, valueSize),
        true, true, element[SizeAnyValue]))
    }

    def mkSizeBox(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]], tokens: Rep[Size[Coll[(Coll[Byte], Long)]]]): Rep[SizeBox] = {
      asRep[SizeBox](mkMethodCall(source,
        SizeBuilderClass.getMethod("mkSizeBox", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](propositionBytes, bytes, bytesWithoutRef, registers, tokens),
        true, true, element[SizeBox]))
    }

    def mkSizeContext(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]], vars: Rep[Coll[Size[AnyValue]]]): Rep[SizeContext] = {
      asRep[SizeContext](mkMethodCall(source,
        SizeBuilderClass.getMethod("mkSizeContext", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars),
        true, true, element[SizeContext]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeBuilder(p: Rep[SizeBuilder]): SizeBuilder = {
    if (p.rhs.isInstanceOf[SizeBuilder]) p.rhs.asInstanceOf[SizeBuilder]
    else
      SizeBuilderAdapter(p)
  }

  // familyElem
  class SizeBuilderElem[To <: SizeBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeBuilder, To](LiftableSizeBuilder)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeBuilder], classOf[SSizeBuilder], Set(
        "mkSizeAnyValue", "mkSizeBox", "mkSizeContext"
        ))
    }
  }

  implicit lazy val sizeBuilderElement: Elem[SizeBuilder] =
    new SizeBuilderElem[SizeBuilder]

  implicit case object SizeBuilderCompanionElem extends CompanionElem[SizeBuilderCompanionCtor]

  abstract class SizeBuilderCompanionCtor extends CompanionDef[SizeBuilderCompanionCtor] with SizeBuilderCompanion {
    def selfType = SizeBuilderCompanionElem
    override def toString = "SizeBuilder"
  }
  implicit def proxySizeBuilderCompanionCtor(p: Rep[SizeBuilderCompanionCtor]): SizeBuilderCompanionCtor =
    p.rhs.asInstanceOf[SizeBuilderCompanionCtor]

  lazy val RSizeBuilder: Rep[SizeBuilderCompanionCtor] = new SizeBuilderCompanionCtor {
    private val thisClass = classOf[SizeBuilderCompanion]
  }

  object SizeBuilderMethods {
    object mkSizeAnyValue {
      def unapply(d: Def[_]): Nullable[(Rep[SizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeAnyValue" && receiver.elem.isInstanceOf[SizeBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeBox {
      def unapply(d: Def[_]): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]], Rep[Size[Coll[(Coll[Byte], Long)]]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeBox" && receiver.elem.isInstanceOf[SizeBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3), args(4))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]], Rep[Size[Coll[(Coll[Byte], Long)]]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]], Rep[Size[Coll[(Coll[Byte], Long)]]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeContext {
      def unapply(d: Def[_]): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]], Rep[Coll[Size[AnyValue]]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeContext" && receiver.elem.isInstanceOf[SizeBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]], Rep[Coll[Size[AnyValue]]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]], Rep[Coll[Size[AnyValue]]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeBuilderCompanionMethods {
  }
} // of object SizeBuilder
  registerEntityObject("SizeBuilder", SizeBuilder)

  registerModule(CostedObjectsModule)
}

object CostedObjectsModule extends scalan.ModuleInfo("special.sigma", "CostedObjects")
}

trait CostedObjectsModule extends special.sigma.impl.CostedObjectsDefs {self: SigmaLibrary =>}
