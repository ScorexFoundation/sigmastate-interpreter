package special.sigma

import scala.language.{existentials,implicitConversions}
import scalan._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait CostedObjectsDefs extends scalan.Scalan with CostedObjects {
  self: SigmaLibrary =>
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
      ) extends LiftedConst[SSizeAnyValue, SizeAnyValue] with SizeAnyValue
        with Def[SizeAnyValue] with SizeAnyValueConstMethods {
    // manual fix
    final def eVal: Elem[AnyValue] = element[AnyValue]

    val liftable: Liftable[SSizeAnyValue, SizeAnyValue] = LiftableSizeAnyValue
    val resultType: Elem[SizeAnyValue] = liftable.eW
  }

  trait SizeAnyValueConstMethods extends SizeAnyValue with SizeConstMethods[AnyValue] { thisConst: Def[_] =>

    private val SizeAnyValueClass = classOf[SizeAnyValue]

    // manual fix
    override def tVal: Ref[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(self,
        SizeAnyValueClass.getMethod("tVal"),
        WrappedArray.empty,
        true, false, wRTypeAnyElement))
    }

    // manual fix
    override def valueSize: Ref[Size[Any]] = {
      asRep[Size[Any]](mkMethodCall(self,
        SizeAnyValueClass.getMethod("valueSize"),
        WrappedArray.empty,
        true, false, sizeAnyElement))
    }
  }

  implicit object LiftableSizeAnyValue
    extends Liftable[SSizeAnyValue, SizeAnyValue] {
    lazy val eW: Elem[SizeAnyValue] = sizeAnyValueElement
    lazy val sourceType: RType[SSizeAnyValue] = {
      RType[SSizeAnyValue]
    }
    def lift(x: SSizeAnyValue): Ref[SizeAnyValue] = SizeAnyValueConst(x)
    def unlift(w: Ref[SizeAnyValue]): SSizeAnyValue = w match {
      case Def(SizeAnyValueConst(x: SSizeAnyValue))
            => x.asInstanceOf[SSizeAnyValue]
      case _ => unliftError(w)
    }
  }

  private val SizeAnyValueClass = classOf[SizeAnyValue]

  // entityAdapter for SizeAnyValue trait
  case class SizeAnyValueAdapter(source: Ref[SizeAnyValue])
      extends Node with SizeAnyValue
      with Def[SizeAnyValue] {
    override lazy val eVal: Elem[AnyValue] = implicitly[Elem[AnyValue]]
    val resultType: Elem[SizeAnyValue] = element[SizeAnyValue]
    override def transform(t: Transformer) = SizeAnyValueAdapter(t(source))

    // manual fix
    def tVal: Ref[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(source,
        SizeAnyValueClass.getMethod("tVal"),
        WrappedArray.empty,
        true, true, wRTypeAnyElement))
    }

    // manual fix
    def valueSize: Ref[Size[Any]] = {
      asRep[Size[Any]](mkMethodCall(source,
        SizeAnyValueClass.getMethod("valueSize"),
        WrappedArray.empty,
        true, true, sizeAnyElement))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeAnyValueClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeAnyValue(p: Ref[SizeAnyValue]): SizeAnyValue = {
    if (p.node.isInstanceOf[SizeAnyValue]) p.node.asInstanceOf[SizeAnyValue]
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
    def resultType = SizeAnyValueCompanionElem
    override def toString = "SizeAnyValue"
  }
  implicit final def unrefSizeAnyValueCompanionCtor(p: Ref[SizeAnyValueCompanionCtor]): SizeAnyValueCompanionCtor =
    p.node.asInstanceOf[SizeAnyValueCompanionCtor]

  lazy val RSizeAnyValue: MutableLazy[SizeAnyValueCompanionCtor] = MutableLazy(new SizeAnyValueCompanionCtor {
    private val thisClass = classOf[SizeAnyValueCompanion]
  })

  object SizeAnyValueMethods {
    object tVal {
      def unapply(d: Def[_]): Nullable[Ref[SizeAnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tVal" && receiver.elem.isInstanceOf[SizeAnyValueElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeAnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeAnyValue]] = unapply(exp.node)
    }

    object valueSize {
      def unapply(d: Def[_]): Nullable[Ref[SizeAnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "valueSize" && receiver.elem.isInstanceOf[SizeAnyValueElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeAnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeAnyValue]] = unapply(exp.node)
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
      ) extends LiftedConst[SSizeSigmaProp, SizeSigmaProp] with SizeSigmaProp
        with Def[SizeSigmaProp] with SizeSigmaPropConstMethods {
    // manual fix
    final def eVal: Elem[SigmaProp] = element[SigmaProp]

    val liftable: Liftable[SSizeSigmaProp, SizeSigmaProp] = LiftableSizeSigmaProp
    val resultType: Elem[SizeSigmaProp] = liftable.eW
  }

  trait SizeSigmaPropConstMethods extends SizeSigmaProp with SizeConstMethods[SigmaProp] { thisConst: Def[_] =>

    private val SizeSigmaPropClass = classOf[SizeSigmaProp]

    override def propBytes: Ref[Size[Coll[Byte]]] = {
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
    def lift(x: SSizeSigmaProp): Ref[SizeSigmaProp] = SizeSigmaPropConst(x)
    def unlift(w: Ref[SizeSigmaProp]): SSizeSigmaProp = w match {
      case Def(SizeSigmaPropConst(x: SSizeSigmaProp))
            => x.asInstanceOf[SSizeSigmaProp]
      case _ => unliftError(w)
    }
  }

  private val SizeSigmaPropClass = classOf[SizeSigmaProp]

  // entityAdapter for SizeSigmaProp trait
  case class SizeSigmaPropAdapter(source: Ref[SizeSigmaProp])
      extends Node with SizeSigmaProp
      with Def[SizeSigmaProp] {
    override lazy val eVal: Elem[SigmaProp] = implicitly[Elem[SigmaProp]]
    val resultType: Elem[SizeSigmaProp] = element[SizeSigmaProp]
    override def transform(t: Transformer) = SizeSigmaPropAdapter(t(source))

    def propBytes: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeSigmaPropClass.getMethod("propBytes"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeSigmaPropClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeSigmaProp(p: Ref[SizeSigmaProp]): SizeSigmaProp = {
    if (p.node.isInstanceOf[SizeSigmaProp]) p.node.asInstanceOf[SizeSigmaProp]
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
    def resultType = SizeSigmaPropCompanionElem
    override def toString = "SizeSigmaProp"
  }
  implicit final def unrefSizeSigmaPropCompanionCtor(p: Ref[SizeSigmaPropCompanionCtor]): SizeSigmaPropCompanionCtor =
    p.node.asInstanceOf[SizeSigmaPropCompanionCtor]

  lazy val RSizeSigmaProp: MutableLazy[SizeSigmaPropCompanionCtor] = MutableLazy(new SizeSigmaPropCompanionCtor {
    private val thisClass = classOf[SizeSigmaPropCompanion]
  })

  object SizeSigmaPropMethods {
    object propBytes {
      def unapply(d: Def[_]): Nullable[Ref[SizeSigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "propBytes" && receiver.elem.isInstanceOf[SizeSigmaPropElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeSigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeSigmaProp]] = unapply(exp.node)
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
      ) extends LiftedConst[SSizeBox, SizeBox] with SizeBox
        with Def[SizeBox] with SizeBoxConstMethods {
    // manual fix
    final def eVal: Elem[Box] = element[Box]

    val liftable: Liftable[SSizeBox, SizeBox] = LiftableSizeBox
    val resultType: Elem[SizeBox] = liftable.eW
  }

  trait SizeBoxConstMethods extends SizeBox with SizeConstMethods[Box] { thisConst: Def[_] =>

    private val SizeBoxClass = classOf[SizeBox]

    override def propositionBytes: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("propositionBytes"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }

    override def bytes: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("bytes"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }

    override def bytesWithoutRef: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("bytesWithoutRef"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Byte]]]))
    }

    override def registers: Ref[Size[Coll[WOption[AnyValue]]]] = {
      asRep[Size[Coll[WOption[AnyValue]]]](mkMethodCall(self,
        SizeBoxClass.getMethod("registers"),
        WrappedArray.empty,
        true, false, element[Size[Coll[WOption[AnyValue]]]]))
    }

    override def getReg[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(self,
        SizeBoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, false, element[Size[WOption[T]]]))
    }

    override def tokens: Ref[Size[Coll[(Coll[Byte], Long)]]] = {
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
    def lift(x: SSizeBox): Ref[SizeBox] = SizeBoxConst(x)
    def unlift(w: Ref[SizeBox]): SSizeBox = w match {
      case Def(SizeBoxConst(x: SSizeBox))
            => x.asInstanceOf[SSizeBox]
      case _ => unliftError(w)
    }
  }

  private val SizeBoxClass = classOf[SizeBox]

  // entityAdapter for SizeBox trait
  case class SizeBoxAdapter(source: Ref[SizeBox])
      extends Node with SizeBox
      with Def[SizeBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    val resultType: Elem[SizeBox] = element[SizeBox]
    override def transform(t: Transformer) = SizeBoxAdapter(t(source))

    def propositionBytes: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeBoxClass.getMethod("propositionBytes"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def bytes: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeBoxClass.getMethod("bytes"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def bytesWithoutRef: Ref[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        SizeBoxClass.getMethod("bytesWithoutRef"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Byte]]]))
    }

    def registers: Ref[Size[Coll[WOption[AnyValue]]]] = {
      asRep[Size[Coll[WOption[AnyValue]]]](mkMethodCall(source,
        SizeBoxClass.getMethod("registers"),
        WrappedArray.empty,
        true, true, element[Size[Coll[WOption[AnyValue]]]]))
    }

    def getReg[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        SizeBoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, true, element[Size[WOption[T]]]))
    }

    def tokens: Ref[Size[Coll[(Coll[Byte], Long)]]] = {
      asRep[Size[Coll[(Coll[Byte], Long)]]](mkMethodCall(source,
        SizeBoxClass.getMethod("tokens"),
        WrappedArray.empty,
        true, true, element[Size[Coll[(Coll[Byte], Long)]]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeBoxClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeBox(p: Ref[SizeBox]): SizeBox = {
    if (p.node.isInstanceOf[SizeBox]) p.node.asInstanceOf[SizeBox]
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
    def resultType = SizeBoxCompanionElem
    override def toString = "SizeBox"
  }
  implicit final def unrefSizeBoxCompanionCtor(p: Ref[SizeBoxCompanionCtor]): SizeBoxCompanionCtor =
    p.node.asInstanceOf[SizeBoxCompanionCtor]

  lazy val RSizeBox: MutableLazy[SizeBoxCompanionCtor] = MutableLazy(new SizeBoxCompanionCtor {
    private val thisClass = classOf[SizeBoxCompanion]
  })

  object SizeBoxMethods {
    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Ref[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "propositionBytes" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeBox]] = unapply(exp.node)
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Ref[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "bytes" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeBox]] = unapply(exp.node)
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Ref[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "bytesWithoutRef" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeBox]] = unapply(exp.node)
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Ref[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "registers" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeBox]] = unapply(exp.node)
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Ref[SizeBox], Ref[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getReg" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SizeBox], Ref[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SizeBox], Ref[Byte], Elem[T]) forSome {type T}] = unapply(exp.node)
    }

    object tokens {
      def unapply(d: Def[_]): Nullable[Ref[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tokens" && receiver.elem.isInstanceOf[SizeBoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeBox]] = unapply(exp.node)
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
      ) extends LiftedConst[SSizeContext, SizeContext] with SizeContext
        with Def[SizeContext] with SizeContextConstMethods {
    // manual fix
    final def eVal: Elem[Context] = element[Context]

    val liftable: Liftable[SSizeContext, SizeContext] = LiftableSizeContext
    val resultType: Elem[SizeContext] = liftable.eW
  }

  trait SizeContextConstMethods extends SizeContext with SizeConstMethods[Context] { thisConst: Def[_] =>

    private val SizeContextClass = classOf[SizeContext]

    override def outputs: Ref[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("outputs"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Box]]]))
    }

    override def inputs: Ref[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("inputs"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Box]]]))
    }

    override def dataInputs: Ref[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("dataInputs"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Box]]]))
    }

    override def selfBox: Ref[Size[Box]] = {
      asRep[Size[Box]](mkMethodCall(self,
        SizeContextClass.getMethod("selfBox"),
        WrappedArray.empty,
        true, false, element[Size[Box]]))
    }

    override def lastBlockUtxoRootHash: Ref[Size[AvlTree]] = {
      asRep[Size[AvlTree]](mkMethodCall(self,
        SizeContextClass.getMethod("lastBlockUtxoRootHash"),
        WrappedArray.empty,
        true, false, element[Size[AvlTree]]))
    }

    override def headers: Ref[Size[Coll[Header]]] = {
      asRep[Size[Coll[Header]]](mkMethodCall(self,
        SizeContextClass.getMethod("headers"),
        WrappedArray.empty,
        true, false, element[Size[Coll[Header]]]))
    }

    override def preHeader: Ref[Size[PreHeader]] = {
      asRep[Size[PreHeader]](mkMethodCall(self,
        SizeContextClass.getMethod("preHeader"),
        WrappedArray.empty,
        true, false, element[Size[PreHeader]]))
    }

    override def getVar[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = {
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
    def lift(x: SSizeContext): Ref[SizeContext] = SizeContextConst(x)
    def unlift(w: Ref[SizeContext]): SSizeContext = w match {
      case Def(SizeContextConst(x: SSizeContext))
            => x.asInstanceOf[SSizeContext]
      case _ => unliftError(w)
    }
  }

  private val SizeContextClass = classOf[SizeContext]

  // entityAdapter for SizeContext trait
  case class SizeContextAdapter(source: Ref[SizeContext])
      extends Node with SizeContext
      with Def[SizeContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    val resultType: Elem[SizeContext] = element[SizeContext]
    override def transform(t: Transformer) = SizeContextAdapter(t(source))

    def outputs: Ref[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        SizeContextClass.getMethod("outputs"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Box]]]))
    }

    def inputs: Ref[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        SizeContextClass.getMethod("inputs"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Box]]]))
    }

    def dataInputs: Ref[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        SizeContextClass.getMethod("dataInputs"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Box]]]))
    }

    def selfBox: Ref[Size[Box]] = {
      asRep[Size[Box]](mkMethodCall(source,
        SizeContextClass.getMethod("selfBox"),
        WrappedArray.empty,
        true, true, element[Size[Box]]))
    }

    def lastBlockUtxoRootHash: Ref[Size[AvlTree]] = {
      asRep[Size[AvlTree]](mkMethodCall(source,
        SizeContextClass.getMethod("lastBlockUtxoRootHash"),
        WrappedArray.empty,
        true, true, element[Size[AvlTree]]))
    }

    def headers: Ref[Size[Coll[Header]]] = {
      asRep[Size[Coll[Header]]](mkMethodCall(source,
        SizeContextClass.getMethod("headers"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Header]]]))
    }

    def preHeader: Ref[Size[PreHeader]] = {
      asRep[Size[PreHeader]](mkMethodCall(source,
        SizeContextClass.getMethod("preHeader"),
        WrappedArray.empty,
        true, true, element[Size[PreHeader]]))
    }

    def getVar[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        SizeContextClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, true, element[Size[WOption[T]]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeContextClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeContext(p: Ref[SizeContext]): SizeContext = {
    if (p.node.isInstanceOf[SizeContext]) p.node.asInstanceOf[SizeContext]
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
    def resultType = SizeContextCompanionElem
    override def toString = "SizeContext"
  }
  implicit final def unrefSizeContextCompanionCtor(p: Ref[SizeContextCompanionCtor]): SizeContextCompanionCtor =
    p.node.asInstanceOf[SizeContextCompanionCtor]

  lazy val RSizeContext: MutableLazy[SizeContextCompanionCtor] = MutableLazy(new SizeContextCompanionCtor {
    private val thisClass = classOf[SizeContextCompanion]
  })

  object SizeContextMethods {
    object outputs {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "outputs" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object inputs {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "inputs" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object dataInputs {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "dataInputs" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object selfBox {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "selfBox" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object lastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "lastBlockUtxoRootHash" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object headers {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "headers" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object preHeader {
      def unapply(d: Def[_]): Nullable[Ref[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "preHeader" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeContext]] = unapply(exp.node)
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Ref[SizeContext], Ref[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getVar" && receiver.elem.isInstanceOf[SizeContextElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[SizeContext], Ref[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[SizeContext], Ref[Byte], Elem[T]) forSome {type T}] = unapply(exp.node)
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
      ) extends LiftedConst[SSizeBuilder, SizeBuilder] with SizeBuilder
        with Def[SizeBuilder] with SizeBuilderConstMethods {
    val liftable: Liftable[SSizeBuilder, SizeBuilder] = LiftableSizeBuilder
    val resultType: Elem[SizeBuilder] = liftable.eW
  }

  trait SizeBuilderConstMethods extends SizeBuilder  { thisConst: Def[_] =>

    private val SizeBuilderClass = classOf[SizeBuilder]

    override def mkSizeAnyValue(tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[SizeAnyValue] = {
      asRep[SizeAnyValue](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeAnyValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](tVal, valueSize),
        true, false, element[SizeAnyValue]))
    }

    override def mkSizeBox(propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[(Coll[Byte], Long)]]]): Ref[SizeBox] = {
      asRep[SizeBox](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeBox", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](propositionBytes, bytes, bytesWithoutRef, registers, tokens),
        true, false, element[SizeBox]))
    }

    override def mkSizeContext(outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[SizeContext] = {
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
    def lift(x: SSizeBuilder): Ref[SizeBuilder] = SizeBuilderConst(x)
    def unlift(w: Ref[SizeBuilder]): SSizeBuilder = w match {
      case Def(SizeBuilderConst(x: SSizeBuilder))
            => x.asInstanceOf[SSizeBuilder]
      case _ => unliftError(w)
    }
  }

  private val SizeBuilderClass = classOf[SizeBuilder]

  // entityAdapter for SizeBuilder trait
  case class SizeBuilderAdapter(source: Ref[SizeBuilder])
      extends Node with SizeBuilder
      with Def[SizeBuilder] {
    val resultType: Elem[SizeBuilder] = element[SizeBuilder]
    override def transform(t: Transformer) = SizeBuilderAdapter(t(source))

    def mkSizeAnyValue(tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[SizeAnyValue] = {
      asRep[SizeAnyValue](mkMethodCall(source,
        SizeBuilderClass.getMethod("mkSizeAnyValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](tVal, valueSize),
        true, true, element[SizeAnyValue]))
    }

    def mkSizeBox(propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[(Coll[Byte], Long)]]]): Ref[SizeBox] = {
      asRep[SizeBox](mkMethodCall(source,
        SizeBuilderClass.getMethod("mkSizeBox", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](propositionBytes, bytes, bytesWithoutRef, registers, tokens),
        true, true, element[SizeBox]))
    }

    def mkSizeContext(outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[SizeContext] = {
      asRep[SizeContext](mkMethodCall(source,
        SizeBuilderClass.getMethod("mkSizeContext", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars),
        true, true, element[SizeContext]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeBuilder(p: Ref[SizeBuilder]): SizeBuilder = {
    if (p.node.isInstanceOf[SizeBuilder]) p.node.asInstanceOf[SizeBuilder]
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
    def resultType = SizeBuilderCompanionElem
    override def toString = "SizeBuilder"
  }
  implicit final def unrefSizeBuilderCompanionCtor(p: Ref[SizeBuilderCompanionCtor]): SizeBuilderCompanionCtor =
    p.node.asInstanceOf[SizeBuilderCompanionCtor]

  lazy val RSizeBuilder: MutableLazy[SizeBuilderCompanionCtor] = MutableLazy(new SizeBuilderCompanionCtor {
    private val thisClass = classOf[SizeBuilderCompanion]
  })
} // of object SizeBuilder
  registerEntityObject("SizeBuilder", SizeBuilder)

  override def resetContext(): Unit = {
    super.resetContext()
    RSizeAnyValue.reset()
    RSizeSigmaProp.reset()
    RSizeBox.reset()
    RSizeContext.reset()
    RSizeBuilder.reset()
  }

  registerModule(CostedObjectsModule)
}

object CostedObjectsModule extends scalan.ModuleInfo("special.sigma", "CostedObjects")
}

trait CostedObjectsModule extends special.sigma.impl.CostedObjectsDefs {self: SigmaLibrary =>}
