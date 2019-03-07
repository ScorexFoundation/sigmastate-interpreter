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
import Header._
import PreHeader._
import Size._
import SizeAnyValue._
import SizeBox._
import SizeBuilder._
import SizeContext._
import WOption._
import WRType._

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
        List(),
        true, false, wRTypeElement(AnyElement)))
    }

    // manual fix
    override def valueSize: Rep[Size[Any]] = {
      asRep[Size[Any]](mkMethodCall(self,
        SizeAnyValueClass.getMethod("valueSize"),
        List(),
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

  // entityAdapter for SizeAnyValue trait
  case class SizeAnyValueAdapter(source: Rep[SizeAnyValue])
      extends SizeAnyValue with Def[SizeAnyValue] {
    override lazy val eVal: Elem[AnyValue] = implicitly[Elem[AnyValue]]
    val selfType: Elem[SizeAnyValue] = element[SizeAnyValue]
    override def transform(t: Transformer) = SizeAnyValueAdapter(t(source))
    private val thisClass = classOf[SizeAnyValue]

    // manual fix
    def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(source,
        thisClass.getMethod("tVal"),
        List(),
        true, true, wRTypeElement(AnyElement)))
    }

    def valueSize: Rep[Size[Any]] = {
      asRep[Size[Any]](mkMethodCall(source,
        thisClass.getMethod("valueSize"),
        List(),
        true, true, sizeElement(AnyElement)))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeAnyValue(p: Rep[SizeAnyValue]): SizeAnyValue = {
    if (p.rhs.isInstanceOf[SizeAnyValue@unchecked]) p.rhs.asInstanceOf[SizeAnyValue]
    else
      SizeAnyValueAdapter(p)
  }

  // familyElem
  class SizeAnyValueElem[To <: SizeAnyValue]
    extends SizeElem[AnyValue, To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSizeAnyValue.asLiftable[SSizeAnyValue, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeAnyValue], classOf[SSizeAnyValue], Set(
        "tVal", "valueSize"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(anyValueElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SizeAnyValue].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeAnyValue] => convertSizeAnyValue(x) }
      tryConvert(element[SizeAnyValue], this, x, conv)
    }

    def convertSizeAnyValue(x: Rep[SizeAnyValue]): Rep[To] = {
      x.elem match {
        case _: SizeAnyValueElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeAnyValueElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sizeAnyValueElement: Elem[SizeAnyValue] =
    new SizeAnyValueElem[SizeAnyValue]

  implicit case object SizeAnyValueCompanionElem extends CompanionElem[SizeAnyValueCompanionCtor] {
    lazy val tag = weakTypeTag[SizeAnyValueCompanionCtor]
    protected def getDefaultRep = RSizeAnyValue
  }

  abstract class SizeAnyValueCompanionCtor extends CompanionDef[SizeAnyValueCompanionCtor] with SizeAnyValueCompanion {
    def selfType = SizeAnyValueCompanionElem
    override def toString = "SizeAnyValue"
  }
  implicit def proxySizeAnyValueCompanionCtor(p: Rep[SizeAnyValueCompanionCtor]): SizeAnyValueCompanionCtor =
    proxyOps[SizeAnyValueCompanionCtor](p)

  lazy val RSizeAnyValue: Rep[SizeAnyValueCompanionCtor] = new SizeAnyValueCompanionCtor {
    private val thisClass = classOf[SizeAnyValueCompanion]
  }

  object SizeAnyValueMethods {
    object tVal {
      def unapply(d: Def[_]): Nullable[Rep[SizeAnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeAnyValueElem[_]] && method.getName == "tVal" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeAnyValueElem[_]] && method.getName == "valueSize" =>
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
        List(),
        true, false, element[Size[Coll[Byte]]]))
    }

    override def bytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("bytes"),
        List(),
        true, false, element[Size[Coll[Byte]]]))
    }

    override def bytesWithoutRef: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(self,
        SizeBoxClass.getMethod("bytesWithoutRef"),
        List(),
        true, false, element[Size[Coll[Byte]]]))
    }

    override def registers: Rep[Size[Coll[WOption[AnyValue]]]] = {
      asRep[Size[Coll[WOption[AnyValue]]]](mkMethodCall(self,
        SizeBoxClass.getMethod("registers"),
        List(),
        true, false, element[Size[Coll[WOption[AnyValue]]]]))
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

  // entityAdapter for SizeBox trait
  case class SizeBoxAdapter(source: Rep[SizeBox])
      extends SizeBox with Def[SizeBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    val selfType: Elem[SizeBox] = element[SizeBox]
    override def transform(t: Transformer) = SizeBoxAdapter(t(source))
    private val thisClass = classOf[SizeBox]

    def propositionBytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        thisClass.getMethod("propositionBytes"),
        List(),
        true, true, element[Size[Coll[Byte]]]))
    }

    def bytes: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        thisClass.getMethod("bytes"),
        List(),
        true, true, element[Size[Coll[Byte]]]))
    }

    def bytesWithoutRef: Rep[Size[Coll[Byte]]] = {
      asRep[Size[Coll[Byte]]](mkMethodCall(source,
        thisClass.getMethod("bytesWithoutRef"),
        List(),
        true, true, element[Size[Coll[Byte]]]))
    }

    def registers: Rep[Size[Coll[WOption[AnyValue]]]] = {
      asRep[Size[Coll[WOption[AnyValue]]]](mkMethodCall(source,
        thisClass.getMethod("registers"),
        List(),
        true, true, element[Size[Coll[WOption[AnyValue]]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeBox(p: Rep[SizeBox]): SizeBox = {
    if (p.rhs.isInstanceOf[SizeBox@unchecked]) p.rhs.asInstanceOf[SizeBox]
    else
      SizeBoxAdapter(p)
  }

  // familyElem
  class SizeBoxElem[To <: SizeBox]
    extends SizeElem[Box, To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSizeBox.asLiftable[SSizeBox, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeBox], classOf[SSizeBox], Set(
        "propositionBytes", "bytes", "bytesWithoutRef", "registers"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(boxElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SizeBox].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeBox] => convertSizeBox(x) }
      tryConvert(element[SizeBox], this, x, conv)
    }

    def convertSizeBox(x: Rep[SizeBox]): Rep[To] = {
      x.elem match {
        case _: SizeBoxElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeBoxElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sizeBoxElement: Elem[SizeBox] =
    new SizeBoxElem[SizeBox]

  implicit case object SizeBoxCompanionElem extends CompanionElem[SizeBoxCompanionCtor] {
    lazy val tag = weakTypeTag[SizeBoxCompanionCtor]
    protected def getDefaultRep = RSizeBox
  }

  abstract class SizeBoxCompanionCtor extends CompanionDef[SizeBoxCompanionCtor] with SizeBoxCompanion {
    def selfType = SizeBoxCompanionElem
    override def toString = "SizeBox"
  }
  implicit def proxySizeBoxCompanionCtor(p: Rep[SizeBoxCompanionCtor]): SizeBoxCompanionCtor =
    proxyOps[SizeBoxCompanionCtor](p)

  lazy val RSizeBox: Rep[SizeBoxCompanionCtor] = new SizeBoxCompanionCtor {
    private val thisClass = classOf[SizeBoxCompanion]
  }

  object SizeBoxMethods {
    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[SizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeBoxElem[_]] && method.getName == "propositionBytes" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeBoxElem[_]] && method.getName == "bytes" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeBoxElem[_]] && method.getName == "bytesWithoutRef" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeBoxElem[_]] && method.getName == "registers" =>
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
        List(),
        true, false, element[Size[Coll[Box]]]))
    }

    override def inputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("inputs"),
        List(),
        true, false, element[Size[Coll[Box]]]))
    }

    override def dataInputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(self,
        SizeContextClass.getMethod("dataInputs"),
        List(),
        true, false, element[Size[Coll[Box]]]))
    }

    override def selfBox: Rep[Size[Box]] = {
      asRep[Size[Box]](mkMethodCall(self,
        SizeContextClass.getMethod("selfBox"),
        List(),
        true, false, element[Size[Box]]))
    }

    override def lastBlockUtxoRootHash: Rep[Size[AvlTree]] = {
      asRep[Size[AvlTree]](mkMethodCall(self,
        SizeContextClass.getMethod("lastBlockUtxoRootHash"),
        List(),
        true, false, element[Size[AvlTree]]))
    }

    override def headers: Rep[Size[Coll[Header]]] = {
      asRep[Size[Coll[Header]]](mkMethodCall(self,
        SizeContextClass.getMethod("headers"),
        List(),
        true, false, element[Size[Coll[Header]]]))
    }

    override def preHeader: Rep[Size[PreHeader]] = {
      asRep[Size[PreHeader]](mkMethodCall(self,
        SizeContextClass.getMethod("preHeader"),
        List(),
        true, false, element[Size[PreHeader]]))
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

  // entityAdapter for SizeContext trait
  case class SizeContextAdapter(source: Rep[SizeContext])
      extends SizeContext with Def[SizeContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    val selfType: Elem[SizeContext] = element[SizeContext]
    override def transform(t: Transformer) = SizeContextAdapter(t(source))
    private val thisClass = classOf[SizeContext]

    def outputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        thisClass.getMethod("outputs"),
        List(),
        true, true, element[Size[Coll[Box]]]))
    }

    def inputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        thisClass.getMethod("inputs"),
        List(),
        true, true, element[Size[Coll[Box]]]))
    }

    def dataInputs: Rep[Size[Coll[Box]]] = {
      asRep[Size[Coll[Box]]](mkMethodCall(source,
        thisClass.getMethod("dataInputs"),
        List(),
        true, true, element[Size[Coll[Box]]]))
    }

    def selfBox: Rep[Size[Box]] = {
      asRep[Size[Box]](mkMethodCall(source,
        thisClass.getMethod("selfBox"),
        List(),
        true, true, element[Size[Box]]))
    }

    def lastBlockUtxoRootHash: Rep[Size[AvlTree]] = {
      asRep[Size[AvlTree]](mkMethodCall(source,
        thisClass.getMethod("lastBlockUtxoRootHash"),
        List(),
        true, true, element[Size[AvlTree]]))
    }

    def headers: Rep[Size[Coll[Header]]] = {
      asRep[Size[Coll[Header]]](mkMethodCall(source,
        thisClass.getMethod("headers"),
        List(),
        true, true, element[Size[Coll[Header]]]))
    }

    def preHeader: Rep[Size[PreHeader]] = {
      asRep[Size[PreHeader]](mkMethodCall(source,
        thisClass.getMethod("preHeader"),
        List(),
        true, true, element[Size[PreHeader]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeContext(p: Rep[SizeContext]): SizeContext = {
    if (p.rhs.isInstanceOf[SizeContext@unchecked]) p.rhs.asInstanceOf[SizeContext]
    else
      SizeContextAdapter(p)
  }

  // familyElem
  class SizeContextElem[To <: SizeContext]
    extends SizeElem[Context, To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSizeContext.asLiftable[SSizeContext, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeContext], classOf[SSizeContext], Set(
        "outputs", "inputs", "dataInputs", "selfBox", "lastBlockUtxoRootHash", "headers", "preHeader"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(contextElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SizeContext].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeContext] => convertSizeContext(x) }
      tryConvert(element[SizeContext], this, x, conv)
    }

    def convertSizeContext(x: Rep[SizeContext]): Rep[To] = {
      x.elem match {
        case _: SizeContextElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeContextElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sizeContextElement: Elem[SizeContext] =
    new SizeContextElem[SizeContext]

  implicit case object SizeContextCompanionElem extends CompanionElem[SizeContextCompanionCtor] {
    lazy val tag = weakTypeTag[SizeContextCompanionCtor]
    protected def getDefaultRep = RSizeContext
  }

  abstract class SizeContextCompanionCtor extends CompanionDef[SizeContextCompanionCtor] with SizeContextCompanion {
    def selfType = SizeContextCompanionElem
    override def toString = "SizeContext"
  }
  implicit def proxySizeContextCompanionCtor(p: Rep[SizeContextCompanionCtor]): SizeContextCompanionCtor =
    proxyOps[SizeContextCompanionCtor](p)

  lazy val RSizeContext: Rep[SizeContextCompanionCtor] = new SizeContextCompanionCtor {
    private val thisClass = classOf[SizeContextCompanion]
  }

  object SizeContextMethods {
    object outputs {
      def unapply(d: Def[_]): Nullable[Rep[SizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "outputs" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "inputs" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "dataInputs" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "selfBox" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "lastBlockUtxoRootHash" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "headers" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeContextElem[_]] && method.getName == "preHeader" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeContext]] = exp match {
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
        List(tVal, valueSize),
        true, false, element[SizeAnyValue]))
    }

    override def mkSizeBox(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]]): Rep[SizeBox] = {
      asRep[SizeBox](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeBox", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(propositionBytes, bytes, bytesWithoutRef, registers),
        true, false, element[SizeBox]))
    }

    override def mkSizeContext(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]]): Rep[SizeContext] = {
      asRep[SizeContext](mkMethodCall(self,
        SizeBuilderClass.getMethod("mkSizeContext", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader),
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

  // entityAdapter for SizeBuilder trait
  case class SizeBuilderAdapter(source: Rep[SizeBuilder])
      extends SizeBuilder with Def[SizeBuilder] {
    val selfType: Elem[SizeBuilder] = element[SizeBuilder]
    override def transform(t: Transformer) = SizeBuilderAdapter(t(source))
    private val thisClass = classOf[SizeBuilder]

    def mkSizeAnyValue(tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[SizeAnyValue] = {
      asRep[SizeAnyValue](mkMethodCall(source,
        thisClass.getMethod("mkSizeAnyValue", classOf[Sym], classOf[Sym]),
        List(tVal, valueSize),
        true, true, element[SizeAnyValue]))
    }

    def mkSizeBox(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]]): Rep[SizeBox] = {
      asRep[SizeBox](mkMethodCall(source,
        thisClass.getMethod("mkSizeBox", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(propositionBytes, bytes, bytesWithoutRef, registers),
        true, true, element[SizeBox]))
    }

    def mkSizeContext(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]]): Rep[SizeContext] = {
      asRep[SizeContext](mkMethodCall(source,
        thisClass.getMethod("mkSizeContext", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader),
        true, true, element[SizeContext]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeBuilder(p: Rep[SizeBuilder]): SizeBuilder = {
    if (p.rhs.isInstanceOf[SizeBuilder@unchecked]) p.rhs.asInstanceOf[SizeBuilder]
    else
      SizeBuilderAdapter(p)
  }

  // familyElem
  class SizeBuilderElem[To <: SizeBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSizeBuilder.asLiftable[SSizeBuilder, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeBuilder], classOf[SSizeBuilder], Set(
        "mkSizeAnyValue", "mkSizeBox", "mkSizeContext"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SizeBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeBuilder] => convertSizeBuilder(x) }
      tryConvert(element[SizeBuilder], this, x, conv)
    }

    def convertSizeBuilder(x: Rep[SizeBuilder]): Rep[To] = {
      x.elem match {
        case _: SizeBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sizeBuilderElement: Elem[SizeBuilder] =
    new SizeBuilderElem[SizeBuilder]

  implicit case object SizeBuilderCompanionElem extends CompanionElem[SizeBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[SizeBuilderCompanionCtor]
    protected def getDefaultRep = RSizeBuilder
  }

  abstract class SizeBuilderCompanionCtor extends CompanionDef[SizeBuilderCompanionCtor] with SizeBuilderCompanion {
    def selfType = SizeBuilderCompanionElem
    override def toString = "SizeBuilder"
  }
  implicit def proxySizeBuilderCompanionCtor(p: Rep[SizeBuilderCompanionCtor]): SizeBuilderCompanionCtor =
    proxyOps[SizeBuilderCompanionCtor](p)

  lazy val RSizeBuilder: Rep[SizeBuilderCompanionCtor] = new SizeBuilderCompanionCtor {
    private val thisClass = classOf[SizeBuilderCompanion]
  }

  object SizeBuilderMethods {
    object mkSizeAnyValue {
      def unapply(d: Def[_]): Nullable[(Rep[SizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SizeBuilderElem[_]] && method.getName == "mkSizeAnyValue" =>
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
      def unapply(d: Def[_]): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SizeBuilderElem[_]] && method.getName == "mkSizeBox" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeContext {
      def unapply(d: Def[_]): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SizeBuilderElem[_]] && method.getName == "mkSizeContext" =>
          val res = (receiver, args(0), args(1), args(2), args(3), args(4), args(5), args(6))
          Nullable(res).asInstanceOf[Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]])] = exp match {
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
