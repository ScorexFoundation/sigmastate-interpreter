package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait SizesDefs extends scalan.Scalan with Sizes {
  self: Library =>
import Coll._
import Size._
import WOption._
import WRType._
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._

object Size extends EntityObject("Size") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSize[Val] = special.collection.Size[Val]
  case class SizeConst[SVal, Val](
        constValue: SSize[SVal],
        lVal: Liftable[SVal, Val]
      ) extends LiftedConst[SSize[SVal], Size[Val]] with Size[Val]
        with Def[Size[Val]] with SizeConstMethods[Val] {
    implicit final def eVal: Elem[Val] = lVal.eW

    val liftable: Liftable[SSize[SVal], Size[Val]] = liftableSize(lVal)
    val resultType: Elem[Size[Val]] = liftable.eW
  }

  trait SizeConstMethods[Val] extends Size[Val]  { thisConst: Def[_] =>
    implicit def eVal: Elem[Val]
    private val SizeClass = classOf[Size[Val]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        SizeClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }

  case class LiftableSize[SVal, Val](lVal: Liftable[SVal, Val])
    extends Liftable[SSize[SVal], Size[Val]] {
    lazy val eW: Elem[Size[Val]] = sizeElement(lVal.eW)
    lazy val sourceType: RType[SSize[SVal]] = {
            implicit val tagSVal = lVal.sourceType.asInstanceOf[RType[SVal]]
      RType[SSize[SVal]]
    }
    def lift(x: SSize[SVal]): Ref[Size[Val]] = SizeConst(x, lVal)
    def unlift(w: Ref[Size[Val]]): SSize[SVal] = w match {
      case Def(SizeConst(x: SSize[_], _lVal))
            if _lVal == lVal => x.asInstanceOf[SSize[SVal]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableSize[SVal, Val](implicit lVal: Liftable[SVal,Val]): Liftable[SSize[SVal], Size[Val]] =
    LiftableSize(lVal)

  private val SizeClass = classOf[Size[_]]

  // entityAdapter for Size trait
  case class SizeAdapter[Val](source: Ref[Size[Val]])
      extends Node with Size[Val]
      with Def[Size[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asInstanceOf[Elem[Val]]

    val resultType: Elem[Size[Val]] = element[Size[Val]]
    override def transform(t: Transformer) = SizeAdapter[Val](t(source))

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSize[Val](p: Ref[Size[Val]]): Size[Val] = {
    if (p.node.isInstanceOf[Size[Val]@unchecked]) p.node.asInstanceOf[Size[Val]]
    else
      SizeAdapter(p)
  }

  // familyElem
  class SizeElem[Val, To <: Size[Val]](implicit _eVal: Elem[Val])
    extends EntityElem[To] {
    def eVal = _eVal

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSize[_], To](liftableSize(_eVal.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Size[Val]], classOf[SSize[_]], Set(
        "dataSize"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  implicit final def sizeElement[Val](implicit eVal: Elem[Val]): Elem[Size[Val]] =
    cachedElemByClass(eVal)(classOf[SizeElem[Val, Size[Val]]])

  implicit case object SizeCompanionElem extends CompanionElem[SizeCompanionCtor]

  abstract class SizeCompanionCtor extends CompanionDef[SizeCompanionCtor] with SizeCompanion {
    def resultType = SizeCompanionElem
    override def toString = "Size"
  }
  implicit final def unrefSizeCompanionCtor(p: Ref[SizeCompanionCtor]): SizeCompanionCtor =
    p.node.asInstanceOf[SizeCompanionCtor]

  lazy val RSize: MutableLazy[SizeCompanionCtor] = MutableLazy(new SizeCompanionCtor {
    private val thisClass = classOf[SizeCompanion]
  })

  object SizeMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Ref[Size[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "dataSize" && receiver.elem.isInstanceOf[SizeElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Size[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Size[Val]] forSome {type Val}] = unapply(exp.node)
    }
  }

  object SizeCompanionMethods {
  }
} // of object Size
  registerEntityObject("Size", Size)

object SizePrim extends EntityObject("SizePrim") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizePrim[Val] = special.collection.SizePrim[Val]
  case class SizePrimConst[SVal, Val](
        constValue: SSizePrim[SVal],
        lVal: Liftable[SVal, Val]
      ) extends LiftedConst[SSizePrim[SVal], SizePrim[Val]] with SizePrim[Val]
        with Def[SizePrim[Val]] with SizePrimConstMethods[Val] {
    implicit final def eVal: Elem[Val] = lVal.eW

    val liftable: Liftable[SSizePrim[SVal], SizePrim[Val]] = liftableSizePrim(lVal)
    val resultType: Elem[SizePrim[Val]] = liftable.eW
  }

  trait SizePrimConstMethods[Val] extends SizePrim[Val] with SizeConstMethods[Val] { thisConst: Def[_] =>
    implicit def eVal: Elem[Val]
    private val SizePrimClass = classOf[SizePrim[Val]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        SizePrimClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def tVal: Ref[WRType[Val]] = {
      asRep[WRType[Val]](mkMethodCall(self,
        SizePrimClass.getMethod("tVal"),
        WrappedArray.empty,
        true, false, element[WRType[Val]]))
    }
  }

  case class LiftableSizePrim[SVal, Val](lVal: Liftable[SVal, Val])
    extends Liftable[SSizePrim[SVal], SizePrim[Val]] {
    lazy val eW: Elem[SizePrim[Val]] = sizePrimElement(lVal.eW)
    lazy val sourceType: RType[SSizePrim[SVal]] = {
            implicit val tagSVal = lVal.sourceType.asInstanceOf[RType[SVal]]
      RType[SSizePrim[SVal]]
    }
    def lift(x: SSizePrim[SVal]): Ref[SizePrim[Val]] = SizePrimConst(x, lVal)
    def unlift(w: Ref[SizePrim[Val]]): SSizePrim[SVal] = w match {
      case Def(SizePrimConst(x: SSizePrim[_], _lVal))
            if _lVal == lVal => x.asInstanceOf[SSizePrim[SVal]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableSizePrim[SVal, Val](implicit lVal: Liftable[SVal,Val]): Liftable[SSizePrim[SVal], SizePrim[Val]] =
    LiftableSizePrim(lVal)

  private val SizePrimClass = classOf[SizePrim[_]]

  // entityAdapter for SizePrim trait
  case class SizePrimAdapter[Val](source: Ref[SizePrim[Val]])
      extends Node with SizePrim[Val]
      with Def[SizePrim[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asInstanceOf[Elem[Val]]

    val resultType: Elem[SizePrim[Val]] = element[SizePrim[Val]]
    override def transform(t: Transformer) = SizePrimAdapter[Val](t(source))

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizePrimClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def tVal: Ref[WRType[Val]] = {
      asRep[WRType[Val]](mkMethodCall(source,
        SizePrimClass.getMethod("tVal"),
        WrappedArray.empty,
        true, true, element[WRType[Val]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizePrim[Val](p: Ref[SizePrim[Val]]): SizePrim[Val] = {
    if (p.node.isInstanceOf[SizePrim[Val]@unchecked]) p.node.asInstanceOf[SizePrim[Val]]
    else
      SizePrimAdapter(p)
  }

  // familyElem
  class SizePrimElem[Val, To <: SizePrim[Val]](implicit _eVal: Elem[Val])
    extends SizeElem[Val, To] {
    override def eVal = _eVal

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizePrim[_], To](liftableSizePrim(_eVal.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizePrim[Val]], classOf[SSizePrim[_]], Set(
        "dataSize", "tVal"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  implicit final def sizePrimElement[Val](implicit eVal: Elem[Val]): Elem[SizePrim[Val]] =
    cachedElemByClass(eVal)(classOf[SizePrimElem[Val, SizePrim[Val]]])

  implicit case object SizePrimCompanionElem extends CompanionElem[SizePrimCompanionCtor]

  abstract class SizePrimCompanionCtor extends CompanionDef[SizePrimCompanionCtor] with SizePrimCompanion {
    def resultType = SizePrimCompanionElem
    override def toString = "SizePrim"
  }
  implicit final def unrefSizePrimCompanionCtor(p: Ref[SizePrimCompanionCtor]): SizePrimCompanionCtor =
    p.node.asInstanceOf[SizePrimCompanionCtor]

  lazy val RSizePrim: MutableLazy[SizePrimCompanionCtor] = MutableLazy(new SizePrimCompanionCtor {
    private val thisClass = classOf[SizePrimCompanion]
  })
} // of object SizePrim
  registerEntityObject("SizePrim", SizePrim)

object SizePair extends EntityObject("SizePair") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizePair[L, R] = special.collection.SizePair[L, R]
  case class SizePairConst[SL, SR, L, R](
        constValue: SSizePair[SL, SR],
        lL: Liftable[SL, L], lR: Liftable[SR, R]
      ) extends LiftedConst[SSizePair[SL, SR], SizePair[L, R]] with SizePair[L, R]
        with Def[SizePair[L, R]] with SizePairConstMethods[L, R] {
    implicit final def eL: Elem[L] = lL.eW
    implicit final def eR: Elem[R] = lR.eW
    implicit final def eVal: Elem[(L, R)] = element[(L, R)]

    val liftable: Liftable[SSizePair[SL, SR], SizePair[L, R]] = liftableSizePair(lL,lR)
    val resultType: Elem[SizePair[L, R]] = liftable.eW
  }

  trait SizePairConstMethods[L, R] extends SizePair[L, R] with SizeConstMethods[(L, R)] { thisConst: Def[_] =>
    implicit def eL: Elem[L]
    implicit def eR: Elem[R]
    private val SizePairClass = classOf[SizePair[L, R]]

    override def l: Ref[Size[L]] = {
      asRep[Size[L]](mkMethodCall(self,
        SizePairClass.getMethod("l"),
        WrappedArray.empty,
        true, false, element[Size[L]]))
    }

    override def r: Ref[Size[R]] = {
      asRep[Size[R]](mkMethodCall(self,
        SizePairClass.getMethod("r"),
        WrappedArray.empty,
        true, false, element[Size[R]]))
    }
  }

  case class LiftableSizePair[SL, SR, L, R](lL: Liftable[SL, L],lR: Liftable[SR, R])
    extends Liftable[SSizePair[SL, SR], SizePair[L, R]] {
    lazy val eW: Elem[SizePair[L, R]] = sizePairElement(lL.eW,lR.eW)
    lazy val sourceType: RType[SSizePair[SL, SR]] = {
            implicit val tagSL = lL.sourceType.asInstanceOf[RType[SL]]
      implicit val tagSR = lR.sourceType.asInstanceOf[RType[SR]]
      RType[SSizePair[SL, SR]]
    }
    def lift(x: SSizePair[SL, SR]): Ref[SizePair[L, R]] = SizePairConst(x, lL,lR)
    def unlift(w: Ref[SizePair[L, R]]): SSizePair[SL, SR] = w match {
      case Def(SizePairConst(x: SSizePair[_,_], _lL,_lR))
            if _lL == lL && _lR == lR => x.asInstanceOf[SSizePair[SL, SR]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableSizePair[SL, SR, L, R](implicit lL: Liftable[SL,L],lR: Liftable[SR,R]): Liftable[SSizePair[SL, SR], SizePair[L, R]] =
    LiftableSizePair(lL,lR)

  private val SizePairClass = classOf[SizePair[_, _]]

  // entityAdapter for SizePair trait
  case class SizePairAdapter[L, R](source: Ref[SizePair[L, R]])
      extends Node with SizePair[L, R]
      with Def[SizePair[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asInstanceOf[Elem[L]];
implicit lazy val eR = source.elem.typeArgs("R")._1.asInstanceOf[Elem[R]]
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val resultType: Elem[SizePair[L, R]] = element[SizePair[L, R]]
    override def transform(t: Transformer) = SizePairAdapter[L, R](t(source))

    def l: Ref[Size[L]] = {
      asRep[Size[L]](mkMethodCall(source,
        SizePairClass.getMethod("l"),
        WrappedArray.empty,
        true, true, element[Size[L]]))
    }

    def r: Ref[Size[R]] = {
      asRep[Size[R]](mkMethodCall(source,
        SizePairClass.getMethod("r"),
        WrappedArray.empty,
        true, true, element[Size[R]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizePairClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizePair[L, R](p: Ref[SizePair[L, R]]): SizePair[L, R] = {
    if (p.node.isInstanceOf[SizePair[L, R]@unchecked]) p.node.asInstanceOf[SizePair[L, R]]
    else
      SizePairAdapter(p)
  }

  // familyElem
  class SizePairElem[L, R, To <: SizePair[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends SizeElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizePair[_,_], To](liftableSizePair(_eL.liftable, _eR.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizePair[L, R]], classOf[SSizePair[_,_]], Set(
        "l", "r"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  implicit final def sizePairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[SizePair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[SizePairElem[L, R, SizePair[L, R]]])

  implicit case object SizePairCompanionElem extends CompanionElem[SizePairCompanionCtor]

  abstract class SizePairCompanionCtor extends CompanionDef[SizePairCompanionCtor] with SizePairCompanion {
    def resultType = SizePairCompanionElem
    override def toString = "SizePair"
  }
  implicit final def unrefSizePairCompanionCtor(p: Ref[SizePairCompanionCtor]): SizePairCompanionCtor =
    p.node.asInstanceOf[SizePairCompanionCtor]

  lazy val RSizePair: MutableLazy[SizePairCompanionCtor] = MutableLazy(new SizePairCompanionCtor {
    private val thisClass = classOf[SizePairCompanion]
  })

  object SizePairMethods {
    object l {
      def unapply(d: Def[_]): Nullable[Ref[SizePair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "l" && receiver.elem.isInstanceOf[SizePairElem[_, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizePair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizePair[L, R]] forSome {type L; type R}] = unapply(exp.node)
    }

    object r {
      def unapply(d: Def[_]): Nullable[Ref[SizePair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "r" && receiver.elem.isInstanceOf[SizePairElem[_, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizePair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizePair[L, R]] forSome {type L; type R}] = unapply(exp.node)
    }
  }

  object SizePairCompanionMethods {
  }
} // of object SizePair
  registerEntityObject("SizePair", SizePair)

object SizeColl extends EntityObject("SizeColl") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeColl[Item] = special.collection.SizeColl[Item]
  case class SizeCollConst[SItem, Item](
        constValue: SSizeColl[SItem],
        lItem: Liftable[SItem, Item]
      ) extends LiftedConst[SSizeColl[SItem], SizeColl[Item]] with SizeColl[Item]
        with Def[SizeColl[Item]] with SizeCollConstMethods[Item] {
    implicit final def eItem: Elem[Item] = lItem.eW
    implicit final def eVal: Elem[Coll[Item]] = element[Coll[Item]]

    val liftable: Liftable[SSizeColl[SItem], SizeColl[Item]] = liftableSizeColl(lItem)
    val resultType: Elem[SizeColl[Item]] = liftable.eW
  }

  trait SizeCollConstMethods[Item] extends SizeColl[Item] with SizeConstMethods[Coll[Item]] { thisConst: Def[_] =>
    implicit def eItem: Elem[Item]
    private val SizeCollClass = classOf[SizeColl[Item]]

    override def sizes: Ref[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(self,
        SizeCollClass.getMethod("sizes"),
        WrappedArray.empty,
        true, false, element[Coll[Size[Item]]]))
    }
  }

  case class LiftableSizeColl[SItem, Item](lItem: Liftable[SItem, Item])
    extends Liftable[SSizeColl[SItem], SizeColl[Item]] {
    lazy val eW: Elem[SizeColl[Item]] = sizeCollElement(lItem.eW)
    lazy val sourceType: RType[SSizeColl[SItem]] = {
            implicit val tagSItem = lItem.sourceType.asInstanceOf[RType[SItem]]
      RType[SSizeColl[SItem]]
    }
    def lift(x: SSizeColl[SItem]): Ref[SizeColl[Item]] = SizeCollConst(x, lItem)
    def unlift(w: Ref[SizeColl[Item]]): SSizeColl[SItem] = w match {
      case Def(SizeCollConst(x: SSizeColl[_], _lItem))
            if _lItem == lItem => x.asInstanceOf[SSizeColl[SItem]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableSizeColl[SItem, Item](implicit lItem: Liftable[SItem,Item]): Liftable[SSizeColl[SItem], SizeColl[Item]] =
    LiftableSizeColl(lItem)

  private val SizeCollClass = classOf[SizeColl[_]]

  // entityAdapter for SizeColl trait
  case class SizeCollAdapter[Item](source: Ref[SizeColl[Item]])
      extends Node with SizeColl[Item]
      with Def[SizeColl[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asInstanceOf[Elem[Item]]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    val resultType: Elem[SizeColl[Item]] = element[SizeColl[Item]]
    override def transform(t: Transformer) = SizeCollAdapter[Item](t(source))

    def sizes: Ref[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(source,
        SizeCollClass.getMethod("sizes"),
        WrappedArray.empty,
        true, true, element[Coll[Size[Item]]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeCollClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeColl[Item](p: Ref[SizeColl[Item]]): SizeColl[Item] = {
    if (p.node.isInstanceOf[SizeColl[Item]@unchecked]) p.node.asInstanceOf[SizeColl[Item]]
    else
      SizeCollAdapter(p)
  }

  // familyElem
  class SizeCollElem[Item, To <: SizeColl[Item]](implicit _eItem: Elem[Item])
    extends SizeElem[Coll[Item], To] {
    def eItem = _eItem

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeColl[_], To](liftableSizeColl(_eItem.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeColl[Item]], classOf[SSizeColl[_]], Set(
        "sizes"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(collElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  implicit final def sizeCollElement[Item](implicit eItem: Elem[Item]): Elem[SizeColl[Item]] =
    cachedElemByClass(eItem)(classOf[SizeCollElem[Item, SizeColl[Item]]])

  implicit case object SizeCollCompanionElem extends CompanionElem[SizeCollCompanionCtor]

  abstract class SizeCollCompanionCtor extends CompanionDef[SizeCollCompanionCtor] with SizeCollCompanion {
    def resultType = SizeCollCompanionElem
    override def toString = "SizeColl"
  }
  implicit final def unrefSizeCollCompanionCtor(p: Ref[SizeCollCompanionCtor]): SizeCollCompanionCtor =
    p.node.asInstanceOf[SizeCollCompanionCtor]

  lazy val RSizeColl: MutableLazy[SizeCollCompanionCtor] = MutableLazy(new SizeCollCompanionCtor {
    private val thisClass = classOf[SizeCollCompanion]
  })

  object SizeCollMethods {
    object sizes {
      def unapply(d: Def[_]): Nullable[Ref[SizeColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizes" && receiver.elem.isInstanceOf[SizeCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeColl[Item]] forSome {type Item}] = unapply(exp.node)
    }
  }

  object SizeCollCompanionMethods {
  }
} // of object SizeColl
  registerEntityObject("SizeColl", SizeColl)

object SizeFunc extends EntityObject("SizeFunc") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeFunc[Env, Arg, Res] = special.collection.SizeFunc[Env, Arg, Res]
  case class SizeFuncConst[SEnv, SArg, SRes, Env, Arg, Res](
        constValue: SSizeFunc[SEnv, SArg, SRes],
        lEnv: Liftable[SEnv, Env], lArg: Liftable[SArg, Arg], lRes: Liftable[SRes, Res]
      ) extends LiftedConst[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] with SizeFunc[Env, Arg, Res]
        with Def[SizeFunc[Env, Arg, Res]] with SizeFuncConstMethods[Env, Arg, Res] {
    implicit final def eEnv: Elem[Env] = lEnv.eW
    implicit final def eArg: Elem[Arg] = lArg.eW
    implicit final def eRes: Elem[Res] = lRes.eW
    implicit final def eVal: Elem[Arg => Res] = element[Arg => Res]

    val liftable: Liftable[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] = liftableSizeFunc(lEnv,lArg,lRes)
    val resultType: Elem[SizeFunc[Env, Arg, Res]] = liftable.eW
  }

  trait SizeFuncConstMethods[Env, Arg, Res] extends SizeFunc[Env, Arg, Res] with SizeConstMethods[Arg => Res] { thisConst: Def[_] =>
    implicit def eEnv: Elem[Env]
    implicit def eArg: Elem[Arg]
    implicit def eRes: Elem[Res]
    private val SizeFuncClass = classOf[SizeFunc[Env, Arg, Res]]

    override def sizeEnv: Ref[Size[Env]] = {
      asRep[Size[Env]](mkMethodCall(self,
        SizeFuncClass.getMethod("sizeEnv"),
        WrappedArray.empty,
        true, false, element[Size[Env]]))
    }
  }

  case class LiftableSizeFunc[SEnv, SArg, SRes, Env, Arg, Res](lEnv: Liftable[SEnv, Env],lArg: Liftable[SArg, Arg],lRes: Liftable[SRes, Res])
    extends Liftable[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] {
    lazy val eW: Elem[SizeFunc[Env, Arg, Res]] = sizeFuncElement(lEnv.eW,lArg.eW,lRes.eW)
    lazy val sourceType: RType[SSizeFunc[SEnv, SArg, SRes]] = {
            implicit val tagSEnv = lEnv.sourceType.asInstanceOf[RType[SEnv]]
      implicit val tagSArg = lArg.sourceType.asInstanceOf[RType[SArg]]
      implicit val tagSRes = lRes.sourceType.asInstanceOf[RType[SRes]]
      RType[SSizeFunc[SEnv, SArg, SRes]]
    }
    def lift(x: SSizeFunc[SEnv, SArg, SRes]): Ref[SizeFunc[Env, Arg, Res]] = SizeFuncConst(x, lEnv,lArg,lRes)
    def unlift(w: Ref[SizeFunc[Env, Arg, Res]]): SSizeFunc[SEnv, SArg, SRes] = w match {
      case Def(SizeFuncConst(x: SSizeFunc[_,_,_], _lEnv,_lArg,_lRes))
            if _lEnv == lEnv && _lArg == lArg && _lRes == lRes => x.asInstanceOf[SSizeFunc[SEnv, SArg, SRes]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableSizeFunc[SEnv, SArg, SRes, Env, Arg, Res](implicit lEnv: Liftable[SEnv,Env],lArg: Liftable[SArg,Arg],lRes: Liftable[SRes,Res]): Liftable[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] =
    LiftableSizeFunc(lEnv,lArg,lRes)

  private val SizeFuncClass = classOf[SizeFunc[_, _, _]]

  // entityAdapter for SizeFunc trait
  case class SizeFuncAdapter[Env, Arg, Res](source: Ref[SizeFunc[Env, Arg, Res]])
      extends Node with SizeFunc[Env, Arg, Res]
      with Def[SizeFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = source.elem.typeArgs("Env")._1.asInstanceOf[Elem[Env]];
implicit lazy val eArg = source.elem.typeArgs("Arg")._1.asInstanceOf[Elem[Arg]];
implicit lazy val eRes = source.elem.typeArgs("Res")._1.asInstanceOf[Elem[Res]]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    val resultType: Elem[SizeFunc[Env, Arg, Res]] = element[SizeFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = SizeFuncAdapter[Env, Arg, Res](t(source))

    def sizeEnv: Ref[Size[Env]] = {
      asRep[Size[Env]](mkMethodCall(source,
        SizeFuncClass.getMethod("sizeEnv"),
        WrappedArray.empty,
        true, true, element[Size[Env]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeFuncClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeFunc[Env, Arg, Res](p: Ref[SizeFunc[Env, Arg, Res]]): SizeFunc[Env, Arg, Res] = {
    if (p.node.isInstanceOf[SizeFunc[Env, Arg, Res]@unchecked]) p.node.asInstanceOf[SizeFunc[Env, Arg, Res]]
    else
      SizeFuncAdapter(p)
  }

  // familyElem
  class SizeFuncElem[Env, Arg, Res, To <: SizeFunc[Env, Arg, Res]](implicit _eEnv: Elem[Env], _eArg: Elem[Arg], _eRes: Elem[Res])
    extends SizeElem[Arg => Res, To] {
    def eEnv = _eEnv
    def eArg = _eArg
    def eRes = _eRes

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeFunc[_,_,_], To](liftableSizeFunc(_eEnv.liftable, _eArg.liftable, _eRes.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeFunc[Env, Arg, Res]], classOf[SSizeFunc[_,_,_]], Set(
        "sizeEnv"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(funcElement(element[Arg],element[Res])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }

  implicit final def sizeFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[SizeFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[SizeFuncElem[Env, Arg, Res, SizeFunc[Env, Arg, Res]]])

  implicit case object SizeFuncCompanionElem extends CompanionElem[SizeFuncCompanionCtor]

  abstract class SizeFuncCompanionCtor extends CompanionDef[SizeFuncCompanionCtor] with SizeFuncCompanion {
    def resultType = SizeFuncCompanionElem
    override def toString = "SizeFunc"
  }
  implicit final def unrefSizeFuncCompanionCtor(p: Ref[SizeFuncCompanionCtor]): SizeFuncCompanionCtor =
    p.node.asInstanceOf[SizeFuncCompanionCtor]

  lazy val RSizeFunc: MutableLazy[SizeFuncCompanionCtor] = MutableLazy(new SizeFuncCompanionCtor {
    private val thisClass = classOf[SizeFuncCompanion]
  })

  object SizeFuncMethods {
    object sizeEnv {
      def unapply(d: Def[_]): Nullable[Ref[SizeFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizeEnv" && receiver.elem.isInstanceOf[SizeFuncElem[_, _, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = unapply(exp.node)
    }
  }

  object SizeFuncCompanionMethods {
  }
} // of object SizeFunc
  registerEntityObject("SizeFunc", SizeFunc)

object SizeOption extends EntityObject("SizeOption") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeOption[T] = special.collection.SizeOption[T]
  case class SizeOptionConst[ST, T](
        constValue: SSizeOption[ST],
        lT: Liftable[ST, T]
      ) extends LiftedConst[SSizeOption[ST], SizeOption[T]] with SizeOption[T]
        with Def[SizeOption[T]] with SizeOptionConstMethods[T] {
    implicit final def eT: Elem[T] = lT.eW
    implicit final def eVal: Elem[WOption[T]] = element[WOption[T]]

    val liftable: Liftable[SSizeOption[ST], SizeOption[T]] = liftableSizeOption(lT)
    val resultType: Elem[SizeOption[T]] = liftable.eW
  }

  trait SizeOptionConstMethods[T] extends SizeOption[T] with SizeConstMethods[WOption[T]] { thisConst: Def[_] =>
    implicit def eT: Elem[T]
    private val SizeOptionClass = classOf[SizeOption[T]]

    override def sizeOpt: Ref[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(self,
        SizeOptionClass.getMethod("sizeOpt"),
        WrappedArray.empty,
        true, false, element[WOption[Size[T]]]))
    }
  }

  case class LiftableSizeOption[ST, T](lT: Liftable[ST, T])
    extends Liftable[SSizeOption[ST], SizeOption[T]] {
    lazy val eW: Elem[SizeOption[T]] = sizeOptionElement(lT.eW)
    lazy val sourceType: RType[SSizeOption[ST]] = {
            implicit val tagST = lT.sourceType.asInstanceOf[RType[ST]]
      RType[SSizeOption[ST]]
    }
    def lift(x: SSizeOption[ST]): Ref[SizeOption[T]] = SizeOptionConst(x, lT)
    def unlift(w: Ref[SizeOption[T]]): SSizeOption[ST] = w match {
      case Def(SizeOptionConst(x: SSizeOption[_], _lT))
            if _lT == lT => x.asInstanceOf[SSizeOption[ST]]
      case _ => unliftError(w)
    }
  }
  implicit final def liftableSizeOption[ST, T](implicit lT: Liftable[ST,T]): Liftable[SSizeOption[ST], SizeOption[T]] =
    LiftableSizeOption(lT)

  private val SizeOptionClass = classOf[SizeOption[_]]

  // entityAdapter for SizeOption trait
  case class SizeOptionAdapter[T](source: Ref[SizeOption[T]])
      extends Node with SizeOption[T]
      with Def[SizeOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asInstanceOf[Elem[T]]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val resultType: Elem[SizeOption[T]] = element[SizeOption[T]]
    override def transform(t: Transformer) = SizeOptionAdapter[T](t(source))

    def sizeOpt: Ref[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(source,
        SizeOptionClass.getMethod("sizeOpt"),
        WrappedArray.empty,
        true, true, element[WOption[Size[T]]]))
    }

    def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeOptionClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit final def unrefSizeOption[T](p: Ref[SizeOption[T]]): SizeOption[T] = {
    if (p.node.isInstanceOf[SizeOption[T]@unchecked]) p.node.asInstanceOf[SizeOption[T]]
    else
      SizeOptionAdapter(p)
  }

  // familyElem
  class SizeOptionElem[T, To <: SizeOption[T]](implicit _eT: Elem[T])
    extends SizeElem[WOption[T], To] {
    def eT = _eT

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeOption[_], To](liftableSizeOption(_eT.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeOption[T]], classOf[SSizeOption[_]], Set(
        "sizeOpt"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(wOptionElement(element[T])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }

  implicit final def sizeOptionElement[T](implicit eT: Elem[T]): Elem[SizeOption[T]] =
    cachedElemByClass(eT)(classOf[SizeOptionElem[T, SizeOption[T]]])

  implicit case object SizeOptionCompanionElem extends CompanionElem[SizeOptionCompanionCtor]

  abstract class SizeOptionCompanionCtor extends CompanionDef[SizeOptionCompanionCtor] with SizeOptionCompanion {
    def resultType = SizeOptionCompanionElem
    override def toString = "SizeOption"
  }
  implicit final def unrefSizeOptionCompanionCtor(p: Ref[SizeOptionCompanionCtor]): SizeOptionCompanionCtor =
    p.node.asInstanceOf[SizeOptionCompanionCtor]

  lazy val RSizeOption: MutableLazy[SizeOptionCompanionCtor] = MutableLazy(new SizeOptionCompanionCtor {
    private val thisClass = classOf[SizeOptionCompanion]
  })

  object SizeOptionMethods {
    object sizeOpt {
      def unapply(d: Def[_]): Nullable[Ref[SizeOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizeOpt" && receiver.elem.isInstanceOf[SizeOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[SizeOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[SizeOption[T]] forSome {type T}] = unapply(exp.node)
    }
  }

  object SizeOptionCompanionMethods {
  }
} // of object SizeOption
  registerEntityObject("SizeOption", SizeOption)

  override def resetContext(): Unit = {
    super.resetContext()
    RSize.reset()
    RSizePrim.reset()
    RSizePair.reset()
    RSizeColl.reset()
    RSizeFunc.reset()
    RSizeOption.reset()
  }

  registerModule(SizesModule)
}

object SizesModule extends scalan.ModuleInfo("special.collection", "Sizes")
}

trait SizesModule extends special.collection.impl.SizesDefs {self: Library =>}
