package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import scalan.OverloadHack.Overloaded1 // manual fix

  // Abs -----------------------------------
trait SigmaDslDefs extends scalan.Scalan with SigmaDsl {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AnyValue._
import AvlTree._
import BigInt._
import Box._
import Coll._
import CollBuilder._
import Context._
import CostModel._
import CostedBuilder._
import GroupElement._
import Header._
import MonoidBuilder._
import PreHeader._
import SigmaContract._
import SigmaDslBuilder._
import SigmaProp._
import WBigInteger._
import WOption._
import WRType._

object CostModel extends EntityObject("CostModel") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SCostModel = special.sigma.CostModel
  case class CostModelConst(
        constValue: SCostModel
      ) extends CostModel with LiftedConst[SCostModel, CostModel]
        with Def[CostModel] with CostModelConstMethods {
    val liftable: Liftable[SCostModel, CostModel] = LiftableCostModel
    val selfType: Elem[CostModel] = liftable.eW
  }

  trait CostModelConstMethods extends CostModel  { thisConst: Def[_] =>

    private val CostModelClass = classOf[CostModel]

    override def AccessBox: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("AccessBox"),
        List(),
        true, false, element[Int]))
    }

    override def AccessAvlTree: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("AccessAvlTree"),
        List(),
        true, false, element[Int]))
    }

    override def GetVar: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("GetVar"),
        List(),
        true, false, element[Int]))
    }

    override def DeserializeVar: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("DeserializeVar"),
        List(),
        true, false, element[Int]))
    }

    override def GetRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("GetRegister"),
        List(),
        true, false, element[Int]))
    }

    override def DeserializeRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("DeserializeRegister"),
        List(),
        true, false, element[Int]))
    }

    override def SelectField: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("SelectField"),
        List(),
        true, false, element[Int]))
    }

    override def CollectionConst: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("CollectionConst"),
        List(),
        true, false, element[Int]))
    }

    override def AccessKiloByteOfData: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("AccessKiloByteOfData"),
        List(),
        true, false, element[Int]))
    }

    override def dataSize[T](x: Rep[T])(implicit cT: Elem[T]): Rep[Long] = {
      implicit val eT = x.elem
      asRep[Long](mkMethodCall(self,
        CostModelClass.getMethod("dataSize", classOf[Sym], classOf[Elem[_]]),
        List(x, cT),
        true, false, element[Long]))
    }

    override def PubKeySize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        CostModelClass.getMethod("PubKeySize"),
        List(),
        true, false, element[Long]))
    }
  }

  implicit object LiftableCostModel
    extends Liftable[SCostModel, CostModel] {
    lazy val eW: Elem[CostModel] = costModelElement
    lazy val sourceType: RType[SCostModel] = {
      RType[SCostModel]
    }
    def lift(x: SCostModel): Rep[CostModel] = CostModelConst(x)
    def unlift(w: Rep[CostModel]): SCostModel = w match {
      case Def(CostModelConst(x: SCostModel))
            => x.asInstanceOf[SCostModel]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for CostModel trait
  case class CostModelAdapter(source: Rep[CostModel])
      extends CostModel with Def[CostModel] {
    val selfType: Elem[CostModel] = element[CostModel]
    override def transform(t: Transformer) = CostModelAdapter(t(source))
    private val thisClass = classOf[CostModel]

    def AccessBox: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("AccessBox"),
        List(),
        true, true, element[Int]))
    }

    def AccessAvlTree: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("AccessAvlTree"),
        List(),
        true, true, element[Int]))
    }

    def GetVar: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("GetVar"),
        List(),
        true, true, element[Int]))
    }

    def DeserializeVar: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("DeserializeVar"),
        List(),
        true, true, element[Int]))
    }

    def GetRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("GetRegister"),
        List(),
        true, true, element[Int]))
    }

    def DeserializeRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("DeserializeRegister"),
        List(),
        true, true, element[Int]))
    }

    def SelectField: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("SelectField"),
        List(),
        true, true, element[Int]))
    }

    def CollectionConst: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("CollectionConst"),
        List(),
        true, true, element[Int]))
    }

    def AccessKiloByteOfData: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("AccessKiloByteOfData"),
        List(),
        true, true, element[Int]))
    }

    def dataSize[T](x: Rep[T])(implicit cT: Elem[T]): Rep[Long] = {
      implicit val eT = x.elem
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize", classOf[Sym], classOf[Elem[_]]),
        List(x, cT),
        true, true, element[Long]))
    }

    def PubKeySize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("PubKeySize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostModel(p: Rep[CostModel]): CostModel = {
    if (p.rhs.isInstanceOf[CostModel@unchecked]) p.rhs.asInstanceOf[CostModel]
    else
      CostModelAdapter(p)
  }

  // familyElem
  class CostModelElem[To <: CostModel]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableCostModel.asLiftable[SCostModel, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[CostModel], classOf[SCostModel], Set(
        "AccessBox", "AccessAvlTree", "GetVar", "DeserializeVar", "GetRegister", "DeserializeRegister", "SelectField", "CollectionConst", "AccessKiloByteOfData", "dataSize", "PubKeySize"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CostModel].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostModel] => convertCostModel(x) }
      tryConvert(element[CostModel], this, x, conv)
    }

    def convertCostModel(x: Rep[CostModel]): Rep[To] = {
      x.elem match {
        case _: CostModelElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostModelElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val costModelElement: Elem[CostModel] =
    new CostModelElem[CostModel]

  implicit case object CostModelCompanionElem extends CompanionElem[CostModelCompanionCtor] {
    lazy val tag = weakTypeTag[CostModelCompanionCtor]
    protected def getDefaultRep = RCostModel
  }

  abstract class CostModelCompanionCtor extends CompanionDef[CostModelCompanionCtor] with CostModelCompanion {
    def selfType = CostModelCompanionElem
    override def toString = "CostModel"
  }
  implicit def proxyCostModelCompanionCtor(p: Rep[CostModelCompanionCtor]): CostModelCompanionCtor =
    proxyOps[CostModelCompanionCtor](p)

  lazy val RCostModel: Rep[CostModelCompanionCtor] = new CostModelCompanionCtor {
    private val thisClass = classOf[CostModelCompanion]
  }

  object CostModelMethods {
    object AccessBox {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "AccessBox" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object AccessAvlTree {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "AccessAvlTree" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object GetVar {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "GetVar" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object DeserializeVar {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "DeserializeVar" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object GetRegister {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "GetRegister" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object DeserializeRegister {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "DeserializeRegister" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SelectField {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "SelectField" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object CollectionConst {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "CollectionConst" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object AccessKiloByteOfData {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "AccessKiloByteOfData" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[(Rep[CostModel], Rep[T], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "dataSize" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostModel], Rep[T], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostModel], Rep[T], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKeySize {
      def unapply(d: Def[_]): Nullable[Rep[CostModel]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostModelElem[_]] && method.getName == "PubKeySize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostModel]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostModel]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostModelCompanionMethods {
  }
} // of object CostModel
  registerEntityObject("CostModel", CostModel)

object BigInt extends EntityObject("BigInt") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SBigInt = special.sigma.BigInt
  case class BigIntConst(
        constValue: SBigInt
      ) extends BigInt with LiftedConst[SBigInt, BigInt]
        with Def[BigInt] with BigIntConstMethods {
    val liftable: Liftable[SBigInt, BigInt] = LiftableBigInt
    val selfType: Elem[BigInt] = liftable.eW
  }

  trait BigIntConstMethods extends BigInt  { thisConst: Def[_] =>

    private val BigIntClass = classOf[BigInt]

    override def toByte: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        BigIntClass.getMethod("toByte"),
        List(),
        true, false, element[Byte]))
    }

    override def toShort: Rep[Short] = {
      asRep[Short](mkMethodCall(self,
        BigIntClass.getMethod("toShort"),
        List(),
        true, false, element[Short]))
    }

    override def toInt: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        BigIntClass.getMethod("toInt"),
        List(),
        true, false, element[Int]))
    }

    override def toLong: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        BigIntClass.getMethod("toLong"),
        List(),
        true, false, element[Long]))
    }

    override def toBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BigIntClass.getMethod("toBytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def toBits: Rep[Coll[Boolean]] = {
      asRep[Coll[Boolean]](mkMethodCall(self,
        BigIntClass.getMethod("toBits"),
        List(),
        true, false, element[Coll[Boolean]]))
    }

    override def toAbs: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("toAbs"),
        List(),
        true, false, element[BigInt]))
    }

    override def compareTo(that: Rep[BigInt]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        BigIntClass.getMethod("compareTo", classOf[Sym]),
        List(that),
        true, false, element[Int]))
    }

    override def modQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("modQ"),
        List(),
        true, false, element[BigInt]))
    }

    override def plusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("plusModQ", classOf[Sym]),
        List(other),
        true, false, element[BigInt]))
    }

    override def minusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("minusModQ", classOf[Sym]),
        List(other),
        true, false, element[BigInt]))
    }

    override def multModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("multModQ", classOf[Sym]),
        List(other),
        true, false, element[BigInt]))
    }

    override def inverseModQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("inverseModQ"),
        List(),
        true, false, element[BigInt]))
    }

    override def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        BigIntClass.getMethod("signum"),
        List(),
        true, false, element[Int]))
    }

    override def add(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("add", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def subtract(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("subtract", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def multiply(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("multiply", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def divide(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("divide", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def mod(m: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("mod", classOf[Sym]),
        List(m),
        true, false, element[BigInt]))
    }

    override def remainder(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("remainder", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def min(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("min", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def max(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("max", classOf[Sym]),
        List(that),
        true, false, element[BigInt]))
    }

    override def negate: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("negate"),
        List(),
        true, false, element[BigInt]))
    }
  }

  implicit object LiftableBigInt
    extends Liftable[SBigInt, BigInt] {
    lazy val eW: Elem[BigInt] = bigIntElement
    lazy val sourceType: RType[SBigInt] = {
      RType[SBigInt]
    }
    def lift(x: SBigInt): Rep[BigInt] = BigIntConst(x)
    def unlift(w: Rep[BigInt]): SBigInt = w match {
      case Def(BigIntConst(x: SBigInt))
            => x.asInstanceOf[SBigInt]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for BigInt trait
  case class BigIntAdapter(source: Rep[BigInt])
      extends BigInt with Def[BigInt] {
    val selfType: Elem[BigInt] = element[BigInt]
    override def transform(t: Transformer) = BigIntAdapter(t(source))
    private val thisClass = classOf[BigInt]

    def toByte: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        thisClass.getMethod("toByte"),
        List(),
        true, true, element[Byte]))
    }

    def toShort: Rep[Short] = {
      asRep[Short](mkMethodCall(source,
        thisClass.getMethod("toShort"),
        List(),
        true, true, element[Short]))
    }

    def toInt: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("toInt"),
        List(),
        true, true, element[Int]))
    }

    def toLong: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("toLong"),
        List(),
        true, true, element[Long]))
    }

    def toBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("toBytes"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def toBits: Rep[Coll[Boolean]] = {
      asRep[Coll[Boolean]](mkMethodCall(source,
        thisClass.getMethod("toBits"),
        List(),
        true, true, element[Coll[Boolean]]))
    }

    def toAbs: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("toAbs"),
        List(),
        true, true, element[BigInt]))
    }

    def compareTo(that: Rep[BigInt]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("compareTo", classOf[Sym]),
        List(that),
        true, true, element[Int]))
    }

    def modQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("modQ"),
        List(),
        true, true, element[BigInt]))
    }

    def plusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("plusModQ", classOf[Sym]),
        List(other),
        true, true, element[BigInt]))
    }

    def minusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("minusModQ", classOf[Sym]),
        List(other),
        true, true, element[BigInt]))
    }

    def multModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("multModQ", classOf[Sym]),
        List(other),
        true, true, element[BigInt]))
    }

    def inverseModQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("inverseModQ"),
        List(),
        true, true, element[BigInt]))
    }

    def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("signum"),
        List(),
        true, true, element[Int]))
    }

    def add(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("add", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def subtract(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("subtract", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def multiply(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("multiply", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def divide(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("divide", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def mod(m: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("mod", classOf[Sym]),
        List(m),
        true, true, element[BigInt]))
    }

    def remainder(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("remainder", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def min(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("min", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def max(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("max", classOf[Sym]),
        List(that),
        true, true, element[BigInt]))
    }

    def negate: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("negate"),
        List(),
        true, true, element[BigInt]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBigInt(p: Rep[BigInt]): BigInt = {
    if (p.rhs.isInstanceOf[BigInt@unchecked]) p.rhs.asInstanceOf[BigInt]
    else
      BigIntAdapter(p)
  }

  // familyElem
  class BigIntElem[To <: BigInt]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableBigInt.asLiftable[SBigInt, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[BigInt], classOf[SBigInt], Set(
        "toByte", "toShort", "toInt", "toLong", "toBytes", "toBits", "toAbs", "compareTo", "modQ", "plusModQ", "minusModQ", "multModQ", "inverseModQ", "signum", "add", "subtract", "multiply", "divide", "mod", "remainder", "min", "max", "negate"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[BigInt].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[BigInt] => convertBigInt(x) }
      tryConvert(element[BigInt], this, x, conv)
    }

    def convertBigInt(x: Rep[BigInt]): Rep[To] = {
      x.elem match {
        case _: BigIntElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have BigIntElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val bigIntElement: Elem[BigInt] =
    new BigIntElem[BigInt]

  implicit case object BigIntCompanionElem extends CompanionElem[BigIntCompanionCtor] {
    lazy val tag = weakTypeTag[BigIntCompanionCtor]
    protected def getDefaultRep = RBigInt
  }

  abstract class BigIntCompanionCtor extends CompanionDef[BigIntCompanionCtor] with BigIntCompanion {
    def selfType = BigIntCompanionElem
    override def toString = "BigInt"
  }
  implicit def proxyBigIntCompanionCtor(p: Rep[BigIntCompanionCtor]): BigIntCompanionCtor =
    proxyOps[BigIntCompanionCtor](p)

  lazy val RBigInt: Rep[BigIntCompanionCtor] = new BigIntCompanionCtor {
    private val thisClass = classOf[BigIntCompanion]
  }

  object BigIntMethods {
    object toByte {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toByte" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toShort {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toShort" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toInt {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toInt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toLong {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toLong" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toBytes {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toBits {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toBits" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toAbs {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "toAbs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object compareTo {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "compareTo" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object modQ {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "modQ" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object plusModQ {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "plusModQ" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object minusModQ {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "minusModQ" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multModQ {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "multModQ" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object inverseModQ {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "inverseModQ" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object signum {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "signum" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "subtract" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "divide" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "mod" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object remainder {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "remainder" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "min" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "max" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object negate {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BigIntElem[_]] && method.getName == "negate" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object BigIntCompanionMethods {
  }
} // of object BigInt
  registerEntityObject("BigInt", BigInt)

object GroupElement extends EntityObject("GroupElement") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SGroupElement = special.sigma.GroupElement
  case class GroupElementConst(
        constValue: SGroupElement
      ) extends GroupElement with LiftedConst[SGroupElement, GroupElement]
        with Def[GroupElement] with GroupElementConstMethods {
    val liftable: Liftable[SGroupElement, GroupElement] = LiftableGroupElement
    val selfType: Elem[GroupElement] = liftable.eW
  }

  trait GroupElementConstMethods extends GroupElement  { thisConst: Def[_] =>

    private val GroupElementClass = classOf[GroupElement]

    override def isInfinity: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        GroupElementClass.getMethod("isInfinity"),
        List(),
        true, false, element[Boolean]))
    }

    override def multiply(k: Rep[BigInt]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        GroupElementClass.getMethod("multiply", classOf[Sym]),
        List(k),
        true, false, element[GroupElement]))
    }

    override def add(that: Rep[GroupElement]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        GroupElementClass.getMethod("add", classOf[Sym]),
        List(that),
        true, false, element[GroupElement]))
    }

    override def negate: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        GroupElementClass.getMethod("negate"),
        List(),
        true, false, element[GroupElement]))
    }

    override def getEncoded(compressed: Rep[Boolean]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        GroupElementClass.getMethod("getEncoded", classOf[Sym]),
        List(compressed),
        true, false, element[Coll[Byte]]))
    }
  }

  implicit object LiftableGroupElement
    extends Liftable[SGroupElement, GroupElement] {
    lazy val eW: Elem[GroupElement] = groupElementElement
    lazy val sourceType: RType[SGroupElement] = {
      RType[SGroupElement]
    }
    def lift(x: SGroupElement): Rep[GroupElement] = GroupElementConst(x)
    def unlift(w: Rep[GroupElement]): SGroupElement = w match {
      case Def(GroupElementConst(x: SGroupElement))
            => x.asInstanceOf[SGroupElement]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for GroupElement trait
  case class GroupElementAdapter(source: Rep[GroupElement])
      extends GroupElement with Def[GroupElement] {
    val selfType: Elem[GroupElement] = element[GroupElement]
    override def transform(t: Transformer) = GroupElementAdapter(t(source))
    private val thisClass = classOf[GroupElement]

    def isInfinity: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isInfinity"),
        List(),
        true, true, element[Boolean]))
    }

    def multiply(k: Rep[BigInt]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("multiply", classOf[Sym]),
        List(k),
        true, true, element[GroupElement]))
    }

    def add(that: Rep[GroupElement]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("add", classOf[Sym]),
        List(that),
        true, true, element[GroupElement]))
    }

    def negate: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("negate"),
        List(),
        true, true, element[GroupElement]))
    }

    def getEncoded(compressed: Rep[Boolean]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("getEncoded", classOf[Sym]),
        List(compressed),
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyGroupElement(p: Rep[GroupElement]): GroupElement = {
    if (p.rhs.isInstanceOf[GroupElement@unchecked]) p.rhs.asInstanceOf[GroupElement]
    else
      GroupElementAdapter(p)
  }

  // familyElem
  class GroupElementElem[To <: GroupElement]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableGroupElement.asLiftable[SGroupElement, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[GroupElement], classOf[SGroupElement], Set(
        "isInfinity", "multiply", "add", "negate", "getEncoded"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[GroupElement].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[GroupElement] => convertGroupElement(x) }
      tryConvert(element[GroupElement], this, x, conv)
    }

    def convertGroupElement(x: Rep[GroupElement]): Rep[To] = {
      x.elem match {
        case _: GroupElementElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have GroupElementElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val groupElementElement: Elem[GroupElement] =
    new GroupElementElem[GroupElement]

  implicit case object GroupElementCompanionElem extends CompanionElem[GroupElementCompanionCtor] {
    lazy val tag = weakTypeTag[GroupElementCompanionCtor]
    protected def getDefaultRep = RGroupElement
  }

  abstract class GroupElementCompanionCtor extends CompanionDef[GroupElementCompanionCtor] with GroupElementCompanion {
    def selfType = GroupElementCompanionElem
    override def toString = "GroupElement"
  }
  implicit def proxyGroupElementCompanionCtor(p: Rep[GroupElementCompanionCtor]): GroupElementCompanionCtor =
    proxyOps[GroupElementCompanionCtor](p)

  lazy val RGroupElement: Rep[GroupElementCompanionCtor] = new GroupElementCompanionCtor {
    private val thisClass = classOf[GroupElementCompanion]
  }

  object GroupElementMethods {
    object isInfinity {
      def unapply(d: Def[_]): Nullable[Rep[GroupElement]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GroupElementElem[_]] && method.getName == "isInfinity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[GroupElement]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[GroupElement]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[GroupElement], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[GroupElementElem[_]] && method.getName == "multiply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[GroupElement], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[GroupElement], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[GroupElement], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[GroupElementElem[_]] && method.getName == "add" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[GroupElement], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[GroupElement], Rep[GroupElement])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object negate {
      def unapply(d: Def[_]): Nullable[Rep[GroupElement]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[GroupElementElem[_]] && method.getName == "negate" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[GroupElement]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[GroupElement]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getEncoded {
      def unapply(d: Def[_]): Nullable[(Rep[GroupElement], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[GroupElementElem[_]] && method.getName == "getEncoded" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[GroupElement], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[GroupElement], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object GroupElementCompanionMethods {
  }
} // of object GroupElement
  registerEntityObject("GroupElement", GroupElement)

object SigmaProp extends EntityObject("SigmaProp") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaProp = special.sigma.SigmaProp
  case class SigmaPropConst(
        constValue: SSigmaProp
      ) extends SigmaProp with LiftedConst[SSigmaProp, SigmaProp]
        with Def[SigmaProp] with SigmaPropConstMethods {
    val liftable: Liftable[SSigmaProp, SigmaProp] = LiftableSigmaProp
    val selfType: Elem[SigmaProp] = liftable.eW
  }

  trait SigmaPropConstMethods extends SigmaProp  { thisConst: Def[_] =>

    private val SigmaPropClass = classOf[SigmaProp]

    override def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaPropClass.getMethod("isValid"),
        List(),
        true, false, element[Boolean]))
    }

    override def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaPropClass.getMethod("propBytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    // manual fix &&
    override def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    // manual fix &&
    override def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    // manual fix ||
    override def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    // manual fix ||
    override def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, false, element[SigmaProp]))
    }
  }

  implicit object LiftableSigmaProp
    extends Liftable[SSigmaProp, SigmaProp] {
    lazy val eW: Elem[SigmaProp] = sigmaPropElement
    lazy val sourceType: RType[SSigmaProp] = {
      RType[SSigmaProp]
    }
    def lift(x: SSigmaProp): Rep[SigmaProp] = SigmaPropConst(x)
    def unlift(w: Rep[SigmaProp]): SSigmaProp = w match {
      case Def(SigmaPropConst(x: SSigmaProp))
            => x.asInstanceOf[SSigmaProp]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for SigmaProp trait
  case class SigmaPropAdapter(source: Rep[SigmaProp])
      extends SigmaProp with Def[SigmaProp] {
    val selfType: Elem[SigmaProp] = element[SigmaProp]
    override def transform(t: Transformer) = SigmaPropAdapter(t(source))
    private val thisClass = classOf[SigmaProp]

    def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isValid"),
        List(),
        true, true, element[Boolean]))
    }

    def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("propBytes"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    // manual fix &&
    def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    // manual fix &&
    def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, true, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, true, element[SigmaProp]))
    }

    def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("lazyAnd", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("lazyOr", classOf[Sym]),
        List(other),
        true, true, element[SigmaProp]))
    }

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaProp(p: Rep[SigmaProp]): SigmaProp = {
    if (p.rhs.isInstanceOf[SigmaProp@unchecked]) p.rhs.asInstanceOf[SigmaProp]
    else
      SigmaPropAdapter(p)
  }

  // familyElem
  class SigmaPropElem[To <: SigmaProp]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSigmaProp.asLiftable[SSigmaProp, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaProp], classOf[SSigmaProp], Set(
        "isValid", "propBytes", "$amp$amp", "$amp$amp", "$bar$bar", "$bar$bar"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaProp].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaProp] => convertSigmaProp(x) }
      tryConvert(element[SigmaProp], this, x, conv)
    }

    def convertSigmaProp(x: Rep[SigmaProp]): Rep[To] = {
      x.elem match {
        case _: SigmaPropElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaPropElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sigmaPropElement: Elem[SigmaProp] =
    new SigmaPropElem[SigmaProp]

  implicit case object SigmaPropCompanionElem extends CompanionElem[SigmaPropCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaPropCompanionCtor]
    protected def getDefaultRep = RSigmaProp
  }

  abstract class SigmaPropCompanionCtor extends CompanionDef[SigmaPropCompanionCtor] with SigmaPropCompanion {
    def selfType = SigmaPropCompanionElem
    override def toString = "SigmaProp"
  }
  implicit def proxySigmaPropCompanionCtor(p: Rep[SigmaPropCompanionCtor]): SigmaPropCompanionCtor =
    proxyOps[SigmaPropCompanionCtor](p)

  lazy val RSigmaProp: Rep[SigmaPropCompanionCtor] = new SigmaPropCompanionCtor {
    private val thisClass = classOf[SigmaPropCompanion]
  }

  object SigmaPropMethods {
    object isValid {
      def unapply(d: Def[_]): Nullable[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "isValid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "propBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaProp]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaPropElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaPropCompanionMethods {
  }
} // of object SigmaProp
  registerEntityObject("SigmaProp", SigmaProp)

object AnyValue extends EntityObject("AnyValue") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SAnyValue = special.sigma.AnyValue
  case class AnyValueConst(
        constValue: SAnyValue
      ) extends AnyValue with LiftedConst[SAnyValue, AnyValue]
        with Def[AnyValue] with AnyValueConstMethods {
    val liftable: Liftable[SAnyValue, AnyValue] = LiftableAnyValue
    val selfType: Elem[AnyValue] = liftable.eW
  }

  trait AnyValueConstMethods extends AnyValue  { thisConst: Def[_] =>

    private val AnyValueClass = classOf[AnyValue]

    // manual fix
    override def value: Rep[Any] = {
      asRep[Any](mkMethodCall(self,
        AnyValueClass.getMethod("value"),
        List(),
        true, false, AnyElement))
    }

    override def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(self,
        AnyValueClass.getMethod("tVal"),
        List(),
        true, false, element[WRType[Any]]))
    }
  }

  implicit object LiftableAnyValue
    extends Liftable[SAnyValue, AnyValue] {
    lazy val eW: Elem[AnyValue] = anyValueElement
    lazy val sourceType: RType[SAnyValue] = {
      RType[SAnyValue]
    }
    def lift(x: SAnyValue): Rep[AnyValue] = AnyValueConst(x)
    def unlift(w: Rep[AnyValue]): SAnyValue = w match {
      case Def(AnyValueConst(x: SAnyValue))
            => x.asInstanceOf[SAnyValue]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for AnyValue trait
  case class AnyValueAdapter(source: Rep[AnyValue])
      extends AnyValue with Def[AnyValue] {
    val selfType: Elem[AnyValue] = element[AnyValue]
    override def transform(t: Transformer) = AnyValueAdapter(t(source))
    private val thisClass = classOf[AnyValue]

    // manual fix
    def value: Rep[Any] = {
      asRep[Any](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, AnyElement))
    }

    def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(source,
        thisClass.getMethod("tVal"),
        List(),
        true, true, element[WRType[Any]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAnyValue(p: Rep[AnyValue]): AnyValue = {
    if (p.rhs.isInstanceOf[AnyValue@unchecked]) p.rhs.asInstanceOf[AnyValue]
    else
      AnyValueAdapter(p)
  }

  // familyElem
  class AnyValueElem[To <: AnyValue]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableAnyValue.asLiftable[SAnyValue, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[AnyValue], classOf[SAnyValue], Set(
        "value", "tVal"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[AnyValue].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[AnyValue] => convertAnyValue(x) }
      tryConvert(element[AnyValue], this, x, conv)
    }

    def convertAnyValue(x: Rep[AnyValue]): Rep[To] = {
      x.elem match {
        case _: AnyValueElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have AnyValueElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val anyValueElement: Elem[AnyValue] =
    new AnyValueElem[AnyValue]

  implicit case object AnyValueCompanionElem extends CompanionElem[AnyValueCompanionCtor] {
    lazy val tag = weakTypeTag[AnyValueCompanionCtor]
    protected def getDefaultRep = RAnyValue
  }

  abstract class AnyValueCompanionCtor extends CompanionDef[AnyValueCompanionCtor] with AnyValueCompanion {
    def selfType = AnyValueCompanionElem
    override def toString = "AnyValue"
  }
  implicit def proxyAnyValueCompanionCtor(p: Rep[AnyValueCompanionCtor]): AnyValueCompanionCtor =
    proxyOps[AnyValueCompanionCtor](p)

  lazy val RAnyValue: Rep[AnyValueCompanionCtor] = new AnyValueCompanionCtor {
    private val thisClass = classOf[AnyValueCompanion]
  }

  object AnyValueMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AnyValueElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object tVal {
      def unapply(d: Def[_]): Nullable[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AnyValueElem[_]] && method.getName == "tVal" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object AnyValueCompanionMethods {
  }
} // of object AnyValue
  registerEntityObject("AnyValue", AnyValue)

object Box extends EntityObject("Box") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SBox = special.sigma.Box
  case class BoxConst(
        constValue: SBox
      ) extends Box with LiftedConst[SBox, Box]
        with Def[Box] with BoxConstMethods {
    val liftable: Liftable[SBox, Box] = LiftableBox
    val selfType: Elem[Box] = liftable.eW
  }

  trait BoxConstMethods extends Box  { thisConst: Def[_] =>

    private val BoxClass = classOf[Box]

    override def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("id"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def value: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        BoxClass.getMethod("value"),
        List(),
        true, false, element[Long]))
    }

    override def propositionBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("propositionBytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def bytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("bytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def bytesWithoutRef: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("bytesWithoutRef"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def registers: Rep[Coll[AnyValue]] = {
      asRep[Coll[AnyValue]](mkMethodCall(self,
        BoxClass.getMethod("registers"),
        List(),
        true, false, element[Coll[AnyValue]]))
    }

    override def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        BoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        List(i, cT),
        true, false, element[WOption[T]]))
    }

    override def tokens: Rep[Coll[(Coll[Byte], Long)]] = {
      asRep[Coll[(Coll[Byte], Long)]](mkMethodCall(self,
        BoxClass.getMethod("tokens"),
        List(),
        true, false, element[Coll[(Coll[Byte], Long)]]))
    }

    override def creationInfo: Rep[(Int, Coll[Byte])] = {
      asRep[(Int, Coll[Byte])](mkMethodCall(self,
        BoxClass.getMethod("creationInfo"),
        List(),
        true, false, element[(Int, Coll[Byte])]))
    }

    override def executeFromRegister[T](regId: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[T](mkMethodCall(self,
        BoxClass.getMethod("executeFromRegister", classOf[Sym], classOf[Elem[_]]),
        List(regId, cT),
        true, false, element[T]))
    }
  }

  implicit object LiftableBox
    extends Liftable[SBox, Box] {
    lazy val eW: Elem[Box] = boxElement
    lazy val sourceType: RType[SBox] = {
      RType[SBox]
    }
    def lift(x: SBox): Rep[Box] = BoxConst(x)
    def unlift(w: Rep[Box]): SBox = w match {
      case Def(BoxConst(x: SBox))
            => x.asInstanceOf[SBox]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for Box trait
  case class BoxAdapter(source: Rep[Box])
      extends Box with Def[Box] {
    val selfType: Elem[Box] = element[Box]
    override def transform(t: Transformer) = BoxAdapter(t(source))
    private val thisClass = classOf[Box]

    def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("id"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def value: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Long]))
    }

    def propositionBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("propositionBytes"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def bytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("bytes"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def bytesWithoutRef: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("bytesWithoutRef"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def registers: Rep[Coll[AnyValue]] = {
      asRep[Coll[AnyValue]](mkMethodCall(source,
        thisClass.getMethod("registers"),
        List(),
        true, true, element[Coll[AnyValue]]))
    }

    def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        thisClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        List(i, cT),
        true, true, element[WOption[T]]))
    }

    def tokens: Rep[Coll[(Coll[Byte], Long)]] = {
      asRep[Coll[(Coll[Byte], Long)]](mkMethodCall(source,
        thisClass.getMethod("tokens"),
        List(),
        true, true, element[Coll[(Coll[Byte], Long)]]))
    }

    def creationInfo: Rep[(Int, Coll[Byte])] = {
      asRep[(Int, Coll[Byte])](mkMethodCall(source,
        thisClass.getMethod("creationInfo"),
        List(),
        true, true, element[(Int, Coll[Byte])]))
    }

    def executeFromRegister[T](regId: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[T](mkMethodCall(source,
        thisClass.getMethod("executeFromRegister", classOf[Sym], classOf[Elem[_]]),
        List(regId, cT),
        true, true, element[T]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBox(p: Rep[Box]): Box = {
    if (p.rhs.isInstanceOf[Box@unchecked]) p.rhs.asInstanceOf[Box]
    else
      BoxAdapter(p)
  }

  // familyElem
  class BoxElem[To <: Box]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableBox.asLiftable[SBox, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Box], classOf[SBox], Set(
        "id", "value", "propositionBytes", "bytes", "bytesWithoutRef", "registers", "getReg", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "tokens", "creationInfo", "executeFromRegister"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Box].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Box] => convertBox(x) }
      tryConvert(element[Box], this, x, conv)
    }

    def convertBox(x: Rep[Box]): Rep[To] = {
      x.elem match {
        case _: BoxElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have BoxElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val boxElement: Elem[Box] =
    new BoxElem[Box]

  implicit case object BoxCompanionElem extends CompanionElem[BoxCompanionCtor] {
    lazy val tag = weakTypeTag[BoxCompanionCtor]
    protected def getDefaultRep = RBox
  }

  abstract class BoxCompanionCtor extends CompanionDef[BoxCompanionCtor] with BoxCompanion {
    def selfType = BoxCompanionElem
    override def toString = "Box"
  }
  implicit def proxyBoxCompanionCtor(p: Rep[BoxCompanionCtor]): BoxCompanionCtor =
    proxyOps[BoxCompanionCtor](p)

  lazy val RBox: Rep[BoxCompanionCtor] = new BoxCompanionCtor {
    private val thisClass = classOf[BoxCompanion]
  }

  object BoxMethods {
    object id {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "id" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "propositionBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "bytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "bytesWithoutRef" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "registers" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "getReg" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R0 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R0" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R1 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R1" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R2 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R2" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R3 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R3" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R4 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R4" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R5 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R5" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R6 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R6" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R7 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R7" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R8 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R8" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object R9 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "R9" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object tokens {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "tokens" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object creationInfo {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "creationInfo" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object executeFromRegister {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[BoxElem[_]] && method.getName == "executeFromRegister" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object BoxCompanionMethods {
  }
} // of object Box
  registerEntityObject("Box", Box)

object AvlTree extends EntityObject("AvlTree") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SAvlTree = special.sigma.AvlTree
  case class AvlTreeConst(
        constValue: SAvlTree
      ) extends AvlTree with LiftedConst[SAvlTree, AvlTree]
        with Def[AvlTree] with AvlTreeConstMethods {
    val liftable: Liftable[SAvlTree, AvlTree] = LiftableAvlTree
    val selfType: Elem[AvlTree] = liftable.eW
  }

  trait AvlTreeConstMethods extends AvlTree  { thisConst: Def[_] =>

    private val AvlTreeClass = classOf[AvlTree]

    override def digest: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        AvlTreeClass.getMethod("digest"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def enabledOperations: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        AvlTreeClass.getMethod("enabledOperations"),
        List(),
        true, false, element[Byte]))
    }

    override def keyLength: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        AvlTreeClass.getMethod("keyLength"),
        List(),
        true, false, element[Int]))
    }

    override def valueLengthOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(self,
        AvlTreeClass.getMethod("valueLengthOpt"),
        List(),
        true, false, element[WOption[Int]]))
    }

    override def isInsertAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("isInsertAllowed"),
        List(),
        true, false, element[Boolean]))
    }

    override def isUpdateAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("isUpdateAllowed"),
        List(),
        true, false, element[Boolean]))
    }

    override def isRemoveAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("isRemoveAllowed"),
        List(),
        true, false, element[Boolean]))
    }

    override def updateDigest(newDigest: Rep[Coll[Byte]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        AvlTreeClass.getMethod("updateDigest", classOf[Sym]),
        List(newDigest),
        true, false, element[AvlTree]))
    }

    override def updateOperations(newOperations: Rep[Byte]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        AvlTreeClass.getMethod("updateOperations", classOf[Sym]),
        List(newOperations),
        true, false, element[AvlTree]))
    }

    override def contains(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("contains", classOf[Sym], classOf[Sym]),
        List(key, proof),
        true, false, element[Boolean]))
    }

    override def get(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(self,
        AvlTreeClass.getMethod("get", classOf[Sym], classOf[Sym]),
        List(key, proof),
        true, false, element[WOption[Coll[Byte]]]))
    }

    override def getMany(keys: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[Coll[WOption[Coll[Byte]]]] = {
      asRep[Coll[WOption[Coll[Byte]]]](mkMethodCall(self,
        AvlTreeClass.getMethod("getMany", classOf[Sym], classOf[Sym]),
        List(keys, proof),
        true, false, element[Coll[WOption[Coll[Byte]]]]))
    }

    override def insert(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(self,
        AvlTreeClass.getMethod("insert", classOf[Sym], classOf[Sym]),
        List(operations, proof),
        true, false, element[WOption[AvlTree]]))
    }

    override def update(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(self,
        AvlTreeClass.getMethod("update", classOf[Sym], classOf[Sym]),
        List(operations, proof),
        true, false, element[WOption[AvlTree]]))
    }

    override def remove(operations: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(self,
        AvlTreeClass.getMethod("remove", classOf[Sym], classOf[Sym]),
        List(operations, proof),
        true, false, element[WOption[AvlTree]]))
    }
  }

  implicit object LiftableAvlTree
    extends Liftable[SAvlTree, AvlTree] {
    lazy val eW: Elem[AvlTree] = avlTreeElement
    lazy val sourceType: RType[SAvlTree] = {
      RType[SAvlTree]
    }
    def lift(x: SAvlTree): Rep[AvlTree] = AvlTreeConst(x)
    def unlift(w: Rep[AvlTree]): SAvlTree = w match {
      case Def(AvlTreeConst(x: SAvlTree))
            => x.asInstanceOf[SAvlTree]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for AvlTree trait
  case class AvlTreeAdapter(source: Rep[AvlTree])
      extends AvlTree with Def[AvlTree] {
    val selfType: Elem[AvlTree] = element[AvlTree]
    override def transform(t: Transformer) = AvlTreeAdapter(t(source))
    private val thisClass = classOf[AvlTree]

    def digest: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("digest"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def enabledOperations: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        thisClass.getMethod("enabledOperations"),
        List(),
        true, true, element[Byte]))
    }

    def keyLength: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("keyLength"),
        List(),
        true, true, element[Int]))
    }

    def valueLengthOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        thisClass.getMethod("valueLengthOpt"),
        List(),
        true, true, element[WOption[Int]]))
    }

    def isInsertAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isInsertAllowed"),
        List(),
        true, true, element[Boolean]))
    }

    def isUpdateAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isUpdateAllowed"),
        List(),
        true, true, element[Boolean]))
    }

    def isRemoveAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isRemoveAllowed"),
        List(),
        true, true, element[Boolean]))
    }

    def updateDigest(newDigest: Rep[Coll[Byte]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("updateDigest", classOf[Sym]),
        List(newDigest),
        true, true, element[AvlTree]))
    }

    def updateOperations(newOperations: Rep[Byte]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("updateOperations", classOf[Sym]),
        List(newOperations),
        true, true, element[AvlTree]))
    }

    def contains(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("contains", classOf[Sym], classOf[Sym]),
        List(key, proof),
        true, true, element[Boolean]))
    }

    def get(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(source,
        thisClass.getMethod("get", classOf[Sym], classOf[Sym]),
        List(key, proof),
        true, true, element[WOption[Coll[Byte]]]))
    }

    def getMany(keys: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[Coll[WOption[Coll[Byte]]]] = {
      asRep[Coll[WOption[Coll[Byte]]]](mkMethodCall(source,
        thisClass.getMethod("getMany", classOf[Sym], classOf[Sym]),
        List(keys, proof),
        true, true, element[Coll[WOption[Coll[Byte]]]]))
    }

    def insert(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(source,
        thisClass.getMethod("insert", classOf[Sym], classOf[Sym]),
        List(operations, proof),
        true, true, element[WOption[AvlTree]]))
    }

    def update(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(source,
        thisClass.getMethod("update", classOf[Sym], classOf[Sym]),
        List(operations, proof),
        true, true, element[WOption[AvlTree]]))
    }

    def remove(operations: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(source,
        thisClass.getMethod("remove", classOf[Sym], classOf[Sym]),
        List(operations, proof),
        true, true, element[WOption[AvlTree]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAvlTree(p: Rep[AvlTree]): AvlTree = {
    if (p.rhs.isInstanceOf[AvlTree@unchecked]) p.rhs.asInstanceOf[AvlTree]
    else
      AvlTreeAdapter(p)
  }

  // familyElem
  class AvlTreeElem[To <: AvlTree]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableAvlTree.asLiftable[SAvlTree, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[AvlTree], classOf[SAvlTree], Set(
        "digest", "enabledOperations", "keyLength", "valueLengthOpt", "isInsertAllowed", "isUpdateAllowed", "isRemoveAllowed", "updateDigest", "updateOperations", "contains", "get", "getMany", "insert", "update", "remove"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[AvlTree].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[AvlTree] => convertAvlTree(x) }
      tryConvert(element[AvlTree], this, x, conv)
    }

    def convertAvlTree(x: Rep[AvlTree]): Rep[To] = {
      x.elem match {
        case _: AvlTreeElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have AvlTreeElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val avlTreeElement: Elem[AvlTree] =
    new AvlTreeElem[AvlTree]

  implicit case object AvlTreeCompanionElem extends CompanionElem[AvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[AvlTreeCompanionCtor]
    protected def getDefaultRep = RAvlTree
  }

  abstract class AvlTreeCompanionCtor extends CompanionDef[AvlTreeCompanionCtor] with AvlTreeCompanion {
    def selfType = AvlTreeCompanionElem
    override def toString = "AvlTree"
  }
  implicit def proxyAvlTreeCompanionCtor(p: Rep[AvlTreeCompanionCtor]): AvlTreeCompanionCtor =
    proxyOps[AvlTreeCompanionCtor](p)

  lazy val RAvlTree: Rep[AvlTreeCompanionCtor] = new AvlTreeCompanionCtor {
    private val thisClass = classOf[AvlTreeCompanion]
  }

  object AvlTreeMethods {
    object digest {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "digest" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object enabledOperations {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "enabledOperations" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object keyLength {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "keyLength" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueLengthOpt {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "valueLengthOpt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isInsertAllowed {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "isInsertAllowed" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isUpdateAllowed {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "isUpdateAllowed" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isRemoveAllowed {
      def unapply(d: Def[_]): Nullable[Rep[AvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "isRemoveAllowed" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updateDigest {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "updateDigest" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updateOperations {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Byte])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "updateOperations" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Byte])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Byte])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object contains {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "contains" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object get {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "get" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getMany {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[Coll[Byte]]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "getMany" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[Coll[Byte]]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[Coll[Byte]]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object insert {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[(Coll[Byte], Coll[Byte])]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "insert" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[(Coll[Byte], Coll[Byte])]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[(Coll[Byte], Coll[Byte])]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object update {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[(Coll[Byte], Coll[Byte])]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "update" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[(Coll[Byte], Coll[Byte])]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[(Coll[Byte], Coll[Byte])]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object remove {
      def unapply(d: Def[_]): Nullable[(Rep[AvlTree], Rep[Coll[Coll[Byte]]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AvlTreeElem[_]] && method.getName == "remove" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[AvlTree], Rep[Coll[Coll[Byte]]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AvlTree], Rep[Coll[Coll[Byte]]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object AvlTreeCompanionMethods {
  }
} // of object AvlTree
  registerEntityObject("AvlTree", AvlTree)

object PreHeader extends EntityObject("PreHeader") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SPreHeader = special.sigma.PreHeader
  case class PreHeaderConst(
        constValue: SPreHeader
      ) extends PreHeader with LiftedConst[SPreHeader, PreHeader]
        with Def[PreHeader] with PreHeaderConstMethods {
    val liftable: Liftable[SPreHeader, PreHeader] = LiftablePreHeader
    val selfType: Elem[PreHeader] = liftable.eW
  }

  trait PreHeaderConstMethods extends PreHeader  { thisConst: Def[_] =>

    private val PreHeaderClass = classOf[PreHeader]

    override def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        PreHeaderClass.getMethod("version"),
        List(),
        true, false, element[Byte]))
    }

    override def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        PreHeaderClass.getMethod("parentId"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        PreHeaderClass.getMethod("timestamp"),
        List(),
        true, false, element[Long]))
    }

    override def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        PreHeaderClass.getMethod("nBits"),
        List(),
        true, false, element[Long]))
    }

    override def height: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        PreHeaderClass.getMethod("height"),
        List(),
        true, false, element[Int]))
    }

    override def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        PreHeaderClass.getMethod("minerPk"),
        List(),
        true, false, element[GroupElement]))
    }

    override def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        PreHeaderClass.getMethod("votes"),
        List(),
        true, false, element[Coll[Byte]]))
    }
  }

  implicit object LiftablePreHeader
    extends Liftable[SPreHeader, PreHeader] {
    lazy val eW: Elem[PreHeader] = preHeaderElement
    lazy val sourceType: RType[SPreHeader] = {
      RType[SPreHeader]
    }
    def lift(x: SPreHeader): Rep[PreHeader] = PreHeaderConst(x)
    def unlift(w: Rep[PreHeader]): SPreHeader = w match {
      case Def(PreHeaderConst(x: SPreHeader))
            => x.asInstanceOf[SPreHeader]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for PreHeader trait
  case class PreHeaderAdapter(source: Rep[PreHeader])
      extends PreHeader with Def[PreHeader] {
    val selfType: Elem[PreHeader] = element[PreHeader]
    override def transform(t: Transformer) = PreHeaderAdapter(t(source))
    private val thisClass = classOf[PreHeader]

    def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        thisClass.getMethod("version"),
        List(),
        true, true, element[Byte]))
    }

    def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("parentId"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("timestamp"),
        List(),
        true, true, element[Long]))
    }

    def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("nBits"),
        List(),
        true, true, element[Long]))
    }

    def height: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("height"),
        List(),
        true, true, element[Int]))
    }

    def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("minerPk"),
        List(),
        true, true, element[GroupElement]))
    }

    def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("votes"),
        List(),
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyPreHeader(p: Rep[PreHeader]): PreHeader = {
    if (p.rhs.isInstanceOf[PreHeader@unchecked]) p.rhs.asInstanceOf[PreHeader]
    else
      PreHeaderAdapter(p)
  }

  // familyElem
  class PreHeaderElem[To <: PreHeader]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftablePreHeader.asLiftable[SPreHeader, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[PreHeader], classOf[SPreHeader], Set(
        "version", "parentId", "timestamp", "nBits", "height", "minerPk", "votes"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[PreHeader].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[PreHeader] => convertPreHeader(x) }
      tryConvert(element[PreHeader], this, x, conv)
    }

    def convertPreHeader(x: Rep[PreHeader]): Rep[To] = {
      x.elem match {
        case _: PreHeaderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have PreHeaderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val preHeaderElement: Elem[PreHeader] =
    new PreHeaderElem[PreHeader]

  implicit case object PreHeaderCompanionElem extends CompanionElem[PreHeaderCompanionCtor] {
    lazy val tag = weakTypeTag[PreHeaderCompanionCtor]
    protected def getDefaultRep = RPreHeader
  }

  abstract class PreHeaderCompanionCtor extends CompanionDef[PreHeaderCompanionCtor] with PreHeaderCompanion {
    def selfType = PreHeaderCompanionElem
    override def toString = "PreHeader"
  }
  implicit def proxyPreHeaderCompanionCtor(p: Rep[PreHeaderCompanionCtor]): PreHeaderCompanionCtor =
    proxyOps[PreHeaderCompanionCtor](p)

  lazy val RPreHeader: Rep[PreHeaderCompanionCtor] = new PreHeaderCompanionCtor {
    private val thisClass = classOf[PreHeaderCompanion]
  }

  object PreHeaderMethods {
    object version {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "version" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object parentId {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "parentId" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object timestamp {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "timestamp" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object nBits {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "nBits" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object height {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "height" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object minerPk {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "minerPk" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object votes {
      def unapply(d: Def[_]): Nullable[Rep[PreHeader]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PreHeaderElem[_]] && method.getName == "votes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PreHeader]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PreHeader]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object PreHeaderCompanionMethods {
  }
} // of object PreHeader
  registerEntityObject("PreHeader", PreHeader)

object Header extends EntityObject("Header") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SHeader = special.sigma.Header
  case class HeaderConst(
        constValue: SHeader
      ) extends Header with LiftedConst[SHeader, Header]
        with Def[Header] with HeaderConstMethods {
    val liftable: Liftable[SHeader, Header] = LiftableHeader
    val selfType: Elem[Header] = liftable.eW
  }

  trait HeaderConstMethods extends Header  { thisConst: Def[_] =>

    private val HeaderClass = classOf[Header]

    override def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("id"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        HeaderClass.getMethod("version"),
        List(),
        true, false, element[Byte]))
    }

    override def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("parentId"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def ADProofsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("ADProofsRoot"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def stateRoot: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        HeaderClass.getMethod("stateRoot"),
        List(),
        true, false, element[AvlTree]))
    }

    override def transactionsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("transactionsRoot"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        HeaderClass.getMethod("timestamp"),
        List(),
        true, false, element[Long]))
    }

    override def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        HeaderClass.getMethod("nBits"),
        List(),
        true, false, element[Long]))
    }

    override def height: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        HeaderClass.getMethod("height"),
        List(),
        true, false, element[Int]))
    }

    override def extensionRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("extensionRoot"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        HeaderClass.getMethod("minerPk"),
        List(),
        true, false, element[GroupElement]))
    }

    override def powOnetimePk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        HeaderClass.getMethod("powOnetimePk"),
        List(),
        true, false, element[GroupElement]))
    }

    override def powNonce: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("powNonce"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def powDistance: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        HeaderClass.getMethod("powDistance"),
        List(),
        true, false, element[BigInt]))
    }

    override def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("votes"),
        List(),
        true, false, element[Coll[Byte]]))
    }
  }

  implicit object LiftableHeader
    extends Liftable[SHeader, Header] {
    lazy val eW: Elem[Header] = headerElement
    lazy val sourceType: RType[SHeader] = {
      RType[SHeader]
    }
    def lift(x: SHeader): Rep[Header] = HeaderConst(x)
    def unlift(w: Rep[Header]): SHeader = w match {
      case Def(HeaderConst(x: SHeader))
            => x.asInstanceOf[SHeader]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for Header trait
  case class HeaderAdapter(source: Rep[Header])
      extends Header with Def[Header] {
    val selfType: Elem[Header] = element[Header]
    override def transform(t: Transformer) = HeaderAdapter(t(source))
    private val thisClass = classOf[Header]

    def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("id"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        thisClass.getMethod("version"),
        List(),
        true, true, element[Byte]))
    }

    def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("parentId"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def ADProofsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("ADProofsRoot"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def stateRoot: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("stateRoot"),
        List(),
        true, true, element[AvlTree]))
    }

    def transactionsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("transactionsRoot"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("timestamp"),
        List(),
        true, true, element[Long]))
    }

    def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("nBits"),
        List(),
        true, true, element[Long]))
    }

    def height: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("height"),
        List(),
        true, true, element[Int]))
    }

    def extensionRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("extensionRoot"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("minerPk"),
        List(),
        true, true, element[GroupElement]))
    }

    def powOnetimePk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("powOnetimePk"),
        List(),
        true, true, element[GroupElement]))
    }

    def powNonce: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("powNonce"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def powDistance: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("powDistance"),
        List(),
        true, true, element[BigInt]))
    }

    def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("votes"),
        List(),
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyHeader(p: Rep[Header]): Header = {
    if (p.rhs.isInstanceOf[Header@unchecked]) p.rhs.asInstanceOf[Header]
    else
      HeaderAdapter(p)
  }

  // familyElem
  class HeaderElem[To <: Header]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableHeader.asLiftable[SHeader, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Header], classOf[SHeader], Set(
        "id", "version", "parentId", "ADProofsRoot", "stateRoot", "transactionsRoot", "timestamp", "nBits", "height", "extensionRoot", "minerPk", "powOnetimePk", "powNonce", "powDistance", "votes"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Header].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Header] => convertHeader(x) }
      tryConvert(element[Header], this, x, conv)
    }

    def convertHeader(x: Rep[Header]): Rep[To] = {
      x.elem match {
        case _: HeaderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have HeaderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val headerElement: Elem[Header] =
    new HeaderElem[Header]

  implicit case object HeaderCompanionElem extends CompanionElem[HeaderCompanionCtor] {
    lazy val tag = weakTypeTag[HeaderCompanionCtor]
    protected def getDefaultRep = RHeader
  }

  abstract class HeaderCompanionCtor extends CompanionDef[HeaderCompanionCtor] with HeaderCompanion {
    def selfType = HeaderCompanionElem
    override def toString = "Header"
  }
  implicit def proxyHeaderCompanionCtor(p: Rep[HeaderCompanionCtor]): HeaderCompanionCtor =
    proxyOps[HeaderCompanionCtor](p)

  lazy val RHeader: Rep[HeaderCompanionCtor] = new HeaderCompanionCtor {
    private val thisClass = classOf[HeaderCompanion]
  }

  object HeaderMethods {
    object id {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "id" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object version {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "version" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object parentId {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "parentId" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object ADProofsRoot {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "ADProofsRoot" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object stateRoot {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "stateRoot" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object transactionsRoot {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "transactionsRoot" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object timestamp {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "timestamp" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object nBits {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "nBits" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object height {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "height" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object extensionRoot {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "extensionRoot" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object minerPk {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "minerPk" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object powOnetimePk {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "powOnetimePk" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object powNonce {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "powNonce" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object powDistance {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "powDistance" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object votes {
      def unapply(d: Def[_]): Nullable[Rep[Header]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[HeaderElem[_]] && method.getName == "votes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Header]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Header]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object HeaderCompanionMethods {
  }
} // of object Header
  registerEntityObject("Header", Header)

object Context extends EntityObject("Context") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SContext = special.sigma.Context
  case class ContextConst(
        constValue: SContext
      ) extends Context with LiftedConst[SContext, Context]
        with Def[Context] with ContextConstMethods {
    val liftable: Liftable[SContext, Context] = LiftableContext
    val selfType: Elem[Context] = liftable.eW
  }

  trait ContextConstMethods extends Context  { thisConst: Def[_] =>

    private val ContextClass = classOf[Context]

    override def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        ContextClass.getMethod("builder"),
        List(),
        true, false, element[SigmaDslBuilder]))
    }

    override def OUTPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        ContextClass.getMethod("OUTPUTS"),
        List(),
        true, false, element[Coll[Box]]))
    }

    override def INPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        ContextClass.getMethod("INPUTS"),
        List(),
        true, false, element[Coll[Box]]))
    }

    override def dataInputs: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        ContextClass.getMethod("dataInputs"),
        List(),
        true, false, element[Coll[Box]]))
    }

    override def HEIGHT: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        ContextClass.getMethod("HEIGHT"),
        List(),
        true, false, element[Int]))
    }

    override def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(self,
        ContextClass.getMethod("SELF"),
        List(),
        true, false, element[Box]))
    }

    override def selfBoxIndex: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        ContextClass.getMethod("selfBoxIndex"),
        List(),
        true, false, element[Int]))
    }

    override def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        ContextClass.getMethod("LastBlockUtxoRootHash"),
        List(),
        true, false, element[AvlTree]))
    }

    override def headers: Rep[Coll[Header]] = {
      asRep[Coll[Header]](mkMethodCall(self,
        ContextClass.getMethod("headers"),
        List(),
        true, false, element[Coll[Header]]))
    }

    override def preHeader: Rep[PreHeader] = {
      asRep[PreHeader](mkMethodCall(self,
        ContextClass.getMethod("preHeader"),
        List(),
        true, false, element[PreHeader]))
    }

    override def minerPubKey: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        ContextClass.getMethod("minerPubKey"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        ContextClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, element[WOption[T]]))
    }
  }

  implicit object LiftableContext
    extends Liftable[SContext, Context] {
    lazy val eW: Elem[Context] = contextElement
    lazy val sourceType: RType[SContext] = {
      RType[SContext]
    }
    def lift(x: SContext): Rep[Context] = ContextConst(x)
    def unlift(w: Rep[Context]): SContext = w match {
      case Def(ContextConst(x: SContext))
            => x.asInstanceOf[SContext]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for Context trait
  case class ContextAdapter(source: Rep[Context])
      extends Context with Def[Context] {
    val selfType: Elem[Context] = element[Context]
    override def transform(t: Transformer) = ContextAdapter(t(source))
    private val thisClass = classOf[Context]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    def OUTPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(source,
        thisClass.getMethod("OUTPUTS"),
        List(),
        true, true, element[Coll[Box]]))
    }

    def INPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(source,
        thisClass.getMethod("INPUTS"),
        List(),
        true, true, element[Coll[Box]]))
    }

    def dataInputs: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(source,
        thisClass.getMethod("dataInputs"),
        List(),
        true, true, element[Coll[Box]]))
    }

    def HEIGHT: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("HEIGHT"),
        List(),
        true, true, element[Int]))
    }

    def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(source,
        thisClass.getMethod("SELF"),
        List(),
        true, true, element[Box]))
    }

    def selfBoxIndex: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("selfBoxIndex"),
        List(),
        true, true, element[Int]))
    }

    def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("LastBlockUtxoRootHash"),
        List(),
        true, true, element[AvlTree]))
    }

    def headers: Rep[Coll[Header]] = {
      asRep[Coll[Header]](mkMethodCall(source,
        thisClass.getMethod("headers"),
        List(),
        true, true, element[Coll[Header]]))
    }

    def preHeader: Rep[PreHeader] = {
      asRep[PreHeader](mkMethodCall(source,
        thisClass.getMethod("preHeader"),
        List(),
        true, true, element[PreHeader]))
    }

    def minerPubKey: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("minerPubKey"),
        List(),
        true, true, element[Coll[Byte]]))
    }

    def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        thisClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, true, element[WOption[T]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyContext(p: Rep[Context]): Context = {
    if (p.rhs.isInstanceOf[Context@unchecked]) p.rhs.asInstanceOf[Context]
    else
      ContextAdapter(p)
  }

  // familyElem
  class ContextElem[To <: Context]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableContext.asLiftable[SContext, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Context], classOf[SContext], Set(
        "builder", "OUTPUTS", "INPUTS", "dataInputs", "HEIGHT", "SELF", "selfBoxIndex", "LastBlockUtxoRootHash", "headers", "preHeader", "minerPubKey", "getVar"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[Context].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Context] => convertContext(x) }
      tryConvert(element[Context], this, x, conv)
    }

    def convertContext(x: Rep[Context]): Rep[To] = {
      x.elem match {
        case _: ContextElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have ContextElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val contextElement: Elem[Context] =
    new ContextElem[Context]

  implicit case object ContextCompanionElem extends CompanionElem[ContextCompanionCtor] {
    lazy val tag = weakTypeTag[ContextCompanionCtor]
    protected def getDefaultRep = RContext
  }

  abstract class ContextCompanionCtor extends CompanionDef[ContextCompanionCtor] with ContextCompanion {
    def selfType = ContextCompanionElem
    override def toString = "Context"
  }
  implicit def proxyContextCompanionCtor(p: Rep[ContextCompanionCtor]): ContextCompanionCtor =
    proxyOps[ContextCompanionCtor](p)

  lazy val RContext: Rep[ContextCompanionCtor] = new ContextCompanionCtor {
    private val thisClass = classOf[ContextCompanion]
  }

  object ContextMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "OUTPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "INPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataInputs {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "dataInputs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "HEIGHT" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "SELF" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object selfBoxIndex {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "selfBoxIndex" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "LastBlockUtxoRootHash" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object headers {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "headers" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object preHeader {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "preHeader" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object minerPubKey {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "minerPubKey" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ContextElem[_]] && method.getName == "getVar" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ContextCompanionMethods {
  }
} // of object Context
  registerEntityObject("Context", Context)

object SigmaContract extends EntityObject("SigmaContract") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaContract = special.sigma.SigmaContract
  case class SigmaContractConst(
        constValue: SSigmaContract
      ) extends SigmaContract with LiftedConst[SSigmaContract, SigmaContract]
        with Def[SigmaContract] with SigmaContractConstMethods {
    val liftable: Liftable[SSigmaContract, SigmaContract] = LiftableSigmaContract
    val selfType: Elem[SigmaContract] = liftable.eW
  }

  trait SigmaContractConstMethods extends SigmaContract  { thisConst: Def[_] =>

    private val SigmaContractClass = classOf[SigmaContract]

    override def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        SigmaContractClass.getMethod("builder"),
        List(),
        true, false, element[SigmaDslBuilder]))
    }

    override def Collection[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        SigmaContractClass.getMethod("Collection", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, false, element[Coll[T]]))
    }

    override def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaContractClass.getMethod("canOpen", classOf[Sym]),
        List(ctx),
        true, false, element[Boolean]))
    }
  }

  implicit object LiftableSigmaContract
    extends Liftable[SSigmaContract, SigmaContract] {
    lazy val eW: Elem[SigmaContract] = sigmaContractElement
    lazy val sourceType: RType[SSigmaContract] = {
      RType[SSigmaContract]
    }
    def lift(x: SSigmaContract): Rep[SigmaContract] = SigmaContractConst(x)
    def unlift(w: Rep[SigmaContract]): SSigmaContract = w match {
      case Def(SigmaContractConst(x: SSigmaContract))
            => x.asInstanceOf[SSigmaContract]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for SigmaContract trait
  case class SigmaContractAdapter(source: Rep[SigmaContract])
      extends SigmaContract with Def[SigmaContract] {
    val selfType: Elem[SigmaContract] = element[SigmaContract]
    override def transform(t: Transformer) = SigmaContractAdapter(t(source))
    private val thisClass = classOf[SigmaContract]

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[SigmaDslBuilder]))
    }

    override def Collection[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        thisClass.getMethod("Collection", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, true, element[Coll[T]]))
    }

    def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("canOpen", classOf[Sym]),
        List(ctx),
        true, true, element[Boolean]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaContract(p: Rep[SigmaContract]): SigmaContract = {
    if (p.rhs.isInstanceOf[SigmaContract@unchecked]) p.rhs.asInstanceOf[SigmaContract]
    else
      SigmaContractAdapter(p)
  }

  // familyElem
  class SigmaContractElem[To <: SigmaContract]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSigmaContract.asLiftable[SSigmaContract, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaContract], classOf[SSigmaContract], Set(
        "builder", "Collection", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "xorOf", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "byteArrayToLong", "proveDlog", "proveDHTuple", "groupGenerator", "canOpen", "asFunction"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaContract].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaContract] => convertSigmaContract(x) }
      tryConvert(element[SigmaContract], this, x, conv)
    }

    def convertSigmaContract(x: Rep[SigmaContract]): Rep[To] = {
      x.elem match {
        case _: SigmaContractElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaContractElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sigmaContractElement: Elem[SigmaContract] =
    new SigmaContractElem[SigmaContract]

  implicit case object SigmaContractCompanionElem extends CompanionElem[SigmaContractCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaContractCompanionCtor]
    protected def getDefaultRep = RSigmaContract
  }

  abstract class SigmaContractCompanionCtor extends CompanionDef[SigmaContractCompanionCtor] with SigmaContractCompanion {
    def selfType = SigmaContractCompanionElem
    override def toString = "SigmaContract"
  }
  implicit def proxySigmaContractCompanionCtor(p: Rep[SigmaContractCompanionCtor]): SigmaContractCompanionCtor =
    proxyOps[SigmaContractCompanionCtor](p)

  lazy val RSigmaContract: Rep[SigmaContractCompanionCtor] = new SigmaContractCompanionCtor {
    private val thisClass = classOf[SigmaContractCompanion]
  }

  object SigmaContractMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Collection {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "Collection" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "verifyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Int], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "atLeast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Int], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Int], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "allZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "anyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xorOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "xorOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "PubKey" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "sigmaProp" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "blake2b256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "sha256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "byteArrayToBigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "longToByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToLong {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "byteArrayToLong" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "proveDlog" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[GroupElement])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "proveDHTuple" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Nullable[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "groupGenerator" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object canOpen {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaContract], Rep[Context])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "canOpen" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaContract], Rep[Context])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaContract], Rep[Context])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object asFunction {
      def unapply(d: Def[_]): Nullable[Rep[SigmaContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaContractElem[_]] && method.getName == "asFunction" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaContractCompanionMethods {
  }
} // of object SigmaContract
  registerEntityObject("SigmaContract", SigmaContract)

object SigmaDslBuilder extends EntityObject("SigmaDslBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSigmaDslBuilder = special.sigma.SigmaDslBuilder
  case class SigmaDslBuilderConst(
        constValue: SSigmaDslBuilder
      ) extends SigmaDslBuilder with LiftedConst[SSigmaDslBuilder, SigmaDslBuilder]
        with Def[SigmaDslBuilder] with SigmaDslBuilderConstMethods {
    val liftable: Liftable[SSigmaDslBuilder, SigmaDslBuilder] = LiftableSigmaDslBuilder
    val selfType: Elem[SigmaDslBuilder] = liftable.eW
  }

  trait SigmaDslBuilderConstMethods extends SigmaDslBuilder  { thisConst: Def[_] =>

    private val SigmaDslBuilderClass = classOf[SigmaDslBuilder]

    override def Colls: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Colls"),
        List(),
        true, false, element[CollBuilder]))
    }

    override def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Monoids"),
        List(),
        true, false, element[MonoidBuilder]))
    }

    override def Costing: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Costing"),
        List(),
        true, false, element[CostedBuilder]))
    }

    override def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("CostModel"),
        List(),
        true, false, element[CostModel]))
    }

    override def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("verifyZK", classOf[Sym]),
        List(cond),
        true, false, element[Boolean]))
    }

    override def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        List(bound, props),
        true, false, element[SigmaProp]))
    }

    override def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    override def allZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        List(conditions),
        true, false, element[SigmaProp]))
    }

    override def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    override def anyZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        List(conditions),
        true, false, element[SigmaProp]))
    }

    override def xorOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    override def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("PubKey", classOf[Sym]),
        List(base64String),
        true, false, element[SigmaProp]))
    }

    override def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        List(b),
        true, false, element[SigmaProp]))
    }

    override def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        List(bytes),
        true, false, element[Coll[Byte]]))
    }

    override def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        List(bytes),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        List(bytes),
        true, false, element[BigInt]))
    }

    override def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        List(l),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToLong(bytes: Rep[Coll[Byte]]): Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        List(bytes),
        true, false, element[Long]))
    }

    override def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        List(g),
        true, false, element[SigmaProp]))
    }

    override def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(g, h, u, v),
        true, false, element[SigmaProp]))
    }

    override def groupGenerator: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        List(),
        true, false, element[GroupElement]))
    }

    override def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        List(scriptBytes, positions, newValues, cT),
        true, false, element[Coll[Byte]]))
    }

    override def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        List(encoded),
        true, false, element[GroupElement]))
    }

    override def BigInt(n: Rep[WBigInteger]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("BigInt", classOf[Sym]),
        List(n),
        true, false, element[BigInt]))
    }

    override def toBigInteger(n: Rep[BigInt]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("toBigInteger", classOf[Sym]),
        List(n),
        true, false, element[WBigInteger]))
    }

    override def avlTree(operationFlags: Rep[Byte], digest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(operationFlags, digest, keyLength, valueLengthOpt),
        true, false, element[AvlTree]))
    }
  }

  implicit object LiftableSigmaDslBuilder
    extends Liftable[SSigmaDslBuilder, SigmaDslBuilder] {
    lazy val eW: Elem[SigmaDslBuilder] = sigmaDslBuilderElement
    lazy val sourceType: RType[SSigmaDslBuilder] = {
      RType[SSigmaDslBuilder]
    }
    def lift(x: SSigmaDslBuilder): Rep[SigmaDslBuilder] = SigmaDslBuilderConst(x)
    def unlift(w: Rep[SigmaDslBuilder]): SSigmaDslBuilder = w match {
      case Def(SigmaDslBuilderConst(x: SSigmaDslBuilder))
            => x.asInstanceOf[SSigmaDslBuilder]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for SigmaDslBuilder trait
  case class SigmaDslBuilderAdapter(source: Rep[SigmaDslBuilder])
      extends SigmaDslBuilder with Def[SigmaDslBuilder] {
    val selfType: Elem[SigmaDslBuilder] = element[SigmaDslBuilder]
    override def transform(t: Transformer) = SigmaDslBuilderAdapter(t(source))
    private val thisClass = classOf[SigmaDslBuilder]

    def Colls: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        thisClass.getMethod("Colls"),
        List(),
        true, true, element[CollBuilder]))
    }

    def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        thisClass.getMethod("Monoids"),
        List(),
        true, true, element[MonoidBuilder]))
    }

    def Costing: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("Costing"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(source,
        thisClass.getMethod("CostModel"),
        List(),
        true, true, element[CostModel]))
    }

    def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("verifyZK", classOf[Sym]),
        List(cond),
        true, true, element[Boolean]))
    }

    def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        List(bound, props),
        true, true, element[SigmaProp]))
    }

    def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("allOf", classOf[Sym]),
        List(conditions),
        true, true, element[Boolean]))
    }

    def allZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("allZK", classOf[Sym]),
        List(conditions),
        true, true, element[SigmaProp]))
    }

    def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("anyOf", classOf[Sym]),
        List(conditions),
        true, true, element[Boolean]))
    }

    def anyZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("anyZK", classOf[Sym]),
        List(conditions),
        true, true, element[SigmaProp]))
    }

    def xorOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("xorOf", classOf[Sym]),
        List(conditions),
        true, true, element[Boolean]))
    }

    def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("PubKey", classOf[Sym]),
        List(base64String),
        true, true, element[SigmaProp]))
    }

    def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("sigmaProp", classOf[Sym]),
        List(b),
        true, true, element[SigmaProp]))
    }

    def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("blake2b256", classOf[Sym]),
        List(bytes),
        true, true, element[Coll[Byte]]))
    }

    def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("sha256", classOf[Sym]),
        List(bytes),
        true, true, element[Coll[Byte]]))
    }

    def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        List(bytes),
        true, true, element[BigInt]))
    }

    def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("longToByteArray", classOf[Sym]),
        List(l),
        true, true, element[Coll[Byte]]))
    }

    def byteArrayToLong(bytes: Rep[Coll[Byte]]): Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("byteArrayToLong", classOf[Sym]),
        List(bytes),
        true, true, element[Long]))
    }

    def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("proveDlog", classOf[Sym]),
        List(g),
        true, true, element[SigmaProp]))
    }

    def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        thisClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(g, h, u, v),
        true, true, element[SigmaProp]))
    }

    def groupGenerator: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("groupGenerator"),
        List(),
        true, true, element[GroupElement]))
    }

    def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        List(scriptBytes, positions, newValues, cT),
        true, true, element[Coll[Byte]]))
    }

    def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        thisClass.getMethod("decodePoint", classOf[Sym]),
        List(encoded),
        true, true, element[GroupElement]))
    }

    def BigInt(n: Rep[WBigInteger]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        thisClass.getMethod("BigInt", classOf[Sym]),
        List(n),
        true, true, element[BigInt]))
    }

    def toBigInteger(n: Rep[BigInt]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(source,
        thisClass.getMethod("toBigInteger", classOf[Sym]),
        List(n),
        true, true, element[WBigInteger]))
    }

    def avlTree(operationFlags: Rep[Byte], digest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        thisClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(operationFlags, digest, keyLength, valueLengthOpt),
        true, true, element[AvlTree]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaDslBuilder(p: Rep[SigmaDslBuilder]): SigmaDslBuilder = {
    if (p.rhs.isInstanceOf[SigmaDslBuilder@unchecked]) p.rhs.asInstanceOf[SigmaDslBuilder]
    else
      SigmaDslBuilderAdapter(p)
  }

  // familyElem
  class SigmaDslBuilderElem[To <: SigmaDslBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableSigmaDslBuilder.asLiftable[SSigmaDslBuilder, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaDslBuilder], classOf[SSigmaDslBuilder], Set(
        "Colls", "Monoids", "Costing", "CostModel", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "xorOf", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "byteArrayToLong", "proveDlog", "proveDHTuple", "groupGenerator", "substConstants", "decodePoint", "BigInt", "toBigInteger", "avlTree"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SigmaDslBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SigmaDslBuilder] => convertSigmaDslBuilder(x) }
      tryConvert(element[SigmaDslBuilder], this, x, conv)
    }

    def convertSigmaDslBuilder(x: Rep[SigmaDslBuilder]): Rep[To] = {
      x.elem match {
        case _: SigmaDslBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SigmaDslBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val sigmaDslBuilderElement: Elem[SigmaDslBuilder] =
    new SigmaDslBuilderElem[SigmaDslBuilder]

  implicit case object SigmaDslBuilderCompanionElem extends CompanionElem[SigmaDslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[SigmaDslBuilderCompanionCtor]
    protected def getDefaultRep = RSigmaDslBuilder
  }

  abstract class SigmaDslBuilderCompanionCtor extends CompanionDef[SigmaDslBuilderCompanionCtor] with SigmaDslBuilderCompanion {
    def selfType = SigmaDslBuilderCompanionElem
    override def toString = "SigmaDslBuilder"
  }
  implicit def proxySigmaDslBuilderCompanionCtor(p: Rep[SigmaDslBuilderCompanionCtor]): SigmaDslBuilderCompanionCtor =
    proxyOps[SigmaDslBuilderCompanionCtor](p)

  lazy val RSigmaDslBuilder: Rep[SigmaDslBuilderCompanionCtor] = new SigmaDslBuilderCompanionCtor {
    private val thisClass = classOf[SigmaDslBuilderCompanion]
  }

  object SigmaDslBuilderMethods {
    object Colls {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Colls" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Monoids {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Monoids" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Costing {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "Costing" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object CostModel {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "CostModel" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "verifyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "atLeast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "allOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "allZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "anyOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "anyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xorOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "xorOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "PubKey" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "sigmaProp" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "blake2b256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "sha256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "byteArrayToBigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "longToByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToLong {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "byteArrayToLong" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "proveDlog" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "proveDHTuple" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "groupGenerator" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object substConstants {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "substConstants" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object decodePoint {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "decodePoint" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object BigInt {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "BigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toBigInteger {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "toBigInteger" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object avlTree {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] && method.getName == "avlTree" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SigmaDslBuilderCompanionMethods {
  }
} // of object SigmaDslBuilder
  registerEntityObject("SigmaDslBuilder", SigmaDslBuilder)

  registerModule(SigmaDslModule)
}

object SigmaDslModule extends scalan.ModuleInfo("special.sigma", "SigmaDsl")
}

trait SigmaDslModule extends special.sigma.impl.SigmaDslDefs {self: SigmaLibrary =>}
