package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

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
    val resultType: Elem[CostModel] = liftable.eW
  }

  trait CostModelConstMethods extends CostModel  { thisConst: Def[_] =>

    private val CostModelClass = classOf[CostModel]

    override def AccessBox: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("AccessBox"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def AccessAvlTree: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("AccessAvlTree"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def GetVar: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("GetVar"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def DeserializeVar: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("DeserializeVar"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def GetRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("GetRegister"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def DeserializeRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("DeserializeRegister"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def SelectField: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("SelectField"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def CollectionConst: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("CollectionConst"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def AccessKiloByteOfData: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CostModelClass.getMethod("AccessKiloByteOfData"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def PubKeySize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        CostModelClass.getMethod("PubKeySize"),
        WrappedArray.empty,
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

  private val CostModelClass = classOf[CostModel]

  // entityAdapter for CostModel trait
  case class CostModelAdapter(source: Rep[CostModel])
      extends CostModel
      with Def[CostModel] {
    val resultType: Elem[CostModel] = element[CostModel]
    override def transform(t: Transformer) = CostModelAdapter(t(source))

    def AccessBox: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("AccessBox"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def AccessAvlTree: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("AccessAvlTree"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def GetVar: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("GetVar"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def DeserializeVar: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("DeserializeVar"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def GetRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("GetRegister"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def DeserializeRegister: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("DeserializeRegister"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def SelectField: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("SelectField"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def CollectionConst: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("CollectionConst"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def AccessKiloByteOfData: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostModelClass.getMethod("AccessKiloByteOfData"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def PubKeySize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        CostModelClass.getMethod("PubKeySize"),
        WrappedArray.empty,
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostModel(p: Rep[CostModel]): CostModel = {
    if (p.rhs.isInstanceOf[CostModel]) p.rhs.asInstanceOf[CostModel]
    else
      CostModelAdapter(p)
  }

  // familyElem
  class CostModelElem[To <: CostModel]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SCostModel, To](LiftableCostModel)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[CostModel], classOf[SCostModel], Set(
        "AccessBox", "AccessAvlTree", "GetVar", "DeserializeVar", "GetRegister", "DeserializeRegister", "SelectField", "CollectionConst", "AccessKiloByteOfData", "PubKeySize"
        ))
    }
  }

  implicit lazy val costModelElement: Elem[CostModel] =
    new CostModelElem[CostModel]

  implicit case object CostModelCompanionElem extends CompanionElem[CostModelCompanionCtor]

  abstract class CostModelCompanionCtor extends CompanionDef[CostModelCompanionCtor] with CostModelCompanion {
    def resultType = CostModelCompanionElem
    override def toString = "CostModel"
  }
  implicit def proxyCostModelCompanionCtor(p: Rep[CostModelCompanionCtor]): CostModelCompanionCtor =
    p.rhs.asInstanceOf[CostModelCompanionCtor]

  lazy val RCostModel: Rep[CostModelCompanionCtor] = new CostModelCompanionCtor {
    private val thisClass = classOf[CostModelCompanion]
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
    val resultType: Elem[BigInt] = liftable.eW
  }

  trait BigIntConstMethods extends BigInt  { thisConst: Def[_] =>

    private val BigIntClass = classOf[BigInt]

    override def toByte: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        BigIntClass.getMethod("toByte"),
        WrappedArray.empty,
        true, false, element[Byte]))
    }

    override def toShort: Rep[Short] = {
      asRep[Short](mkMethodCall(self,
        BigIntClass.getMethod("toShort"),
        WrappedArray.empty,
        true, false, element[Short]))
    }

    override def toInt: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        BigIntClass.getMethod("toInt"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def toLong: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        BigIntClass.getMethod("toLong"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def toBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BigIntClass.getMethod("toBytes"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def toBits: Rep[Coll[Boolean]] = {
      asRep[Coll[Boolean]](mkMethodCall(self,
        BigIntClass.getMethod("toBits"),
        WrappedArray.empty,
        true, false, element[Coll[Boolean]]))
    }

    override def toAbs: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("toAbs"),
        WrappedArray.empty,
        true, false, element[BigInt]))
    }

    override def compareTo(that: Rep[BigInt]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        BigIntClass.getMethod("compareTo", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[Int]))
    }

    override def modQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("modQ"),
        WrappedArray.empty,
        true, false, element[BigInt]))
    }

    override def plusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("plusModQ", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[BigInt]))
    }

    override def minusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("minusModQ", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[BigInt]))
    }

    override def multModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("multModQ", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[BigInt]))
    }

    override def inverseModQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("inverseModQ"),
        WrappedArray.empty,
        true, false, element[BigInt]))
    }

    override def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        BigIntClass.getMethod("signum"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def add(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("add", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def subtract(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("subtract", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def multiply(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def divide(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("divide", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def mod(m: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("mod", classOf[Sym]),
        Array[AnyRef](m),
        true, false, element[BigInt]))
    }

    override def remainder(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("remainder", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def min(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("min", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def max(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("max", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[BigInt]))
    }

    override def negate: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        BigIntClass.getMethod("negate"),
        WrappedArray.empty,
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

  private val BigIntClass = classOf[BigInt]

  // entityAdapter for BigInt trait
  case class BigIntAdapter(source: Rep[BigInt])
      extends BigInt
      with Def[BigInt] {
    val resultType: Elem[BigInt] = element[BigInt]
    override def transform(t: Transformer) = BigIntAdapter(t(source))

    def toByte: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        BigIntClass.getMethod("toByte"),
        WrappedArray.empty,
        true, true, element[Byte]))
    }

    def toShort: Rep[Short] = {
      asRep[Short](mkMethodCall(source,
        BigIntClass.getMethod("toShort"),
        WrappedArray.empty,
        true, true, element[Short]))
    }

    def toInt: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        BigIntClass.getMethod("toInt"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def toLong: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        BigIntClass.getMethod("toLong"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def toBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        BigIntClass.getMethod("toBytes"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def toBits: Rep[Coll[Boolean]] = {
      asRep[Coll[Boolean]](mkMethodCall(source,
        BigIntClass.getMethod("toBits"),
        WrappedArray.empty,
        true, true, element[Coll[Boolean]]))
    }

    def toAbs: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("toAbs"),
        WrappedArray.empty,
        true, true, element[BigInt]))
    }

    def compareTo(that: Rep[BigInt]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        BigIntClass.getMethod("compareTo", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Int]))
    }

    def modQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("modQ"),
        WrappedArray.empty,
        true, true, element[BigInt]))
    }

    def plusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("plusModQ", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[BigInt]))
    }

    def minusModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("minusModQ", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[BigInt]))
    }

    def multModQ(other: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("multModQ", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[BigInt]))
    }

    def inverseModQ: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("inverseModQ"),
        WrappedArray.empty,
        true, true, element[BigInt]))
    }

    def signum: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        BigIntClass.getMethod("signum"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def add(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("add", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def subtract(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("subtract", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def multiply(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def divide(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("divide", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def mod(m: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("mod", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[BigInt]))
    }

    def remainder(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("remainder", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def min(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("min", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def max(that: Rep[BigInt]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("max", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[BigInt]))
    }

    def negate: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        BigIntClass.getMethod("negate"),
        WrappedArray.empty,
        true, true, element[BigInt]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBigInt(p: Rep[BigInt]): BigInt = {
    if (p.rhs.isInstanceOf[BigInt]) p.rhs.asInstanceOf[BigInt]
    else
      BigIntAdapter(p)
  }

  // familyElem
  class BigIntElem[To <: BigInt]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SBigInt, To](LiftableBigInt)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[BigInt], classOf[SBigInt], Set(
        "toByte", "toShort", "toInt", "toLong", "toBytes", "toBits", "toAbs", "compareTo", "modQ", "plusModQ", "minusModQ", "multModQ", "inverseModQ", "signum", "add", "subtract", "multiply", "divide", "mod", "remainder", "min", "max", "negate"
        ))
    }
  }

  implicit lazy val bigIntElement: Elem[BigInt] =
    new BigIntElem[BigInt]

  implicit case object BigIntCompanionElem extends CompanionElem[BigIntCompanionCtor]

  abstract class BigIntCompanionCtor extends CompanionDef[BigIntCompanionCtor] with BigIntCompanion {
    def resultType = BigIntCompanionElem
    override def toString = "BigInt"
  }
  implicit def proxyBigIntCompanionCtor(p: Rep[BigIntCompanionCtor]): BigIntCompanionCtor =
    p.rhs.asInstanceOf[BigIntCompanionCtor]

  lazy val RBigInt: Rep[BigIntCompanionCtor] = new BigIntCompanionCtor {
    private val thisClass = classOf[BigIntCompanion]
  }

  object BigIntMethods {
    object toByte {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toByte" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object toShort {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toShort" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object toInt {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toInt" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object toLong {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toLong" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object toBytes {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toBytes" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object toBits {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toBits" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object toAbs {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "toAbs" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object compareTo {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "compareTo" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object modQ {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "modQ" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object plusModQ {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "plusModQ" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object minusModQ {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "minusModQ" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object multModQ {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "multModQ" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object inverseModQ {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "inverseModQ" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object signum {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "signum" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
    }

    object add {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "add" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object subtract {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "subtract" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "multiply" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object divide {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "divide" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object mod {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mod" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object remainder {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "remainder" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object min {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "min" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object max {
      def unapply(d: Def[_]): Nullable[(Rep[BigInt], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "max" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[BigInt], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[BigInt], Rep[BigInt])] = unapply(exp.rhs)
    }

    object negate {
      def unapply(d: Def[_]): Nullable[Rep[BigInt]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "negate" && receiver.elem.isInstanceOf[BigIntElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[BigInt]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[BigInt]] = unapply(exp.rhs)
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
    val resultType: Elem[GroupElement] = liftable.eW
  }

  trait GroupElementConstMethods extends GroupElement  { thisConst: Def[_] =>

    private val GroupElementClass = classOf[GroupElement]

    override def isInfinity: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        GroupElementClass.getMethod("isInfinity"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def exp(k: Rep[BigInt]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        GroupElementClass.getMethod("exp", classOf[Sym]),
        Array[AnyRef](k),
        true, false, element[GroupElement]))
    }

    override def multiply(that: Rep[GroupElement]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        GroupElementClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[GroupElement]))
    }

    override def negate: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        GroupElementClass.getMethod("negate"),
        WrappedArray.empty,
        true, false, element[GroupElement]))
    }

    override def getEncoded: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        GroupElementClass.getMethod("getEncoded"),
        WrappedArray.empty,
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

  private val GroupElementClass = classOf[GroupElement]

  // entityAdapter for GroupElement trait
  case class GroupElementAdapter(source: Rep[GroupElement])
      extends GroupElement
      with Def[GroupElement] {
    val resultType: Elem[GroupElement] = element[GroupElement]
    override def transform(t: Transformer) = GroupElementAdapter(t(source))

    def isInfinity: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        GroupElementClass.getMethod("isInfinity"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def exp(k: Rep[BigInt]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        GroupElementClass.getMethod("exp", classOf[Sym]),
        Array[AnyRef](k),
        true, true, element[GroupElement]))
    }

    def multiply(that: Rep[GroupElement]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        GroupElementClass.getMethod("multiply", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[GroupElement]))
    }

    def negate: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        GroupElementClass.getMethod("negate"),
        WrappedArray.empty,
        true, true, element[GroupElement]))
    }

    def getEncoded: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        GroupElementClass.getMethod("getEncoded"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyGroupElement(p: Rep[GroupElement]): GroupElement = {
    if (p.rhs.isInstanceOf[GroupElement]) p.rhs.asInstanceOf[GroupElement]
    else
      GroupElementAdapter(p)
  }

  // familyElem
  class GroupElementElem[To <: GroupElement]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SGroupElement, To](LiftableGroupElement)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[GroupElement], classOf[SGroupElement], Set(
        "isInfinity", "exp", "multiply", "negate", "getEncoded"
        ))
    }
  }

  implicit lazy val groupElementElement: Elem[GroupElement] =
    new GroupElementElem[GroupElement]

  implicit case object GroupElementCompanionElem extends CompanionElem[GroupElementCompanionCtor]

  abstract class GroupElementCompanionCtor extends CompanionDef[GroupElementCompanionCtor] with GroupElementCompanion {
    def resultType = GroupElementCompanionElem
    override def toString = "GroupElement"
  }
  implicit def proxyGroupElementCompanionCtor(p: Rep[GroupElementCompanionCtor]): GroupElementCompanionCtor =
    p.rhs.asInstanceOf[GroupElementCompanionCtor]

  lazy val RGroupElement: Rep[GroupElementCompanionCtor] = new GroupElementCompanionCtor {
    private val thisClass = classOf[GroupElementCompanion]
  }

  object GroupElementMethods {
    object isInfinity {
      def unapply(d: Def[_]): Nullable[Rep[GroupElement]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "isInfinity" && receiver.elem.isInstanceOf[GroupElementElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[GroupElement]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[GroupElement]] = unapply(exp.rhs)
    }

    object exp {
      def unapply(d: Def[_]): Nullable[(Rep[GroupElement], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "exp" && receiver.elem.isInstanceOf[GroupElementElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[GroupElement], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[GroupElement], Rep[BigInt])] = unapply(exp.rhs)
    }

    object multiply {
      def unapply(d: Def[_]): Nullable[(Rep[GroupElement], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "multiply" && receiver.elem.isInstanceOf[GroupElementElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[GroupElement], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[GroupElement], Rep[GroupElement])] = unapply(exp.rhs)
    }

    object negate {
      def unapply(d: Def[_]): Nullable[Rep[GroupElement]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "negate" && receiver.elem.isInstanceOf[GroupElementElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[GroupElement]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[GroupElement]] = unapply(exp.rhs)
    }

    object getEncoded {
      def unapply(d: Def[_]): Nullable[Rep[GroupElement]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "getEncoded" && receiver.elem.isInstanceOf[GroupElementElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[GroupElement]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[GroupElement]] = unapply(exp.rhs)
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
    val resultType: Elem[SigmaProp] = liftable.eW
  }

  trait SigmaPropConstMethods extends SigmaProp  { thisConst: Def[_] =>

    private val SigmaPropClass = classOf[SigmaProp]

    override def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaPropClass.getMethod("isValid"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaPropClass.getMethod("propBytes"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    // manual fix &&
    override def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[SigmaProp]))
    }

    // manual fix &&
    override def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        Array[AnyRef](other, o),
        true, false, element[SigmaProp]))
    }

    // manual fix ||
    override def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[SigmaProp]))
    }

    // manual fix ||
    override def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        Array[AnyRef](other, o),
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

  private val SigmaPropClass = classOf[SigmaProp]

  // entityAdapter for SigmaProp trait
  case class SigmaPropAdapter(source: Rep[SigmaProp])
      extends SigmaProp
      with Def[SigmaProp] {
    val resultType: Elem[SigmaProp] = element[SigmaProp]
    override def transform(t: Transformer) = SigmaPropAdapter(t(source))

    def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaPropClass.getMethod("isValid"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaPropClass.getMethod("propBytes"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    // manual fix &&
    def &&(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[SigmaProp]))
    }

    // manual fix &&
    def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        Array[AnyRef](other, o),
        true, true, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[SigmaProp]))
    }

    // manual fix ||
    def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        Array[AnyRef](other, o),
        true, true, element[SigmaProp]))
    }

    def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("lazyAnd", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[SigmaProp]))
    }

    def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaPropClass.getMethod("lazyOr", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[SigmaProp]))
    }

    // manual fix
    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        SigmaPropClass.getMethod("builder"),
        Array[AnyRef](),
        true, true, element[SigmaDslBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaProp(p: Rep[SigmaProp]): SigmaProp = {
    if (p.rhs.isInstanceOf[SigmaProp]) p.rhs.asInstanceOf[SigmaProp]
    else
      SigmaPropAdapter(p)
  }

  // familyElem
  class SigmaPropElem[To <: SigmaProp]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSigmaProp, To](LiftableSigmaProp)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaProp], classOf[SSigmaProp], Set(
        "isValid", "propBytes", "$amp$amp", "$amp$amp", "$bar$bar", "$bar$bar"
        ))
    }
  }

  implicit lazy val sigmaPropElement: Elem[SigmaProp] =
    new SigmaPropElem[SigmaProp]

  implicit case object SigmaPropCompanionElem extends CompanionElem[SigmaPropCompanionCtor]

  abstract class SigmaPropCompanionCtor extends CompanionDef[SigmaPropCompanionCtor] with SigmaPropCompanion {
    def resultType = SigmaPropCompanionElem
    override def toString = "SigmaProp"
  }
  implicit def proxySigmaPropCompanionCtor(p: Rep[SigmaPropCompanionCtor]): SigmaPropCompanionCtor =
    p.rhs.asInstanceOf[SigmaPropCompanionCtor]

  lazy val RSigmaProp: Rep[SigmaPropCompanionCtor] = new SigmaPropCompanionCtor {
    private val thisClass = classOf[SigmaPropCompanion]
  }

  object SigmaPropMethods {
    object isValid {
      def unapply(d: Def[_]): Nullable[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "isValid" && receiver.elem.isInstanceOf[SigmaPropElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaProp]] = unapply(exp.rhs)
    }

    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[SigmaProp]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "propBytes" && receiver.elem.isInstanceOf[SigmaPropElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaProp]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaProp]] = unapply(exp.rhs)
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "$amp$amp" && receiver.elem.isInstanceOf[SigmaPropElem[_]] && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = unapply(exp.rhs)
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "$amp$amp" && receiver.elem.isInstanceOf[SigmaPropElem[_]] && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Boolean])] = unapply(exp.rhs)
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "$bar$bar" && receiver.elem.isInstanceOf[SigmaPropElem[_]] && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[SigmaProp])] = unapply(exp.rhs)
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaProp], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "$bar$bar" && receiver.elem.isInstanceOf[SigmaPropElem[_]] && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaProp], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaProp], Rep[Boolean])] = unapply(exp.rhs)
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
    val resultType: Elem[AnyValue] = liftable.eW
  }

  trait AnyValueConstMethods extends AnyValue  { thisConst: Def[_] =>

    private val AnyValueClass = classOf[AnyValue]

    // manual fix
    override def value: Rep[Any] = {
      asRep[Any](mkMethodCall(self,
        AnyValueClass.getMethod("value"),
        WrappedArray.empty,
        true, false, AnyElement))
    }

    override def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(self,
        AnyValueClass.getMethod("tVal"),
        WrappedArray.empty,
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

  private val AnyValueClass = classOf[AnyValue]

  // entityAdapter for AnyValue trait
  case class AnyValueAdapter(source: Rep[AnyValue])
      extends AnyValue
      with Def[AnyValue] {
    val resultType: Elem[AnyValue] = element[AnyValue]
    override def transform(t: Transformer) = AnyValueAdapter(t(source))

    // manual fix
    def value: Rep[Any] = {
      asRep[Any](mkMethodCall(source,
        AnyValueClass.getMethod("value"),
        WrappedArray.empty,
        true, true, AnyElement))
    }

    def tVal: Rep[WRType[Any]] = {
      asRep[WRType[Any]](mkMethodCall(source,
        AnyValueClass.getMethod("tVal"),
        WrappedArray.empty,
        true, true, element[WRType[Any]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAnyValue(p: Rep[AnyValue]): AnyValue = {
    if (p.rhs.isInstanceOf[AnyValue]) p.rhs.asInstanceOf[AnyValue]
    else
      AnyValueAdapter(p)
  }

  // familyElem
  class AnyValueElem[To <: AnyValue]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SAnyValue, To](LiftableAnyValue)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[AnyValue], classOf[SAnyValue], Set(
        "value", "tVal"
        ))
    }
  }

  implicit lazy val anyValueElement: Elem[AnyValue] =
    new AnyValueElem[AnyValue]

  implicit case object AnyValueCompanionElem extends CompanionElem[AnyValueCompanionCtor]

  abstract class AnyValueCompanionCtor extends CompanionDef[AnyValueCompanionCtor] with AnyValueCompanion {
    def resultType = AnyValueCompanionElem
    override def toString = "AnyValue"
  }
  implicit def proxyAnyValueCompanionCtor(p: Rep[AnyValueCompanionCtor]): AnyValueCompanionCtor =
    p.rhs.asInstanceOf[AnyValueCompanionCtor]

  lazy val RAnyValue: Rep[AnyValueCompanionCtor] = new AnyValueCompanionCtor {
    private val thisClass = classOf[AnyValueCompanion]
  }

  object AnyValueMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "value" && receiver.elem.isInstanceOf[AnyValueElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AnyValue]] = unapply(exp.rhs)
    }

    object tVal {
      def unapply(d: Def[_]): Nullable[Rep[AnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tVal" && receiver.elem.isInstanceOf[AnyValueElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AnyValue]] = unapply(exp.rhs)
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
    val resultType: Elem[Box] = liftable.eW
  }

  trait BoxConstMethods extends Box  { thisConst: Def[_] =>

    private val BoxClass = classOf[Box]

    override def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("id"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def value: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        BoxClass.getMethod("value"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def propositionBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("propositionBytes"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def bytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("bytes"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def bytesWithoutRef: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        BoxClass.getMethod("bytesWithoutRef"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def registers: Rep[Coll[AnyValue]] = {
      asRep[Coll[AnyValue]](mkMethodCall(self,
        BoxClass.getMethod("registers"),
        WrappedArray.empty,
        true, false, element[Coll[AnyValue]]))
    }

    override def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        BoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](i, cT),
        true, false, element[WOption[T]]))
    }

    override def tokens: Rep[Coll[(Coll[Byte], Long)]] = {
      asRep[Coll[(Coll[Byte], Long)]](mkMethodCall(self,
        BoxClass.getMethod("tokens"),
        WrappedArray.empty,
        true, false, element[Coll[(Coll[Byte], Long)]]))
    }

    override def creationInfo: Rep[(Int, Coll[Byte])] = {
      asRep[(Int, Coll[Byte])](mkMethodCall(self,
        BoxClass.getMethod("creationInfo"),
        WrappedArray.empty,
        true, false, element[(Int, Coll[Byte])]))
    }

    override def executeFromRegister[T](regId: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[T](mkMethodCall(self,
        BoxClass.getMethod("executeFromRegister", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](regId, cT),
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

  private val BoxClass = classOf[Box]

  // entityAdapter for Box trait
  case class BoxAdapter(source: Rep[Box])
      extends Box
      with Def[Box] {
    val resultType: Elem[Box] = element[Box]
    override def transform(t: Transformer) = BoxAdapter(t(source))

    def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        BoxClass.getMethod("id"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def value: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        BoxClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def propositionBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        BoxClass.getMethod("propositionBytes"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def bytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        BoxClass.getMethod("bytes"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def bytesWithoutRef: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        BoxClass.getMethod("bytesWithoutRef"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def registers: Rep[Coll[AnyValue]] = {
      asRep[Coll[AnyValue]](mkMethodCall(source,
        BoxClass.getMethod("registers"),
        WrappedArray.empty,
        true, true, element[Coll[AnyValue]]))
    }

    def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        BoxClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](i, cT),
        true, true, element[WOption[T]]))
    }

    def tokens: Rep[Coll[(Coll[Byte], Long)]] = {
      asRep[Coll[(Coll[Byte], Long)]](mkMethodCall(source,
        BoxClass.getMethod("tokens"),
        WrappedArray.empty,
        true, true, element[Coll[(Coll[Byte], Long)]]))
    }

    def creationInfo: Rep[(Int, Coll[Byte])] = {
      asRep[(Int, Coll[Byte])](mkMethodCall(source,
        BoxClass.getMethod("creationInfo"),
        WrappedArray.empty,
        true, true, element[(Int, Coll[Byte])]))
    }

    def executeFromRegister[T](regId: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[T](mkMethodCall(source,
        BoxClass.getMethod("executeFromRegister", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](regId, cT),
        true, true, element[T]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyBox(p: Rep[Box]): Box = {
    if (p.rhs.isInstanceOf[Box]) p.rhs.asInstanceOf[Box]
    else
      BoxAdapter(p)
  }

  // familyElem
  class BoxElem[To <: Box]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SBox, To](LiftableBox)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Box], classOf[SBox], Set(
        "id", "value", "propositionBytes", "bytes", "bytesWithoutRef", "registers", "getReg", "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "tokens", "creationInfo", "executeFromRegister"
        ))
    }
  }

  implicit lazy val boxElement: Elem[Box] =
    new BoxElem[Box]

  implicit case object BoxCompanionElem extends CompanionElem[BoxCompanionCtor]

  abstract class BoxCompanionCtor extends CompanionDef[BoxCompanionCtor] with BoxCompanion {
    def resultType = BoxCompanionElem
    override def toString = "Box"
  }
  implicit def proxyBoxCompanionCtor(p: Rep[BoxCompanionCtor]): BoxCompanionCtor =
    p.rhs.asInstanceOf[BoxCompanionCtor]

  lazy val RBox: Rep[BoxCompanionCtor] = new BoxCompanionCtor {
    private val thisClass = classOf[BoxCompanion]
  }

  object BoxMethods {
    object id {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "id" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "value" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "propositionBytes" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "bytes" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "bytesWithoutRef" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "registers" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getReg" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Rep[Int], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R0 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R0" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R1 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R1" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R2 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R2" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R3 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R3" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R4 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R4" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R5 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R5" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R6 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R6" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R7 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R7" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R8 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R8" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object R9 {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "R9" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object tokens {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tokens" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object creationInfo {
      def unapply(d: Def[_]): Nullable[Rep[Box]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "creationInfo" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Box]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Box]] = unapply(exp.rhs)
    }

    object executeFromRegister {
      def unapply(d: Def[_]): Nullable[(Rep[Box], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "executeFromRegister" && receiver.elem.isInstanceOf[BoxElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Box], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Box], Rep[Byte], Elem[T]) forSome {type T}] = unapply(exp.rhs)
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
    val resultType: Elem[AvlTree] = liftable.eW
  }

  trait AvlTreeConstMethods extends AvlTree  { thisConst: Def[_] =>

    private val AvlTreeClass = classOf[AvlTree]

    override def digest: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        AvlTreeClass.getMethod("digest"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def enabledOperations: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        AvlTreeClass.getMethod("enabledOperations"),
        WrappedArray.empty,
        true, false, element[Byte]))
    }

    override def keyLength: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        AvlTreeClass.getMethod("keyLength"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def valueLengthOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(self,
        AvlTreeClass.getMethod("valueLengthOpt"),
        WrappedArray.empty,
        true, false, element[WOption[Int]]))
    }

    override def isInsertAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("isInsertAllowed"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def isUpdateAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("isUpdateAllowed"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def isRemoveAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("isRemoveAllowed"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def updateDigest(newDigest: Rep[Coll[Byte]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        AvlTreeClass.getMethod("updateDigest", classOf[Sym]),
        Array[AnyRef](newDigest),
        true, false, element[AvlTree]))
    }

    override def updateOperations(newOperations: Rep[Byte]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        AvlTreeClass.getMethod("updateOperations", classOf[Sym]),
        Array[AnyRef](newOperations),
        true, false, element[AvlTree]))
    }

    override def contains(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        AvlTreeClass.getMethod("contains", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proof),
        true, false, element[Boolean]))
    }

    override def get(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(self,
        AvlTreeClass.getMethod("get", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proof),
        true, false, element[WOption[Coll[Byte]]]))
    }

    override def getMany(keys: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[Coll[WOption[Coll[Byte]]]] = {
      asRep[Coll[WOption[Coll[Byte]]]](mkMethodCall(self,
        AvlTreeClass.getMethod("getMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](keys, proof),
        true, false, element[Coll[WOption[Coll[Byte]]]]))
    }

    override def insert(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(self,
        AvlTreeClass.getMethod("insert", classOf[Sym], classOf[Sym]),
        Array[AnyRef](operations, proof),
        true, false, element[WOption[AvlTree]]))
    }

    override def update(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(self,
        AvlTreeClass.getMethod("update", classOf[Sym], classOf[Sym]),
        Array[AnyRef](operations, proof),
        true, false, element[WOption[AvlTree]]))
    }

    override def remove(operations: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(self,
        AvlTreeClass.getMethod("remove", classOf[Sym], classOf[Sym]),
        Array[AnyRef](operations, proof),
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

  private val AvlTreeClass = classOf[AvlTree]

  // entityAdapter for AvlTree trait
  case class AvlTreeAdapter(source: Rep[AvlTree])
      extends AvlTree
      with Def[AvlTree] {
    val resultType: Elem[AvlTree] = element[AvlTree]
    override def transform(t: Transformer) = AvlTreeAdapter(t(source))

    def digest: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        AvlTreeClass.getMethod("digest"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def enabledOperations: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        AvlTreeClass.getMethod("enabledOperations"),
        WrappedArray.empty,
        true, true, element[Byte]))
    }

    def keyLength: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        AvlTreeClass.getMethod("keyLength"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def valueLengthOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        AvlTreeClass.getMethod("valueLengthOpt"),
        WrappedArray.empty,
        true, true, element[WOption[Int]]))
    }

    def isInsertAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        AvlTreeClass.getMethod("isInsertAllowed"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def isUpdateAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        AvlTreeClass.getMethod("isUpdateAllowed"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def isRemoveAllowed: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        AvlTreeClass.getMethod("isRemoveAllowed"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def updateDigest(newDigest: Rep[Coll[Byte]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        AvlTreeClass.getMethod("updateDigest", classOf[Sym]),
        Array[AnyRef](newDigest),
        true, true, element[AvlTree]))
    }

    def updateOperations(newOperations: Rep[Byte]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        AvlTreeClass.getMethod("updateOperations", classOf[Sym]),
        Array[AnyRef](newOperations),
        true, true, element[AvlTree]))
    }

    def contains(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        AvlTreeClass.getMethod("contains", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proof),
        true, true, element[Boolean]))
    }

    def get(key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(source,
        AvlTreeClass.getMethod("get", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proof),
        true, true, element[WOption[Coll[Byte]]]))
    }

    def getMany(keys: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[Coll[WOption[Coll[Byte]]]] = {
      asRep[Coll[WOption[Coll[Byte]]]](mkMethodCall(source,
        AvlTreeClass.getMethod("getMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](keys, proof),
        true, true, element[Coll[WOption[Coll[Byte]]]]))
    }

    def insert(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(source,
        AvlTreeClass.getMethod("insert", classOf[Sym], classOf[Sym]),
        Array[AnyRef](operations, proof),
        true, true, element[WOption[AvlTree]]))
    }

    def update(operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(source,
        AvlTreeClass.getMethod("update", classOf[Sym], classOf[Sym]),
        Array[AnyRef](operations, proof),
        true, true, element[WOption[AvlTree]]))
    }

    def remove(operations: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[WOption[AvlTree]] = {
      asRep[WOption[AvlTree]](mkMethodCall(source,
        AvlTreeClass.getMethod("remove", classOf[Sym], classOf[Sym]),
        Array[AnyRef](operations, proof),
        true, true, element[WOption[AvlTree]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyAvlTree(p: Rep[AvlTree]): AvlTree = {
    if (p.rhs.isInstanceOf[AvlTree]) p.rhs.asInstanceOf[AvlTree]
    else
      AvlTreeAdapter(p)
  }

  // familyElem
  class AvlTreeElem[To <: AvlTree]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SAvlTree, To](LiftableAvlTree)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[AvlTree], classOf[SAvlTree], Set(
        "digest", "enabledOperations", "keyLength", "valueLengthOpt", "isInsertAllowed", "isUpdateAllowed", "isRemoveAllowed", "updateDigest", "updateOperations", "contains", "get", "getMany", "insert", "update", "remove"
        ))
    }
  }

  implicit lazy val avlTreeElement: Elem[AvlTree] =
    new AvlTreeElem[AvlTree]

  implicit case object AvlTreeCompanionElem extends CompanionElem[AvlTreeCompanionCtor]

  abstract class AvlTreeCompanionCtor extends CompanionDef[AvlTreeCompanionCtor] with AvlTreeCompanion {
    def resultType = AvlTreeCompanionElem
    override def toString = "AvlTree"
  }
  implicit def proxyAvlTreeCompanionCtor(p: Rep[AvlTreeCompanionCtor]): AvlTreeCompanionCtor =
    p.rhs.asInstanceOf[AvlTreeCompanionCtor]

  lazy val RAvlTree: Rep[AvlTreeCompanionCtor] = new AvlTreeCompanionCtor {
    private val thisClass = classOf[AvlTreeCompanion]
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
    val resultType: Elem[PreHeader] = liftable.eW
  }

  trait PreHeaderConstMethods extends PreHeader  { thisConst: Def[_] =>

    private val PreHeaderClass = classOf[PreHeader]

    override def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        PreHeaderClass.getMethod("version"),
        WrappedArray.empty,
        true, false, element[Byte]))
    }

    override def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        PreHeaderClass.getMethod("parentId"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        PreHeaderClass.getMethod("timestamp"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        PreHeaderClass.getMethod("nBits"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def height: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        PreHeaderClass.getMethod("height"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        PreHeaderClass.getMethod("minerPk"),
        WrappedArray.empty,
        true, false, element[GroupElement]))
    }

    override def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        PreHeaderClass.getMethod("votes"),
        WrappedArray.empty,
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

  private val PreHeaderClass = classOf[PreHeader]

  // entityAdapter for PreHeader trait
  case class PreHeaderAdapter(source: Rep[PreHeader])
      extends PreHeader
      with Def[PreHeader] {
    val resultType: Elem[PreHeader] = element[PreHeader]
    override def transform(t: Transformer) = PreHeaderAdapter(t(source))

    def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        PreHeaderClass.getMethod("version"),
        WrappedArray.empty,
        true, true, element[Byte]))
    }

    def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        PreHeaderClass.getMethod("parentId"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        PreHeaderClass.getMethod("timestamp"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        PreHeaderClass.getMethod("nBits"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def height: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        PreHeaderClass.getMethod("height"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        PreHeaderClass.getMethod("minerPk"),
        WrappedArray.empty,
        true, true, element[GroupElement]))
    }

    def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        PreHeaderClass.getMethod("votes"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyPreHeader(p: Rep[PreHeader]): PreHeader = {
    if (p.rhs.isInstanceOf[PreHeader]) p.rhs.asInstanceOf[PreHeader]
    else
      PreHeaderAdapter(p)
  }

  // familyElem
  class PreHeaderElem[To <: PreHeader]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SPreHeader, To](LiftablePreHeader)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[PreHeader], classOf[SPreHeader], Set(
        "version", "parentId", "timestamp", "nBits", "height", "minerPk", "votes"
        ))
    }
  }

  implicit lazy val preHeaderElement: Elem[PreHeader] =
    new PreHeaderElem[PreHeader]

  implicit case object PreHeaderCompanionElem extends CompanionElem[PreHeaderCompanionCtor]

  abstract class PreHeaderCompanionCtor extends CompanionDef[PreHeaderCompanionCtor] with PreHeaderCompanion {
    def resultType = PreHeaderCompanionElem
    override def toString = "PreHeader"
  }
  implicit def proxyPreHeaderCompanionCtor(p: Rep[PreHeaderCompanionCtor]): PreHeaderCompanionCtor =
    p.rhs.asInstanceOf[PreHeaderCompanionCtor]

  lazy val RPreHeader: Rep[PreHeaderCompanionCtor] = new PreHeaderCompanionCtor {
    private val thisClass = classOf[PreHeaderCompanion]
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
    val resultType: Elem[Header] = liftable.eW
  }

  trait HeaderConstMethods extends Header  { thisConst: Def[_] =>

    private val HeaderClass = classOf[Header]

    override def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("id"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(self,
        HeaderClass.getMethod("version"),
        WrappedArray.empty,
        true, false, element[Byte]))
    }

    override def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("parentId"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def ADProofsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("ADProofsRoot"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def stateRoot: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        HeaderClass.getMethod("stateRoot"),
        WrappedArray.empty,
        true, false, element[AvlTree]))
    }

    override def transactionsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("transactionsRoot"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        HeaderClass.getMethod("timestamp"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        HeaderClass.getMethod("nBits"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def height: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        HeaderClass.getMethod("height"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def extensionRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("extensionRoot"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        HeaderClass.getMethod("minerPk"),
        WrappedArray.empty,
        true, false, element[GroupElement]))
    }

    override def powOnetimePk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        HeaderClass.getMethod("powOnetimePk"),
        WrappedArray.empty,
        true, false, element[GroupElement]))
    }

    override def powNonce: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("powNonce"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def powDistance: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        HeaderClass.getMethod("powDistance"),
        WrappedArray.empty,
        true, false, element[BigInt]))
    }

    override def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        HeaderClass.getMethod("votes"),
        WrappedArray.empty,
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

  private val HeaderClass = classOf[Header]

  // entityAdapter for Header trait
  case class HeaderAdapter(source: Rep[Header])
      extends Header
      with Def[Header] {
    val resultType: Elem[Header] = element[Header]
    override def transform(t: Transformer) = HeaderAdapter(t(source))

    def id: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("id"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def version: Rep[Byte] = {
      asRep[Byte](mkMethodCall(source,
        HeaderClass.getMethod("version"),
        WrappedArray.empty,
        true, true, element[Byte]))
    }

    def parentId: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("parentId"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def ADProofsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("ADProofsRoot"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def stateRoot: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        HeaderClass.getMethod("stateRoot"),
        WrappedArray.empty,
        true, true, element[AvlTree]))
    }

    def transactionsRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("transactionsRoot"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def timestamp: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        HeaderClass.getMethod("timestamp"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def nBits: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        HeaderClass.getMethod("nBits"),
        WrappedArray.empty,
        true, true, element[Long]))
    }

    def height: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        HeaderClass.getMethod("height"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def extensionRoot: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("extensionRoot"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def minerPk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        HeaderClass.getMethod("minerPk"),
        WrappedArray.empty,
        true, true, element[GroupElement]))
    }

    def powOnetimePk: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        HeaderClass.getMethod("powOnetimePk"),
        WrappedArray.empty,
        true, true, element[GroupElement]))
    }

    def powNonce: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("powNonce"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def powDistance: Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        HeaderClass.getMethod("powDistance"),
        WrappedArray.empty,
        true, true, element[BigInt]))
    }

    def votes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        HeaderClass.getMethod("votes"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyHeader(p: Rep[Header]): Header = {
    if (p.rhs.isInstanceOf[Header]) p.rhs.asInstanceOf[Header]
    else
      HeaderAdapter(p)
  }

  // familyElem
  class HeaderElem[To <: Header]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SHeader, To](LiftableHeader)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Header], classOf[SHeader], Set(
        "id", "version", "parentId", "ADProofsRoot", "stateRoot", "transactionsRoot", "timestamp", "nBits", "height", "extensionRoot", "minerPk", "powOnetimePk", "powNonce", "powDistance", "votes"
        ))
    }
  }

  implicit lazy val headerElement: Elem[Header] =
    new HeaderElem[Header]

  implicit case object HeaderCompanionElem extends CompanionElem[HeaderCompanionCtor]

  abstract class HeaderCompanionCtor extends CompanionDef[HeaderCompanionCtor] with HeaderCompanion {
    def resultType = HeaderCompanionElem
    override def toString = "Header"
  }
  implicit def proxyHeaderCompanionCtor(p: Rep[HeaderCompanionCtor]): HeaderCompanionCtor =
    p.rhs.asInstanceOf[HeaderCompanionCtor]

  lazy val RHeader: Rep[HeaderCompanionCtor] = new HeaderCompanionCtor {
    private val thisClass = classOf[HeaderCompanion]
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
    val resultType: Elem[Context] = liftable.eW
  }

  trait ContextConstMethods extends Context  { thisConst: Def[_] =>

    private val ContextClass = classOf[Context]

    override def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        ContextClass.getMethod("builder"),
        WrappedArray.empty,
        true, false, element[SigmaDslBuilder]))
    }

    override def OUTPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        ContextClass.getMethod("OUTPUTS"),
        WrappedArray.empty,
        true, false, element[Coll[Box]]))
    }

    override def INPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        ContextClass.getMethod("INPUTS"),
        WrappedArray.empty,
        true, false, element[Coll[Box]]))
    }

    override def dataInputs: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        ContextClass.getMethod("dataInputs"),
        WrappedArray.empty,
        true, false, element[Coll[Box]]))
    }

    override def HEIGHT: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        ContextClass.getMethod("HEIGHT"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(self,
        ContextClass.getMethod("SELF"),
        WrappedArray.empty,
        true, false, element[Box]))
    }

    override def selfBoxIndex: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        ContextClass.getMethod("selfBoxIndex"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        ContextClass.getMethod("LastBlockUtxoRootHash"),
        WrappedArray.empty,
        true, false, element[AvlTree]))
    }

    override def headers: Rep[Coll[Header]] = {
      asRep[Coll[Header]](mkMethodCall(self,
        ContextClass.getMethod("headers"),
        WrappedArray.empty,
        true, false, element[Coll[Header]]))
    }

    override def preHeader: Rep[PreHeader] = {
      asRep[PreHeader](mkMethodCall(self,
        ContextClass.getMethod("preHeader"),
        WrappedArray.empty,
        true, false, element[PreHeader]))
    }

    override def minerPubKey: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        ContextClass.getMethod("minerPubKey"),
        WrappedArray.empty,
        true, false, element[Coll[Byte]]))
    }

    override def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        ContextClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, cT),
        true, false, element[WOption[T]]))
    }

    override def vars: Rep[Coll[AnyValue]] = {
      asRep[Coll[AnyValue]](mkMethodCall(self,
        ContextClass.getMethod("vars"),
        WrappedArray.empty,
        true, false, element[Coll[AnyValue]]))
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

  private val ContextClass = classOf[Context]

  // entityAdapter for Context trait
  case class ContextAdapter(source: Rep[Context])
      extends Context
      with Def[Context] {
    val resultType: Elem[Context] = element[Context]
    override def transform(t: Transformer) = ContextAdapter(t(source))

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        ContextClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[SigmaDslBuilder]))
    }

    def OUTPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(source,
        ContextClass.getMethod("OUTPUTS"),
        WrappedArray.empty,
        true, true, element[Coll[Box]]))
    }

    def INPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(source,
        ContextClass.getMethod("INPUTS"),
        WrappedArray.empty,
        true, true, element[Coll[Box]]))
    }

    def dataInputs: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(source,
        ContextClass.getMethod("dataInputs"),
        WrappedArray.empty,
        true, true, element[Coll[Box]]))
    }

    def HEIGHT: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        ContextClass.getMethod("HEIGHT"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(source,
        ContextClass.getMethod("SELF"),
        WrappedArray.empty,
        true, true, element[Box]))
    }

    def selfBoxIndex: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        ContextClass.getMethod("selfBoxIndex"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        ContextClass.getMethod("LastBlockUtxoRootHash"),
        WrappedArray.empty,
        true, true, element[AvlTree]))
    }

    def headers: Rep[Coll[Header]] = {
      asRep[Coll[Header]](mkMethodCall(source,
        ContextClass.getMethod("headers"),
        WrappedArray.empty,
        true, true, element[Coll[Header]]))
    }

    def preHeader: Rep[PreHeader] = {
      asRep[PreHeader](mkMethodCall(source,
        ContextClass.getMethod("preHeader"),
        WrappedArray.empty,
        true, true, element[PreHeader]))
    }

    def minerPubKey: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        ContextClass.getMethod("minerPubKey"),
        WrappedArray.empty,
        true, true, element[Coll[Byte]]))
    }

    def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        ContextClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, cT),
        true, true, element[WOption[T]]))
    }

    def vars: Rep[Coll[AnyValue]] = {
      asRep[Coll[AnyValue]](mkMethodCall(source,
        ContextClass.getMethod("vars"),
        WrappedArray.empty,
        true, true, element[Coll[AnyValue]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyContext(p: Rep[Context]): Context = {
    if (p.rhs.isInstanceOf[Context]) p.rhs.asInstanceOf[Context]
    else
      ContextAdapter(p)
  }

  // familyElem
  class ContextElem[To <: Context]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SContext, To](LiftableContext)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Context], classOf[SContext], Set(
        "builder", "OUTPUTS", "INPUTS", "dataInputs", "HEIGHT", "SELF", "selfBoxIndex", "LastBlockUtxoRootHash", "headers", "preHeader", "minerPubKey", "getVar", "vars"
        ))
    }
  }

  implicit lazy val contextElement: Elem[Context] =
    new ContextElem[Context]

  implicit case object ContextCompanionElem extends CompanionElem[ContextCompanionCtor]

  abstract class ContextCompanionCtor extends CompanionDef[ContextCompanionCtor] with ContextCompanion {
    def resultType = ContextCompanionElem
    override def toString = "Context"
  }
  implicit def proxyContextCompanionCtor(p: Rep[ContextCompanionCtor]): ContextCompanionCtor =
    p.rhs.asInstanceOf[ContextCompanionCtor]

  lazy val RContext: Rep[ContextCompanionCtor] = new ContextCompanionCtor {
    private val thisClass = classOf[ContextCompanion]
  }

  object ContextMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "builder" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "OUTPUTS" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object INPUTS {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "INPUTS" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object dataInputs {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "dataInputs" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object HEIGHT {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "HEIGHT" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object SELF {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "SELF" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object selfBoxIndex {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "selfBoxIndex" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "LastBlockUtxoRootHash" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object headers {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "headers" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object preHeader {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "preHeader" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object minerPubKey {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "minerPubKey" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getVar" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Context], Rep[Byte], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object vars {
      def unapply(d: Def[_]): Nullable[Rep[Context]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "vars" && receiver.elem.isInstanceOf[ContextElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Context]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Context]] = unapply(exp.rhs)
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
    val resultType: Elem[SigmaContract] = liftable.eW
  }

  trait SigmaContractConstMethods extends SigmaContract  { thisConst: Def[_] =>

    private val SigmaContractClass = classOf[SigmaContract]

    override def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(self,
        SigmaContractClass.getMethod("builder"),
        WrappedArray.empty,
        true, false, element[SigmaDslBuilder]))
    }

    override def Collection[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        SigmaContractClass.getMethod("Collection", classOf[Seq[_]], classOf[Elem[_]]),
        Array[AnyRef](items, cT),
        true, false, element[Coll[T]]))
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

  private val SigmaContractClass = classOf[SigmaContract]

  // entityAdapter for SigmaContract trait
  case class SigmaContractAdapter(source: Rep[SigmaContract])
      extends SigmaContract
      with Def[SigmaContract] {
    val resultType: Elem[SigmaContract] = element[SigmaContract]
    override def transform(t: Transformer) = SigmaContractAdapter(t(source))

    def builder: Rep[SigmaDslBuilder] = {
      asRep[SigmaDslBuilder](mkMethodCall(source,
        SigmaContractClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[SigmaDslBuilder]))
    }

    override def Collection[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        SigmaContractClass.getMethod("Collection", classOf[Seq[_]], classOf[Elem[_]]),
        Array[AnyRef](items, cT),
        true, true, element[Coll[T]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaContract(p: Rep[SigmaContract]): SigmaContract = {
    if (p.rhs.isInstanceOf[SigmaContract]) p.rhs.asInstanceOf[SigmaContract]
    else
      SigmaContractAdapter(p)
  }

  // familyElem
  class SigmaContractElem[To <: SigmaContract]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSigmaContract, To](LiftableSigmaContract)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaContract], classOf[SSigmaContract], Set(
        "builder", "Collection", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "xorOf", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "byteArrayToLong", "proveDlog", "proveDHTuple", "groupGenerator", "decodePoint", "substConstants"
        ))
    }
  }

  implicit lazy val sigmaContractElement: Elem[SigmaContract] =
    new SigmaContractElem[SigmaContract]

  implicit case object SigmaContractCompanionElem extends CompanionElem[SigmaContractCompanionCtor]

  abstract class SigmaContractCompanionCtor extends CompanionDef[SigmaContractCompanionCtor] with SigmaContractCompanion {
    def resultType = SigmaContractCompanionElem
    override def toString = "SigmaContract"
  }
  implicit def proxySigmaContractCompanionCtor(p: Rep[SigmaContractCompanionCtor]): SigmaContractCompanionCtor =
    p.rhs.asInstanceOf[SigmaContractCompanionCtor]

  lazy val RSigmaContract: Rep[SigmaContractCompanionCtor] = new SigmaContractCompanionCtor {
    private val thisClass = classOf[SigmaContractCompanion]
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
    val resultType: Elem[SigmaDslBuilder] = liftable.eW
  }

  trait SigmaDslBuilderConstMethods extends SigmaDslBuilder  { thisConst: Def[_] =>

    private val SigmaDslBuilderClass = classOf[SigmaDslBuilder]

    override def Colls: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Colls"),
        WrappedArray.empty,
        true, false, element[CollBuilder]))
    }

    override def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Monoids"),
        WrappedArray.empty,
        true, false, element[MonoidBuilder]))
    }

    override def Costing: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("Costing"),
        WrappedArray.empty,
        true, false, element[CostedBuilder]))
    }

    override def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("CostModel"),
        WrappedArray.empty,
        true, false, element[CostModel]))
    }

    override def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("verifyZK", classOf[Sym]),
        Array[AnyRef](cond),
        true, false, element[Boolean]))
    }

    override def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        Array[AnyRef](bound, props),
        true, false, element[SigmaProp]))
    }

    override def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def allZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[SigmaProp]))
    }

    override def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def anyZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[SigmaProp]))
    }

    override def xorOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, false, element[Boolean]))
    }

    override def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("PubKey", classOf[Sym]),
        Array[AnyRef](base64String),
        true, false, element[SigmaProp]))
    }

    override def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        Array[AnyRef](b),
        true, false, element[SigmaProp]))
    }

    override def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Coll[Byte]]))
    }

    override def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[BigInt]))
    }

    override def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        Array[AnyRef](l),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToLong(bytes: Rep[Coll[Byte]]): Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        Array[AnyRef](bytes),
        true, false, element[Long]))
    }

    override def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        Array[AnyRef](g),
        true, false, element[SigmaProp]))
    }

    override def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](g, h, u, v),
        true, false, element[SigmaProp]))
    }

    override def groupGenerator: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        WrappedArray.empty,
        true, false, element[GroupElement]))
    }

    override def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](scriptBytes, positions, newValues, cT),
        true, false, element[Coll[Byte]]))
    }

    override def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        Array[AnyRef](encoded),
        true, false, element[GroupElement]))
    }

    override def avlTree(operationFlags: Rep[Byte], digest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](operationFlags, digest, keyLength, valueLengthOpt),
        true, false, element[AvlTree]))
    }

    override def xor(l: Rep[Coll[Byte]], r: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        SigmaDslBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, false, element[Coll[Byte]]))
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

  private val SigmaDslBuilderClass = classOf[SigmaDslBuilder]

  // entityAdapter for SigmaDslBuilder trait
  case class SigmaDslBuilderAdapter(source: Rep[SigmaDslBuilder])
      extends SigmaDslBuilder
      with Def[SigmaDslBuilder] {
    val resultType: Elem[SigmaDslBuilder] = element[SigmaDslBuilder]
    override def transform(t: Transformer) = SigmaDslBuilderAdapter(t(source))

    def Colls: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("Colls"),
        WrappedArray.empty,
        true, true, element[CollBuilder]))
    }

    def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("Monoids"),
        WrappedArray.empty,
        true, true, element[MonoidBuilder]))
    }

    def Costing: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("Costing"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("CostModel"),
        WrappedArray.empty,
        true, true, element[CostModel]))
    }

    def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("verifyZK", classOf[Sym]),
        Array[AnyRef](cond),
        true, true, element[Boolean]))
    }

    def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        Array[AnyRef](bound, props),
        true, true, element[SigmaProp]))
    }

    def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("allOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def allZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("allZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[SigmaProp]))
    }

    def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("anyOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def anyZK(conditions: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("anyZK", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[SigmaProp]))
    }

    def xorOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("xorOf", classOf[Sym]),
        Array[AnyRef](conditions),
        true, true, element[Boolean]))
    }

    def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("PubKey", classOf[Sym]),
        Array[AnyRef](base64String),
        true, true, element[SigmaProp]))
    }

    def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("sigmaProp", classOf[Sym]),
        Array[AnyRef](b),
        true, true, element[SigmaProp]))
    }

    def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("blake2b256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Coll[Byte]]))
    }

    def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("sha256", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Coll[Byte]]))
    }

    def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[BigInt]))
    }

    def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("longToByteArray", classOf[Sym]),
        Array[AnyRef](l),
        true, true, element[Coll[Byte]]))
    }

    def byteArrayToLong(bytes: Rep[Coll[Byte]]): Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("byteArrayToLong", classOf[Sym]),
        Array[AnyRef](bytes),
        true, true, element[Long]))
    }

    def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("proveDlog", classOf[Sym]),
        Array[AnyRef](g),
        true, true, element[SigmaProp]))
    }

    def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](g, h, u, v),
        true, true, element[SigmaProp]))
    }

    def groupGenerator: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("groupGenerator"),
        WrappedArray.empty,
        true, true, element[GroupElement]))
    }

    def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](scriptBytes, positions, newValues, cT),
        true, true, element[Coll[Byte]]))
    }

    def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("decodePoint", classOf[Sym]),
        Array[AnyRef](encoded),
        true, true, element[GroupElement]))
    }

    def avlTree(operationFlags: Rep[Byte], digest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](operationFlags, digest, keyLength, valueLengthOpt),
        true, true, element[AvlTree]))
    }

    def xor(l: Rep[Coll[Byte]], r: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        SigmaDslBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySigmaDslBuilder(p: Rep[SigmaDslBuilder]): SigmaDslBuilder = {
    if (p.rhs.isInstanceOf[SigmaDslBuilder]) p.rhs.asInstanceOf[SigmaDslBuilder]
    else
      SigmaDslBuilderAdapter(p)
  }

  // familyElem
  class SigmaDslBuilderElem[To <: SigmaDslBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSigmaDslBuilder, To](LiftableSigmaDslBuilder)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SigmaDslBuilder], classOf[SSigmaDslBuilder], Set(
        "Colls", "Monoids", "Costing", "CostModel", "verifyZK", "atLeast", "allOf", "allZK", "anyOf", "anyZK", "xorOf", "PubKey", "sigmaProp", "blake2b256", "sha256", "byteArrayToBigInt", "longToByteArray", "byteArrayToLong", "proveDlog", "proveDHTuple", "groupGenerator", "substConstants", "decodePoint", "avlTree", "xor"
        ))
    }
  }

  implicit lazy val sigmaDslBuilderElement: Elem[SigmaDslBuilder] =
    new SigmaDslBuilderElem[SigmaDslBuilder]

  implicit case object SigmaDslBuilderCompanionElem extends CompanionElem[SigmaDslBuilderCompanionCtor]

  abstract class SigmaDslBuilderCompanionCtor extends CompanionDef[SigmaDslBuilderCompanionCtor] with SigmaDslBuilderCompanion {
    def resultType = SigmaDslBuilderCompanionElem
    override def toString = "SigmaDslBuilder"
  }
  implicit def proxySigmaDslBuilderCompanionCtor(p: Rep[SigmaDslBuilderCompanionCtor]): SigmaDslBuilderCompanionCtor =
    p.rhs.asInstanceOf[SigmaDslBuilderCompanionCtor]

  lazy val RSigmaDslBuilder: Rep[SigmaDslBuilderCompanionCtor] = new SigmaDslBuilderCompanionCtor {
    private val thisClass = classOf[SigmaDslBuilderCompanion]
  }

  object SigmaDslBuilderMethods {
    object Colls {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "Colls" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = unapply(exp.rhs)
    }

    object Monoids {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "Monoids" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = unapply(exp.rhs)
    }

    object Costing {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "Costing" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = unapply(exp.rhs)
    }

    object CostModel {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "CostModel" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = unapply(exp.rhs)
    }

    object verifyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "verifyZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Thunk[SigmaProp]])] = unapply(exp.rhs)
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "atLeast" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])] = unapply(exp.rhs)
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "allOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = unapply(exp.rhs)
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "allZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = unapply(exp.rhs)
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "anyOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = unapply(exp.rhs)
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "anyZK" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[SigmaProp]])] = unapply(exp.rhs)
    }

    object xorOf {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "xorOf" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Boolean]])] = unapply(exp.rhs)
    }

    object PubKey {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "PubKey" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[String])] = unapply(exp.rhs)
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "sigmaProp" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Boolean])] = unapply(exp.rhs)
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "blake2b256" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = unapply(exp.rhs)
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "sha256" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = unapply(exp.rhs)
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "byteArrayToBigInt" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = unapply(exp.rhs)
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "longToByteArray" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Long])] = unapply(exp.rhs)
    }

    object byteArrayToLong {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "byteArrayToLong" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = unapply(exp.rhs)
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "proveDlog" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement])] = unapply(exp.rhs)
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "proveDHTuple" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = unapply(exp.rhs)
    }

    object groupGenerator {
      def unapply(d: Def[_]): Nullable[Rep[SigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "groupGenerator" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SigmaDslBuilder]] = unapply(exp.rhs)
    }

    object substConstants {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "substConstants" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object decodePoint {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "decodePoint" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]])] = unapply(exp.rhs)
    }

    object avlTree {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "avlTree" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])] = unapply(exp.rhs)
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "xor" && receiver.elem.isInstanceOf[SigmaDslBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])] = unapply(exp.rhs)
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
