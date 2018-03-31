package sigmastate

import java.math.BigInteger
import sigmastate.SType.TypeCode
import sigmastate.interpreter.GroupSettings
import sigmastate.utils.Overloading.Overload1
import sigmastate.utxo.ErgoBox
import sigmastate.Values._

import scala.collection.mutable

/** Base type for all AST nodes of sigma lang. */
trait SigmaNode extends Product

/** Every type descriptor is a tree represented by nodes in SType hierarchy.
  * In order to extend type family:
  * - Implement concrete class derived from SType
  * - Implement serializer (see SCollectionSerializer) and register it in STypeSerializer.table
  * Each SType is serialized to array of bytes by:
  * - emitting typeCode of each node
  * - then recursively serializing subtrees from left to right on each level
  * */
sealed trait SType extends SigmaNode {
  type WrappedType
  val typeCode: SType.TypeCode

  def isPrimitive: Boolean = SType.allPredefTypes.contains(this)

  /** Elvis operator for types. See https://en.wikipedia.org/wiki/Elvis_operator*/
  def ?:(whenNoType: => SType): SType = if (this == NoType) whenNoType else this
}

object SType {
  type TypeCode = Byte

  implicit val typeInt = SInt
  implicit val typeBigInt = SBigInt
  implicit val typeBoolean = SBoolean
  implicit val typeByteArray = SByteArray
  implicit val typeAvlTree = SAvlTree
  implicit val typeGroupElement = SGroupElement
  implicit val typeBox = SBox

  implicit def typeCollection[V <: SType](implicit tV: V): SCollection[V] = SCollection[V]

  /** All primitive types should be listed here. Note, NoType is not primitive type. */
  val allPredefTypes = Seq(SInt, SBigInt, SBoolean, SByteArray, SAvlTree, SGroupElement, SBox, SUnit, SAny)
  val typeCodeToType = allPredefTypes.map(t => t.typeCode -> t).toMap

  implicit class STypeOps(tpe: SType) {
    def isCollection: Boolean = tpe.isInstanceOf[SCollection[_]]
    def canBeTypedAs(expected: SType): Boolean = (tpe, expected) match {
      case (NoType, _) => true
      case (t1, t2) if t1 == t2 => true
      case (f1: SFunc, f2: SFunc) =>
        val okDom = f1.tDom.size == f2.tDom.size &&
                     f1.tDom.zip(f2.tDom).forall { case (d1, d2) => d1.canBeTypedAs(d2) }
        val okRange = f1.tRange.canBeTypedAs(f2.tRange)
        okDom && okRange
    }
    def asFunc: SFunc = tpe.asInstanceOf[SFunc]
  }

  def typeOfData(x: Any): SType = x match {
    case i: Int => SInt
    case l: Long => SInt
    case b: Boolean => SBoolean
    case arr: Array[Byte] => SByteArray
    case big: BigInteger => SBigInt
    case g: GroupSettings.EcPointType => SGroupElement
    case box: ErgoBox => SBox
    case _: Unit => SUnit
    case v: SValue => v.tpe
    case _ => sys.error(s"Don't know how to return SType for $x: ${x.getClass}")
  }

}

/** Base trait for all primitive types which don't have internal type items. */
trait SPrimType extends SType {
}

/** Primitive type recognizer to pattern match on TypeCode */
object SPrimType {
  def unapply(tc: TypeCode): Option[SType] = SType.typeCodeToType.get(tc)
  def unapply(t: SType): Option[SType] = SType.allPredefTypes.find(_ == t)
}

/** Base trait for all types which have fields (aka properties) */
trait SProduct extends SType {
  /** Returns -1 if `field` is not found. */
  def fieldIndex(field: String): Int = fields.indexWhere(_._1 == field)
  def fields: Seq[(String, SType)]
  def sameFields(that: SProduct): Boolean = {
    if (fields.length != that.fields.length) return false
    // imperative to avoid allocation as it is supposed to be used frequently
    for (i <- fields.indices) {
      if (fields(i)._1 != that.fields(i)._1) return false
    }
    true
  }
}

/** Special type to represent untyped values.
  * Interpreter raises an error when encounter a Value with this type.
  * All Value nodes with this type should be elimitanted during typing.
  * If no specific type can be assigned statically during typing,
  * then either error should be raised or type SAny should be assigned
  * which is interpreted as dynamic typing. */
case object NoType extends SType {
  type WrappedType = Nothing
  val typeCode = 0
}

case object SInt extends SPrimType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 1: Byte
}

case object SBigInt extends SPrimType {
  override type WrappedType = BigInteger
  override val typeCode: TypeCode = 2: Byte
}

case object SBoolean extends SPrimType {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 3: Byte
}

case object SByteArray extends SPrimType {
  override type WrappedType = Array[Byte]
  override val typeCode: TypeCode = 4: Byte
}

case object SAvlTree extends SProduct {
  override type WrappedType = AvlTreeData
  override val typeCode: TypeCode = 5: Byte
  val fields = Seq()
}

case object SGroupElement extends SProduct {
  override type WrappedType = GroupSettings.EcPointType
  override val typeCode: TypeCode = 6: Byte
  val fields = Seq(
    "isIdentity" -> SBoolean,
    "nonce" -> SByteArray
  )
}

case object SBox extends SProduct {
  override type WrappedType = ErgoBox
  override val typeCode: TypeCode = 7: Byte
  private val tT = STypeIdent("T")
  def registers(): Seq[(String, SType)] = {
    (1 to 10).map(i => s"R$i" -> SFunc(IndexedSeq(), SOption(tT), Seq(tT)))
  }
  val PropositionBytes = "propositionBytes"
  val Value = "value"
  val Id = "id"
  val Bytes = "bytes"
  val fields = Vector(
    Value -> SInt,                   // see ExtractAmount
    PropositionBytes -> SByteArray,  // see ExtractScriptBytes
    Bytes -> SByteArray,             // see ExtractBytes
    Id -> SByteArray                 // see ExtractId
  ) ++ registers()
}

/** The type with single inhabitant value `()` */
case object SUnit extends SPrimType {
  override type WrappedType = Unit
  override val typeCode: Byte = 8: Byte
}

/** Any other type is implicitly subtype of this type. */
case object SAny extends SPrimType {
  override type WrappedType = Any
  override val typeCode: Byte = 9: Byte
}

case class SCollection[ElemType <: SType](elemType: ElemType) extends SProduct {
  override type WrappedType = IndexedSeq[Value[ElemType]]
  override val typeCode: TypeCode = SCollection.TypeCode
  override def fields = SCollection.fields

  override def equals(obj: scala.Any) = obj match {
    case that: SCollection[_] => that.elemType == elemType
    case _ => false
  }

  override def hashCode() = (31 + typeCode) * 31 + elemType.hashCode()

  override def toString = s"Array[$elemType]"
}

object SCollection {
  val TypeCode: TypeCode = 80: Byte
  private val tIV = STypeIdent("IV")
  private val tOV = STypeIdent("OV")
  val fields = Seq(
    "size" -> SInt,
    "map" -> SFunc(IndexedSeq(SFunc(tIV, tOV)), SCollection(tOV), Seq(tIV, tOV)),
    "exists" -> SFunc(IndexedSeq(SFunc(tIV, SBoolean)), SBoolean, Seq(tIV)),
    "fold" -> SFunc(IndexedSeq(tIV, SFunc(IndexedSeq(tIV, tIV), tIV)), tIV, Seq(tIV)),
    "forall" -> SFunc(IndexedSeq(SFunc(tIV, SBoolean)), SBoolean, Seq(tIV))
  )
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SCollection[T] = SCollection(elemType)
  def unapply[T <: SType](tCol: SCollection[T]): Option[T] = Some(tCol.elemType)
}

case class SOption[ElemType <: SType](elemType: ElemType) extends SProduct {
  override type WrappedType = Option[Value[ElemType]]
  override val typeCode: TypeCode = SOption.TypeCode
  override def fields = SOption.fields

  override def equals(obj: scala.Any) = obj match {
    case that: SOption[_] => that.elemType == elemType
    case _ => false
  }
  override def hashCode() = (31 + typeCode) * 31 + elemType.hashCode()
  override def toString = s"Option[$elemType]"
}

object SOption {
  val TypeCode: TypeCode = 81: Byte
  private val tT = STypeIdent("T")
  val fields = Seq(
    "isDefined" -> SBoolean,
    "value" -> tT
  )
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SOption[T] = SOption(elemType)
  def unapply[T <: SType](tOpt: SOption[T]): Option[T] = Some(tOpt.elemType)
}

case class SFunc(tDom: IndexedSeq[SType],  tRange: SType, tpeArgs: Seq[STypeIdent] = Nil) extends SType {
  override type WrappedType = Seq[Any] => tRange.WrappedType
  override val typeCode = SFunc.TypeCode
  override def toString = {
    val args = if (tpeArgs.isEmpty) "" else tpeArgs.mkString("[", ",", "]")
    s"$args(${tDom.mkString(",")}) => $tRange"
  }
}

object SFunc {
  val TypeCode = 90: Byte
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(IndexedSeq(tDom), tRange)
}

case class STuple(items: IndexedSeq[SType]) extends SProduct {
  import STuple._
  override type WrappedType = Seq[Any]
  override val typeCode = STuple.TypeCode
  override val fields = {
    val b = new mutable.ArrayBuffer[(String, SType)](items.size)
    var i = 0
    while (i < items.size) {
      b += (componentNames(i) -> items(i))
      i += 1
    }
    b.result
  }
  override def toString = s"(${items.mkString(",")})"
}

object STuple {
  val TypeCode = 100: Byte
  def apply(items: SType*): STuple = STuple(items.toIndexedSeq)
  val componentNames = Range(1, 31).map(i => s"_$i")
}

case class STypeApply(name: String, args: IndexedSeq[SType] = IndexedSeq()) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeApply.TypeCode
}
object STypeApply {
  val TypeCode = 110: Byte
}

case class STypeIdent(name: String) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeIdent.TypeCode
  override def toString = name
}
object STypeIdent {
  val TypeCode = 111: Byte
  implicit def liftString(n: String): STypeIdent = STypeIdent(n)
}

