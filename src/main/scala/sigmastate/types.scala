package sigmastate

import java.math.BigInteger

import sigmastate.SType.TypeCode
import sigmastate.interpreter.GroupSettings
import sigmastate.utils.Overloading.Overload1
import sigmastate.utxo.ErgoBox
import sigmastate.Values._
import sigmastate.lang.SigmaTyper
import sigmastate.SCollection._
import sigmastate.interpreter.GroupSettings.EcPointType

import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag


/** Base type for all AST nodes of sigma lang. */
trait SigmaNode extends Product

/** Every type descriptor is a tree represented by nodes in SType hierarchy.
  * In order to extend type family:
  * - Implement concrete class derived from SType
  * - Implement serializer (see SCollectionSerializer) and register it in STypeSerializer.table
  * Each SType is serialized to array of bytes by:
  * - emitting typeCode of each node (see special case for collections below)
  * - then recursively serializing subtrees from left to right on each level
  * - for each collection of primitive type there is special type code to emit single byte instead of two bytes
  * Types code intervals
  * - (1 .. MaxPrimTypeCode)  // primitive types
  * - (CollectionTypeCode .. CollectionTypeCode + MaxPrimTypeCode) // collections of primitive types
  * - (MaxCollectionTypeCode ..)  // Other types
  * Collection of non-primitive type is serialized as (CollectionTypeCode, serialize(elementType))
  * */
sealed trait SType extends SigmaNode {
  type WrappedType
  val typeCode: SType.TypeCode

  def isPrimitive: Boolean = SType.allPredefTypes.contains(this)

  /** Elvis operator for types. See https://en.wikipedia.org/wiki/Elvis_operator*/
  def ?:(whenNoType: => SType): SType = if (this == NoType) whenNoType else this
}

object SType {
  /** Representation of type codes used in serialization. */
  type TypeCode = Byte

  implicit val typeByte = SByte
  implicit val typeInt = SInt
  implicit val typeBigInt = SBigInt
  implicit val typeBoolean = SBoolean
  implicit val typeAvlTree = SAvlTree
  implicit val typeGroupElement = SGroupElement
  implicit val typeBox = SBox

  implicit def typeCollection[V <: SType](implicit tV: V): SCollection[V] = SCollection[V]

  /** All primitive types should be listed here. Note, NoType is not primitive type. */
  val allPredefTypes = Seq(SByte, SInt, SBigInt, SBoolean, SAvlTree, SGroupElement, SBox, SUnit, SAny)
  val typeCodeToType = allPredefTypes.map(t => t.typeCode -> t).toMap

  implicit class STypeOps(val tpe: SType) {
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
    def classTag[T <: SType#WrappedType]: ClassTag[T] = (tpe match {
      case SByte => reflect.classTag[Byte]
      case SInt => reflect.classTag[Long]
      case SBoolean => reflect.classTag[Boolean]
      case SBigInt => reflect.classTag[BigInteger]
      case SAvlTree => reflect.classTag[AvlTreeData]
      case SGroupElement => reflect.classTag[EcPointType]
      case SUnit => reflect.classTag[Unit]
      case SBox => reflect.classTag[ErgoBox]
      case SAny => reflect.classTag[Any]
      case _ => sys.error(s"Cannot get ClassTag for type $tpe")
    }).asInstanceOf[ClassTag[T]]
  }

  def typeOfData(x: Any): SType = x match {
    case _: Byte => SByte
    case _: Long => SInt
    case _: Boolean => SBoolean
    case _: BigInteger => SBigInt
    case _: GroupSettings.EcPointType => SGroupElement
    case _: ErgoBox => SBox
    case _: AvlTreeData => SAvlTree
    case _: Unit => SUnit
    case _: Array[Byte] => SByteArray
    case _: Array[Long] => SIntArray
    case _: Array[Boolean] => SBooleanArray
    case _: Array[BigInteger] => SBigIntArray
    case _: Array[EcPointType] => SGroupElementArray
    case _: Array[ErgoBox] => SBoxArray
    case _: Array[AvlTreeData] => SAvlTreeArray
    case v: SValue => v.tpe
    case _ => sys.error(s"Don't know how to return SType for $x: ${x.getClass}")
  }

}

/** Base trait for all types which have fields (aka properties) */
trait SProduct extends SType {
  def ancestors: Seq[SType]
  /** Returns -1 if `field` is not found. */
  def fieldIndex(field: String): Int = fields.indexWhere(_._1 == field)
  def hasField(field: String) = fieldIndex(field) != -1

  /** Returns all the fields of this product type. */
  def fields: Seq[(String, SType)]

  /** Checks if `this` product has exactly the same fields as `that`. */
  def sameFields(that: SProduct): Boolean = {
    if (fields.lengthCompare(that.fields.length) != 0) return false
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
  val typeCode = 0: Byte
}

/** Base trait for all primitive types which don't have internal type items (aka atoms).
  * All primitive types can occupy a reserved interval of codes from 1 to MaxPrimTypeCode. */
trait SPrimType extends SType {
}

/** Primitive type recognizer to pattern match on TypeCode */
object SPrimType {
  def unapply(tc: TypeCode): Option[SType] = SType.typeCodeToType.get(tc)
  def unapply(t: SType): Option[SType] = SType.allPredefTypes.find(_ == t)

  /** Type code of the last valid prim type so that (1 to LastPrimTypeCode) is a range of valid codes. */
  final val LastPrimTypeCode: Byte = 9: Byte

  /** Upper limit of the interval of valid type codes for primitive types */
  final val MaxPrimTypeCode: Byte = 19: Byte
}

case object SByte extends SPrimType {
  override type WrappedType = Byte
  override val typeCode: TypeCode = 1: Byte //TODO change to 4 after SByteArray is removed
}

case object SBoolean extends SPrimType {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 2: Byte
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SInt extends SPrimType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 3: Byte
}

case object SBigInt extends SPrimType {
  override type WrappedType = BigInteger
  override val typeCode: TypeCode = 4: Byte

  val Max = GroupSettings.dlogGroup.order //todo: we use mod q, maybe mod p instead?
}

case object SGroupElement extends SProduct with SPrimType {
  override type WrappedType = GroupSettings.EcPointType
  override val typeCode: TypeCode = 5: Byte
  def ancestors = Nil
  val fields = Seq(
    "isIdentity" -> SBoolean,
    "nonce" -> SByteArray
  )
}

case object SAvlTree extends SProduct with SPrimType {
  override type WrappedType = AvlTreeData
  override val typeCode: TypeCode = 6: Byte
  def ancestors = Nil
  val fields = Nil
}

case object SBox extends SProduct with SPrimType {
  override type WrappedType = ErgoBox
  override val typeCode: TypeCode = 7: Byte

  def ancestors = Nil

  private val tT = STypeIdent("T")
  def registers(): Seq[(String, SType)] = {
    (1 to 10).map(i => s"R$i" -> SFunc(IndexedSeq(), SOption(tT), Seq(tT)))
  }
  val PropositionBytes = "propositionBytes"
  val Value = "value"
  val Id = "id"
  val Bytes = "bytes"
  val BytesWithNoRef = "bytesWithNoRef"
  val fields = Vector(
    Value            -> SInt,        // see ExtractAmount
    PropositionBytes -> SByteArray,  // see ExtractScriptBytes
    Bytes            -> SByteArray,  // see ExtractBytes
    BytesWithNoRef   -> SByteArray,  // see ExtractBytesWithNoRef
    Id               -> SByteArray   // see ExtractId
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

case class SCollection[T <: SType](elemType: T) extends SProduct {
  override type WrappedType = Array[T#WrappedType]
  override val typeCode: TypeCode = SCollection.CollectionTypeCode
  def ancestors = Nil
  override def fields = SCollection.fields
  override def toString = s"Array[$elemType]"
}

object SCollection {
  val CollectionTypeCode: TypeCode = (SPrimType.MaxPrimTypeCode + 1).toByte
  val MaxCollectionTypeCode: TypeCode = (CollectionTypeCode + SPrimType.MaxPrimTypeCode).toByte

  private val tIV = STypeIdent("IV")
  private val tOV = STypeIdent("OV")
  val fields = Seq(
    "size" -> SInt,
    "map" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, tOV)), SCollection(tOV), Seq(tIV, tOV)),
    "exists" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SBoolean, Seq(tIV)),
    "fold" -> SFunc(IndexedSeq(SCollection(tIV), tIV, SFunc(IndexedSeq(tIV, tIV), tIV)), tIV, Seq(tIV)),
    "forall" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SBoolean, Seq(tIV)),
    "slice" -> SFunc(IndexedSeq(SCollection(tIV), SInt, SInt), SCollection(tIV), Seq(tIV)),
    "where" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SCollection(tIV), Seq(tIV))
  )
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SCollection[T] = SCollection(elemType)
  def unapply[T <: SType](tCol: SCollection[T]): Option[T] = Some(tCol.elemType)

  type SByteArray         = SCollection[SByte.type]
  type SIntArray          = SCollection[SInt.type]
  type SBooleanArray      = SCollection[SBoolean.type]
  type SBigIntArray       = SCollection[SBigInt.type]
  type SGroupElementArray = SCollection[SGroupElement.type]
  type SBoxArray          = SCollection[SBox.type]
  type SAvlTreeArray      = SCollection[SAvlTree.type]

  val SByteArray         = SCollection(SByte)
  val SIntArray          = SCollection(SInt)
  val SBooleanArray      = SCollection(SBoolean)
  val SBigIntArray       = SCollection(SBigInt)
  val SGroupElementArray = SCollection(SGroupElement)
  val SBoxArray          = SCollection(SBox)
  val SAvlTreeArray      = SCollection(SAvlTree)
}

/** Type description of optional values. Instances of `Option`
  *  are either constructed by `Some` or by `None` constructors. */
case class SOption[ElemType <: SType](elemType: ElemType) extends SProduct {
  override type WrappedType = Option[Value[ElemType]]
  override val typeCode: TypeCode = SOption.OptionTypeCode
  def ancestors = Nil
  override lazy val fields = {
    val subst = Map(SOption.tT -> elemType)
    SOption.fields.map { case (n, t) => (n, SigmaTyper.applySubst(t, subst)) }
  }
  override def toString = s"Option[$elemType]"
}

object SOption {
  val OptionTypeCode: TypeCode = (MaxCollectionTypeCode + 1.toByte).toByte
  val MaxOptionTypeCode: TypeCode = (OptionTypeCode + SPrimType.MaxPrimTypeCode).toByte
  private[sigmastate] def createFields(tArg: STypeIdent) =
    Seq(
      "isDefined" -> SBoolean,
      "value" -> tArg,
      "valueOrElse" -> SFunc(IndexedSeq(SOption(tArg), tArg), tArg, Seq(tT))
    )
  private val tT = STypeIdent("T")
  val fields: Seq[(String, SType)] = createFields(tT)
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SOption[T] = SOption(elemType)
  def unapply[T <: SType](tOpt: SOption[T]): Option[T] = Some(tOpt.elemType)
}

case class SFunc(tDom: IndexedSeq[SType],  tRange: SType, tpeArgs: Seq[STypeIdent] = Nil) extends SType {
  override type WrappedType = Seq[Any] => tRange.WrappedType
  override val typeCode = SFunc.FuncTypeCode
  override def toString = {
    val args = if (tpeArgs.isEmpty) "" else tpeArgs.mkString("[", ",", "]")
    s"$args(${tDom.mkString(",")}) => $tRange"
  }
}

object SFunc {
  final val FuncTypeCode: TypeCode = (SOption.MaxOptionTypeCode + 1.toByte).toByte
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(IndexedSeq(tDom), tRange)
}

case class STuple(items: IndexedSeq[SType]) extends SProduct {
  import STuple._
  override type WrappedType = Seq[Any]
  override val typeCode = STuple.TypeCode

  def ancestors = Nil

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
  val TypeCode = 93: Byte
  def apply(items: SType*): STuple = STuple(items.toIndexedSeq)
  val componentNames = Range(1, 31).map(i => s"_$i")
}

case class STypeApply(name: String, args: IndexedSeq[SType] = IndexedSeq()) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeApply.TypeCode
}
object STypeApply {
  val TypeCode = 94: Byte
}

case class STypeIdent(name: String) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeIdent.TypeCode
  override def toString = name
}
object STypeIdent {
  val TypeCode = 95: Byte
  implicit def liftString(n: String): STypeIdent = STypeIdent(n)
}

