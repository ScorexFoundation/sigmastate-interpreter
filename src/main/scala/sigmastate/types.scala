package sigmastate

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import sigmastate.SType.TypeCode
import sigmastate.interpreter.CryptoConstants
import sigmastate.utils.Overloading.Overload1
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaTyper
import sigmastate.SCollection._
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost

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
  val typeCode: SType.TypeCode  //TODO remove, because in general type is encoded by more than one byte

  def dataCost(v: SType#WrappedType): Long = sys.error(s"Don't know how to compute dataCost($v) with T = $this")

  def isEmbeddable: Boolean = false

  /** Elvis operator for types. See https://en.wikipedia.org/wiki/Elvis_operator*/
  def ?:(whenNoType: => SType): SType = if (this == NoType) whenNoType else this

  /** Construct tree node Constant for a given data object. */
  def mkConstant(v: WrappedType): Value[this.type] =
    sys.error(s"Don't know how mkConstant for data value $v with T = $this")
}

object SType {
  /** Representation of type codes used in serialization. */
  type TypeCode = Byte

  implicit val typeByte = SByte
  implicit val typeShort = SShort
  implicit val typeInt = SInt
  implicit val typeLong = SLong
  implicit val typeBigInt = SBigInt
  implicit val typeBoolean = SBoolean
  implicit val typeAvlTree = SAvlTree
  implicit val typeGroupElement = SGroupElement
  implicit val typeBox = SBox

  implicit def typeCollection[V <: SType](implicit tV: V): SCollection[V] = SCollection[V]

  /** All pre-defined types should be listed here. Note, NoType is not listed. */
  val allPredefTypes = Seq(SBoolean, SByte, SShort, SInt, SLong, SBigInt, SAvlTree, SGroupElement, SBox, SUnit, SAny)
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
    def asNumType: SNumericType = tpe.asInstanceOf[SNumericType]
    def asFunc: SFunc = tpe.asInstanceOf[SFunc]
    def classTag[T <: SType#WrappedType]: ClassTag[T] = (tpe match {
      case SBoolean => reflect.classTag[Boolean]
      case SByte => reflect.classTag[Byte]
      case SShort => reflect.classTag[Short]
      case SInt => reflect.classTag[Int]
      case SLong => reflect.classTag[Long]
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
    case _: Boolean => SBoolean
    case _: Byte => SByte
    case _: Short => SShort
    case _: Int => SInt
    case _: Long => SLong
    case _: BigInteger => SBigInt
    case _: CryptoConstants.EcPointType => SGroupElement
    case _: ErgoBox => SBox
    case _: AvlTreeData => SAvlTree
    case _: Unit => SUnit
    case _: Array[Boolean] => SBooleanArray
    case _: Array[Byte] => SByteArray
    case _: Array[Short] => SShortArray
    case _: Array[Int] => SIntArray
    case _: Array[Long] => SLongArray
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

/** Base trait for all pre-defined types which are not necessary primitive (e.g. Box, AvlTree).
  */
trait SPredefType extends SType {
}

/** Base trait for all embeddable types.
  */
trait SEmbeddable extends SType {
  override def isEmbeddable: Boolean = true
  /** Type code of embeddable type can be combined with code of type constructor.
    * Resulting code can be serialized. This simple convention allows to save space for most frequently used types.
    * See TypeSerializer */
  @inline final def embedIn(typeConstrId: Byte): Byte = (typeConstrId + this.typeCode).toByte
}

/** Base trait for all primitive types which don't have internal type items (aka atoms).
  * All primitive types can occupy a reserved interval of codes from 1 to MaxPrimTypeCode. */
trait SPrimType extends SType with SPredefType {
}

/** Marker trait for all numeric types. */
trait SNumericType extends SType {
  import SNumericType._
  def upcast(i: AnyVal): WrappedType

  /** Returns a type which is larger. */
  @inline def max(that: SNumericType): SNumericType =
    if (this.typeIndex > that.typeIndex) this else that

  /** Number of bytes to store values of this type. */
  @inline private def typeIndex: Int = allNumericTypes.indexOf(this)
}
object SNumericType {
  final val allNumericTypes = Array(SByte, SShort, SInt, SLong, SBigInt)
}

/** Primitive type recognizer to pattern match on TypeCode */
object SPrimType {
  def unapply(tc: TypeCode): Option[SType] = SType.typeCodeToType.get(tc)
  def unapply(t: SType): Option[SType] = SType.allPredefTypes.find(_ == t)

  /** Type code of the last valid prim type so that (1 to LastPrimTypeCode) is a range of valid codes. */
  final val LastPrimTypeCode: Byte = 9: Byte

  /** Upper limit of the interval of valid type codes for primitive types */
  final val MaxPrimTypeCode: Byte = 11: Byte
  final val PrimRange: Byte = (MaxPrimTypeCode + 1).toByte
}

case object SBoolean extends SPrimType with SEmbeddable {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 1: Byte
  override def mkConstant(v: Boolean): Value[SBoolean.type] = BooleanConstant.fromBoolean(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.BooleanConstantDeclaration
}

case object SByte extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = Byte
  override val typeCode: TypeCode = 2: Byte //TODO change to 4 after SByteArray is removed
  override def mkConstant(v: Byte): Value[SByte.type] = ByteConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.ByteConstantDeclaration
  override def upcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SShort extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = Short
  override val typeCode: TypeCode = 3: Byte
  override def mkConstant(v: Short): Value[SShort.type] = ShortConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.ShortConstantDeclaration
  override def upcast(v: AnyVal): Short = v match {
    case x: Byte => x.toShort
    case x: Short => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SInt extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = Int
  override val typeCode: TypeCode = 4: Byte
  override def mkConstant(v: Int): Value[SInt.type] = IntConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.IntConstantDeclaration
  override def upcast(v: AnyVal): Int = v match {
    case x: Byte => x.toInt
    case x: Short => x.toInt
    case x: Int => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SLong extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 5: Byte
  override def mkConstant(v: Long): Value[SLong.type] = LongConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.LongConstantDeclaration
  override def upcast(v: AnyVal): Long = v match {
    case x: Byte => x.toLong
    case x: Short => x.toLong
    case x: Int => x.toLong
    case x: Long => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
}

case object SBigInt extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = BigInteger
  override val typeCode: TypeCode = 6: Byte
  override def mkConstant(v: BigInteger): Value[SBigInt.type] = BigIntConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.BigIntConstantDeclaration
  val Max = CryptoConstants.dlogGroup.order //todo: we use mod q, maybe mod p instead?
  override def upcast(v: AnyVal): BigInteger = v match {
    case x: Byte => BigInteger.valueOf(x.toLong)
    case x: Short => BigInteger.valueOf(x.toLong)
    case x: Int => BigInteger.valueOf(x.toLong)
    case x: Long => BigInteger.valueOf(x)
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
}

case object SGroupElement extends SProduct with SPrimType with SEmbeddable {
  override type WrappedType = EcPointType
  override val typeCode: TypeCode = 7: Byte
  override def mkConstant(v: EcPointType): Value[SGroupElement.type] = GroupElementConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.GroupElementConstantDeclaration
  def ancestors = Nil
  val fields = Seq(
    "isIdentity" -> SBoolean,
    "nonce" -> SByteArray
  )
}

case object SAvlTree extends SProduct with SPredefType {
  override type WrappedType = AvlTreeData
  override val typeCode: TypeCode = 100: Byte
  override def mkConstant(v: AvlTreeData): Value[SAvlTree.type] = AvlTreeConstant(v)
  override def dataCost(v: SType#WrappedType): Long = Cost.AvlTreeConstantDeclaration
  def ancestors = Nil
  val fields = Nil
}

case object SBox extends SProduct with SPredefType {
  override type WrappedType = ErgoBox
  override val typeCode: TypeCode = 99: Byte
  override def mkConstant(v: ErgoBox): Value[SBox.type] = BoxConstant(v)
  override def dataCost(v: SType#WrappedType): Long = v.asInstanceOf[this.WrappedType].cost

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
    Value            -> SLong,        // see ExtractAmount
    PropositionBytes -> SByteArray,  // see ExtractScriptBytes
    Bytes            -> SByteArray,  // see ExtractBytes
    BytesWithNoRef   -> SByteArray,  // see ExtractBytesWithNoRef
    Id               -> SByteArray   // see ExtractId
  ) ++ registers()
}

/** The type with single inhabitant value `()` */
case object SUnit extends SPrimType {
  override type WrappedType = Unit
  override val typeCode: Byte = 98: Byte
}

/** Any other type is implicitly subtype of this type. */
case object SAny extends SPrimType {
  override type WrappedType = Any
  override val typeCode: Byte = 97: Byte
}

case class SCollection[T <: SType](elemType: T) extends SProduct {
  override type WrappedType = Array[T#WrappedType]
  override val typeCode: TypeCode = SCollection.CollectionTypeCode

  override def mkConstant(v: Array[T#WrappedType]): Value[this.type] =
    CollectionConstant(v, elemType).asValue[this.type]

  override def dataCost(v: SType#WrappedType): Long =
    ((v.asInstanceOf[Array[T#WrappedType]].length / 1024) + 1) * Cost.ByteArrayPerKilobyte

  def ancestors = Nil
  override def fields = SCollection.fields
  override def toString = s"Array[$elemType]"
}

object SCollection {
  val CollectionTypeConstrId = 1
  val CollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * CollectionTypeConstrId).toByte
  val NestedCollectionTypeConstrId = 2
  val NestedCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * NestedCollectionTypeConstrId).toByte

  private val tIV = STypeIdent("IV")
  private val tOV = STypeIdent("OV")
  val fields = Seq(
    "size" -> SInt,
    "getOrElse" -> SFunc(IndexedSeq(SCollection(tIV), SInt, tIV), tIV, Seq(tIV)),
    "map" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, tOV)), SCollection(tOV), Seq(tIV, tOV)),
    "exists" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SBoolean, Seq(tIV)),
    "fold" -> SFunc(IndexedSeq(SCollection(tIV), tIV, SFunc(IndexedSeq(tIV, tIV), tIV)), tIV, Seq(tIV)),
    "forall" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SBoolean, Seq(tIV)),
    "slice" -> SFunc(IndexedSeq(SCollection(tIV), SInt, SInt), SCollection(tIV), Seq(tIV)),
    "where" -> SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SCollection(tIV), Seq(tIV))
  )
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SCollection[T] = SCollection(elemType)
  def unapply[T <: SType](tCol: SCollection[T]): Option[T] = Some(tCol.elemType)

  type SBooleanArray      = SCollection[SBoolean.type]
  type SByteArray         = SCollection[SByte.type]
  type SShortArray          = SCollection[SShort.type]
  type SIntArray          = SCollection[SInt.type]
  type SLongArray          = SCollection[SLong.type]
  type SBigIntArray       = SCollection[SBigInt.type]
  type SGroupElementArray = SCollection[SGroupElement.type]
  type SBoxArray          = SCollection[SBox.type]
  type SAvlTreeArray      = SCollection[SAvlTree.type]

  val SBooleanArray      = SCollection(SBoolean)
  val SByteArray         = SCollection(SByte)
  val SShortArray        = SCollection(SShort)
  val SIntArray          = SCollection(SInt)
  val SLongArray          = SCollection(SLong)
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
  val OptionTypeConstrId = 3
  val OptionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionTypeConstrId).toByte
  val OptionCollectionTypeConstrId = 4
  val OptionCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionCollectionTypeConstrId).toByte

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

case class STuple(items: IndexedSeq[SType]) extends SProduct {
  import STuple._
  override type WrappedType = Seq[Any]
  override val typeCode = STuple.TupleTypeCode

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
  val Pair1TypeConstrId = 5
  val Pair1TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair1TypeConstrId).toByte

  val Pair2TypeConstrId = 6
  val Pair2TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair2TypeConstrId).toByte
  val TripleTypeCode: TypeCode = Pair2TypeCode

  val PairSymmetricTypeConstrId = 7
  val PairSymmetricTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * PairSymmetricTypeConstrId).toByte
  val QuadrupleTypeCode: TypeCode = PairSymmetricTypeCode

  val TupleTypeCode = ((SPrimType.MaxPrimTypeCode + 1) * 8).toByte

  def apply(items: SType*): STuple = STuple(items.toIndexedSeq)
  val componentNames = Range(1, 31).map(i => s"_$i")
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
  final val FuncTypeCode: TypeCode = OpCodes.FirstFuncType
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(IndexedSeq(tDom), tRange)
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

