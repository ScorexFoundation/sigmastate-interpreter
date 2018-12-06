package sigmastate

import java.math.BigInteger

import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.SType.{TypeCode, AnyOps}
import sigmastate.interpreter.CryptoConstants
import sigmastate.utils.Overloading.Overload1
import sigmastate.utils.Extensions._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaTyper
import sigmastate.SCollection._
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import special.collection.Col

import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scalan.meta.ScalanAst.STypeArgAnnotation
import sigmastate.SBoolean.typeCode
import sigmastate.SByte.typeCode
import sigmastate.SSigmaProp.{PropBytes, IsValid}


/** Base type for all AST nodes of sigma lang. */
trait SigmaNode extends Product

/** Base type for all companions of AST nodes of sigma lang. */
trait SigmaNodeCompanion

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

  /** Approximate size of the given value in bytes. It is actual size only for primitive types.*/
  def dataSize(v: SType#WrappedType): Long = sys.error(s"Don't know how to compute dataCost($v) with T = $this")

  def isEmbeddable: Boolean = false

  /** Returns true is dataSize doesn't depend on data value.
    * This is useful for optimizations of calculating sizes of collections. */
  def isConstantSize: Boolean

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
  implicit val typeSigmaProp = SSigmaProp
  implicit val typeBox = SBox

  implicit def typeCollection[V <: SType](implicit tV: V): SCollection[V] = SCollection[V]

  /** All pre-defined types should be listed here. Note, NoType is not listed.
    * Should be in sync with sigmastate.lang.Types.predefTypes. */
  val allPredefTypes = Seq(SBoolean, SByte, SShort, SInt, SLong, SBigInt, SContext, SAvlTree, SGroupElement, SSigmaProp, SBox, SUnit, SAny)
  val typeCodeToType = allPredefTypes.map(t => t.typeCode -> t).toMap

  /** A mapping of object types supporting MethodCall operations. For each serialized typeId this map contains
    * a companion object which can be used to access the list of corresponding methods.
    * NOTE: in the current implementation only monomorphic methods are supported (without type parameters)*/
  val types: Map[Byte, STypeCompanion] = Seq(
    SNumericType, SString, STuple, SGroupElement, SSigmaProp, SContext,
    SAvlTree, SBox
  ).map { t => (t.typeId, t) }.toMap

  implicit class STypeOps(val tpe: SType) {
    def isCollectionLike: Boolean = tpe.isInstanceOf[SCollection[_]]
    def isCollection: Boolean = tpe.isInstanceOf[SCollectionType[_]]
    def isSigmaProp: Boolean = tpe.isInstanceOf[SSigmaProp.type]
    def isFunc : Boolean = tpe.isInstanceOf[SFunc]
    def isTuple: Boolean = tpe.isInstanceOf[STuple]
    def canBeTypedAs(expected: SType): Boolean = (tpe, expected) match {
      case (NoType, _) => true
      case (t1, t2) if t1 == t2 => true
      case (f1: SFunc, f2: SFunc) =>
        val okDom = f1.tDom.size == f2.tDom.size &&
                     f1.tDom.zip(f2.tDom).forall { case (d1, d2) => d1.canBeTypedAs(d2) }
        val okRange = f1.tRange.canBeTypedAs(f2.tRange)
        okDom && okRange
    }
    def isNumType: Boolean = tpe.isInstanceOf[SNumericType]
    def asNumType: SNumericType = tpe.asInstanceOf[SNumericType]
    def asFunc: SFunc = tpe.asInstanceOf[SFunc]
    def asOption[T <: SType]: SOption[T] = tpe.asInstanceOf[SOption[T]]
    def whenFunc[T](action: SFunc => Unit) = if(tpe.isInstanceOf[SFunc]) action(tpe.asFunc)
    def asCollection[T <: SType] = tpe.asInstanceOf[SCollection[T]]
    def classTag[T <: SType#WrappedType]: ClassTag[T] = (tpe match {
      case SBoolean => reflect.classTag[Boolean]
      case SByte => reflect.classTag[Byte]
      case SShort => reflect.classTag[Short]
      case SInt => reflect.classTag[Int]
      case SLong => reflect.classTag[Long]
      case SBigInt => reflect.classTag[BigInteger]
      case SAvlTree => reflect.classTag[AvlTreeData]
      case SGroupElement => reflect.classTag[EcPointType]
      case SSigmaProp => reflect.classTag[SigmaBoolean]
      case SUnit => reflect.classTag[Unit]
      case SBox => reflect.classTag[ErgoBox]
      case SAny => reflect.classTag[Any]
      case t: STuple => reflect.classTag[Array[Any]]
      case tCol: SCollection[a] =>
        val elemType = tCol.elemType
        implicit val ca = elemType.classTag[elemType.WrappedType]
        reflect.classTag[Array[elemType.WrappedType]]
      case _ => sys.error(s"Cannot get ClassTag for type $tpe")
    }).asInstanceOf[ClassTag[T]]
  }

  implicit class AnyOps(x: Any) {
    def asWrappedType: SType#WrappedType = x.asInstanceOf[SType#WrappedType]
  }

  def typeOfData(x: Any): Option[SType] = Option(x match {
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
    case _ => null
  })
}

/** Basic interface for all type companions */
trait STypeCompanion {
  /** Type identifier to use in method serialization */
  def typeId: Byte
  /** List of methods defined for instances of this type. */
  def methods: Seq[SMethod]

  def getMethodById(methodId: Byte): SMethod = {
    methods(methodId - 1)
  }

  def getMethodByName(name: String): SMethod = methods.find(_.name == name).get
}

/** Base trait for all types which have methods (and properties) */
trait SProduct extends SType {
  def ancestors: Seq[SType]
  /** Returns -1 if `method` is not found. */
  def methodIndex(name: String): Int = methods.indexWhere(_.name == name)
  def hasMethod(name: String): Boolean = methodIndex(name) != -1

  /** Returns all the methods of this product type. */
  def methods: Seq[SMethod]

  /** Checks if `this` product has exactly the same methods as `that`. */
  def sameMethods(that: SProduct): Boolean = {
    if (methods.lengthCompare(that.methods.length) != 0) return false
    // imperative to avoid allocation as it is supposed to be used frequently
    for (i <- methods.indices) {
      if (methods(i).name != that.methods(i).name) return false
    }
    true
  }
}

/** Base trait implemented by all generic types (those which has type parameters,
  * e.g. Array[T], Option[T], etc.)*/
trait SGenericType {
  def typeParams: Seq[STypeParam]
  def tparamSubst: Map[String, SType]
}

/** Method info including name, arg type and result type.
  * When stype is SFunc, then tDom - arg type and tRange - result type. */
case class SMethod(objType: STypeCompanion, name: String, stype: SType, methodId: Byte) {
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
  def isConstantSize = true
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

/** Base trait for all primitive types (aka atoms) which don't have internal type items.
  * All primitive types can occupy a reserved interval of codes from 1 to MaxPrimTypeCode. */
trait SPrimType extends SType with SPredefType {
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

/** Marker trait for all numeric types. */
trait SNumericType extends SProduct {
  import SNumericType._
  override def ancestors: Seq[SType] = Nil
  def methods = SNumericType.methods
  def isCastMethod (name: String): Boolean = Seq(ToByte, ToShort, ToInt, ToLong, ToBigInt).contains(name)

  def upcast(i: AnyVal): WrappedType
  def downcast(i: AnyVal): WrappedType

  /** Returns a type which is larger. */
  @inline def max(that: SNumericType): SNumericType =
    if (this.typeIndex > that.typeIndex) this else that

  /** Number of bytes to store values of this type. */
  @inline private def typeIndex: Int = allNumericTypes.indexOf(this)
}
object SNumericType extends STypeCompanion {
  final val allNumericTypes = Array(SByte, SShort, SInt, SLong, SBigInt)
  def typeId: TypeCode = 1: Byte
  val ToByte = "toByte"
  val ToShort = "toShort"
  val ToInt = "toInt"
  val ToLong = "toLong"
  val ToBigInt = "toBigInt"
  val methods = Vector(
    SMethod(this, ToByte, SByte, 1),    // see Downcast
    SMethod(this, ToShort, SShort, 2),  // see Downcast
    SMethod(this, ToInt, SInt, 3),      // see Downcast
    SMethod(this, ToLong, SLong, 4),    // see Downcast
    SMethod(this, ToBigInt, SBigInt, 5) // see Downcast
  )
}

trait SLogical extends SType {
}

case object SBoolean extends SPrimType with SEmbeddable with SLogical with STypeCompanion {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 1: Byte
  override def typeId = typeCode
  def methods: Seq[SMethod] = Nil
  override def mkConstant(v: Boolean): Value[SBoolean.type] = BooleanConstant(v)
  override def dataSize(v: SType#WrappedType): Long = 1
  override def isConstantSize = true
}

case object SByte extends SPrimType with SEmbeddable with SNumericType with STypeCompanion {
  override type WrappedType = Byte
  override val typeCode: TypeCode = 2: Byte //TODO change to 4 after SByteArray is removed
  override def typeId = typeCode
  override def mkConstant(v: Byte): Value[SByte.type] = ByteConstant(v)
  override def dataSize(v: SType#WrappedType): Long = 1
  override def isConstantSize = true
  override def upcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  def downcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case s: Short => s.toByteExact
    case i: Int => i.toByteExact
    case l: Long => l.toByteExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SShort extends SPrimType with SEmbeddable with SNumericType with STypeCompanion {
  override type WrappedType = Short
  override val typeCode: TypeCode = 3: Byte
  override def typeId = typeCode
  override def mkConstant(v: Short): Value[SShort.type] = ShortConstant(v)
  override def dataSize(v: SType#WrappedType): Long = 2
  override def isConstantSize = true
  override def upcast(v: AnyVal): Short = v match {
    case x: Byte => x.toShort
    case x: Short => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Short = v match {
    case s: Short => s
    case i: Int => i.toShortExact
    case l: Long => l.toShortExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SInt extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = Int
  override val typeCode: TypeCode = 4: Byte
  override def mkConstant(v: Int): Value[SInt.type] = IntConstant(v)
  override def dataSize(v: SType#WrappedType): Long = 4
  override def isConstantSize = true
  override def upcast(v: AnyVal): Int = v match {
    case x: Byte => x.toInt
    case x: Short => x.toInt
    case x: Int => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Int = v match {
    case b: Byte => b.toInt
    case s: Short => s.toInt
    case i: Int => i
    case l: Long => l.toIntExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

//todo: make PreservingNonNegativeInt type for registers which value should be preserved?
case object SLong extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 5: Byte
  override def mkConstant(v: Long): Value[SLong.type] = LongConstant(v)
  override def dataSize(v: SType#WrappedType): Long = 8
  override def isConstantSize = true
  override def upcast(v: AnyVal): Long = v match {
    case x: Byte => x.toLong
    case x: Short => x.toLong
    case x: Int => x.toLong
    case x: Long => x
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Long = v match {
    case b: Byte => b.toLong
    case s: Short => s.toLong
    case i: Int => i.toLong
    case l: Long => l
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

case object SBigInt extends SPrimType with SEmbeddable with SNumericType {
  override type WrappedType = BigInteger
  override val typeCode: TypeCode = 6: Byte
  override def mkConstant(v: BigInteger): Value[SBigInt.type] = BigIntConstant(v)
  override def dataSize(v: SType#WrappedType): Long = {
    val bits = v.asInstanceOf[BigInteger].bitLength()
    val bytes =
      if (bits == 0) 0
      else
        ((bits - 1) >> 3) + 1
    bytes
  }
  override def isConstantSize = false
  val Max = CryptoConstants.dlogGroup.order //todo: we use mod q, maybe mod p instead?
  override def upcast(v: AnyVal): BigInteger = v match {
    case x: Byte => BigInteger.valueOf(x.toLong)
    case x: Short => BigInteger.valueOf(x.toLong)
    case x: Int => BigInteger.valueOf(x.toLong)
    case x: Long => BigInteger.valueOf(x)
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): BigInteger = v match {
    case x: Byte => BigInteger.valueOf(x.toLong)
    case x: Short => BigInteger.valueOf(x.toLong)
    case x: Int => BigInteger.valueOf(x.toLong)
    case x: Long => BigInteger.valueOf(x)
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

/** NOTE: this descriptor both type and type companion */
case object SString extends SProduct with STypeCompanion {
  override type WrappedType = String
  override def ancestors: Seq[SType] = Nil
  override val typeCode: TypeCode = 101: Byte
  override def typeId = typeCode
  val methods: Seq[SMethod] = Seq()
  override def mkConstant(v: String): Value[SString.type] = StringConstant(v)
  override def dataSize(v: SType#WrappedType): Long = v.asInstanceOf[String].length
  override def isConstantSize = false
}

/** NOTE: this descriptor both type and type companion */
case object SGroupElement extends SProduct with SPrimType with SEmbeddable with STypeCompanion {
  override type WrappedType = EcPointType
  override val typeCode: TypeCode = 7: Byte
  override def typeId = typeCode
  override val methods = Seq(
    SMethod(this, "isIdentity", SBoolean, 1),
    SMethod(this, "nonce", SCollectionType(SByte), 2)
  )
  override def mkConstant(v: EcPointType): Value[SGroupElement.type] = GroupElementConstant(v)
  override def dataSize(v: SType#WrappedType): Long = 32
  override def isConstantSize = true
  def ancestors = Nil
}

case object SSigmaProp extends SProduct with SPrimType with SEmbeddable with SLogical with STypeCompanion {
  import SType._
  override type WrappedType = SigmaBoolean
  override val typeCode: TypeCode = 8: Byte
  override def typeId = typeCode
  override def mkConstant(v: SigmaBoolean): Value[SSigmaProp.type] = SigmaPropConstant(v)
  override def dataSize(v: SType#WrappedType): Long = v match {
    case ProveDlog(GroupElementConstant(g)) =>
      SGroupElement.dataSize(g.asWrappedType) + 1
    case ProveDiffieHellmanTuple(
          GroupElementConstant(gv), GroupElementConstant(hv),
          GroupElementConstant(uv), GroupElementConstant(vv)) =>
      SGroupElement.dataSize(gv.asWrappedType) * 4 + 1
    case _ => ???
  }
  override def isConstantSize = false
  def ancestors = Nil
  val PropBytes = "propBytes"
  val IsValid = "isValid"
  val methods = Seq(
    SMethod(this, PropBytes, SByteArray, 1),
    SMethod(this, IsValid, SBoolean, 2)
  )
}

case object SContext extends SProduct with SPredefType with STypeCompanion {
  override type WrappedType = ErgoLikeContext
  override val typeCode: TypeCode = 101: Byte
  override def typeId = typeCode
  override def mkConstant(v: ErgoLikeContext): Value[SContext.type] = ContextConstant(v)

  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    val ctx = v.asInstanceOf[ErgoLikeContext]
    val avlSize = SAvlTree.dataSize(ctx.lastBlockUtxoRoot.asWrappedType)
    val inputSize = ctx.boxesToSpend.foldLeft(0L)((acc, b) => acc + b.dataSize)
    val outputSize = ctx.spendingTransaction.outputs.foldLeft(0L)((acc, b) => acc + b.dataSize)
    8L +   // Height
    avlSize + ctx.minerPubkey.length + inputSize + outputSize
  }
  override def isConstantSize = false
  def ancestors = Nil
  val methods = Nil
}

case object SAvlTree extends SProduct with SPredefType with STypeCompanion {
  override type WrappedType = AvlTreeData
  override val typeCode: TypeCode = 100: Byte
  override def typeId = typeCode
  override def mkConstant(v: AvlTreeData): Value[SAvlTree.type] = AvlTreeConstant(v)
  override def dataSize(v: SType#WrappedType): Long = {
    val tree = v.asInstanceOf[AvlTreeData]
    tree.startingDigest.length +
    4 + // keyLength
    tree.valueLengthOpt.fold(0)(_ => 4) +
    tree.maxNumOperations.fold(0)(_ => 4) +
    tree.maxDeletes.fold(0)(_ => 4)
  }
  override def isConstantSize = false
  def ancestors = Nil
  val methods = Nil
}

case object SBox extends SProduct with SPredefType with STypeCompanion {
  override type WrappedType = ErgoBox
  override val typeCode: TypeCode = 99: Byte
  override def typeId = typeCode
  override def mkConstant(v: ErgoBox): Value[SBox.type] = BoxConstant(v)
  override def dataSize(v: SType#WrappedType): Long = {
    val box = v.asInstanceOf[this.WrappedType]
    4 + // box.value
    box.propositionBytes.length +
    box.additionalTokens.length * (32 + 4) +
    box.additionalRegisters.values.map(x => x.tpe.dataSize(x.value)).sum +
    box.transactionId.length +
    2 // box.index
  }
  override def isConstantSize = false
  def ancestors = Nil

  private val tT = STypeIdent("T")
  def registers(idOfs: Int): Seq[SMethod] = {
    (1 to 10).map { i =>
      SMethod(this, s"R$i", SFunc(IndexedSeq(), SOption(tT), Seq(STypeParam(tT))), (idOfs + i).toByte)
    }
  }
  val PropositionBytes = "propositionBytes"
  val Value = "value"
  val Id = "id"
  val Bytes = "bytes"
  val BytesWithNoRef = "bytesWithNoRef"
  val CreationInfo = "creationInfo"
  val methods = Vector(
    SMethod(this, Value, SLong, 1), // see ExtractAmount
    SMethod(this, PropositionBytes, SCollectionType(SByte), 2), // see ExtractScriptBytes
    SMethod(this, Bytes, SCollectionType(SByte), 3), // see ExtractBytes
    SMethod(this, BytesWithNoRef, SCollectionType(SByte), 4), // see ExtractBytesWithNoRef
    SMethod(this, Id, SCollectionType(SByte), 5), // see ExtractId
    SMethod(this, CreationInfo, STuple(SLong, SCollectionType(SByte)), 6) // see ExtractCreationInfo
  ) ++ registers(6)
}

/** The type with single inhabitant value `()` */
case object SUnit extends SPrimType {
  override type WrappedType = Unit
  override val typeCode: Byte = 98: Byte
  override def dataSize(v: SType#WrappedType) = 1
  override def isConstantSize = true
}

/** Any other type is implicitly subtype of this type. */
case object SAny extends SPrimType {
  override type WrappedType = Any
  override val typeCode: Byte = 97: Byte
  override def isConstantSize = false
}

trait SCollection[T <: SType] extends SProduct with SGenericType {
  def elemType: T
  override type WrappedType = Array[T#WrappedType]
  def ancestors = Nil
  override def isConstantSize = false
}

case class SCollectionType[T <: SType](elemType: T) extends SCollection[T] {
  override val typeCode: TypeCode = SCollectionType.CollectionTypeCode

  override def mkConstant(v: Array[T#WrappedType]): Value[this.type] =
    CollectionConstant(v, elemType).asValue[this.type]

  override def dataSize(v: SType#WrappedType): Long = {
    val arr = (v match { case col: Col[_] => col.arr case _ => v}).asInstanceOf[Array[T#WrappedType]]
    val header = 2
    val res =
      if (arr.isEmpty)
        header
      else if (elemType.isConstantSize)
        header + elemType.dataSize(arr(0)) * arr.length
      else
        arr.map(x => elemType.dataSize(x)).sum
    res
  }
  val typeParams = Seq(STypeParam(tIV.name))
  val tparamSubst = Map(tIV.name -> elemType)
  override def methods = SCollection.methods
  override def toString = s"Col[$elemType]"
}

object SCollectionType {
  val CollectionTypeConstrId = 1
  val CollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * CollectionTypeConstrId).toByte
  val NestedCollectionTypeConstrId = 2
  val NestedCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * NestedCollectionTypeConstrId).toByte
}

object SCollection extends STypeCompanion {
  override def typeId = SCollectionType.CollectionTypeCode

  val tIV = STypeIdent("IV")
  val tOV = STypeIdent("OV")
  val SizeMethod = SMethod(this, "size", SInt, 1)
  val GetOrElseMethod = SMethod(this, "getOrElse", SFunc(IndexedSeq(SCollection(tIV), SInt, tIV), tIV, Seq(STypeParam(tIV))), 2)
  val MapMethod = SMethod(this, "map", SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, tOV)), SCollection(tOV), Seq(STypeParam(tIV), STypeParam(tOV))), 3)
  val ExistsMethod = SMethod(this, "exists", SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SBoolean, Seq(STypeParam(tIV))), 4)
  val FoldMethod = SMethod(this, "fold", SFunc(IndexedSeq(SCollection(tIV), tOV, SFunc(IndexedSeq(tOV, tIV), tOV)), tOV, Seq(STypeParam(tIV), STypeParam(tOV))), 5)
  val ForallMethod = SMethod(this, "forall", SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SBoolean, Seq(STypeParam(tIV))), 6)
  val SliceMethod = SMethod(this, "slice", SFunc(IndexedSeq(SCollection(tIV), SInt, SInt), SCollection(tIV), Seq(STypeParam(tIV))), 7)
  val FilterMethod = SMethod(this, "filter", SFunc(IndexedSeq(SCollection(tIV), SFunc(tIV, SBoolean)), SCollection(tIV), Seq(STypeParam(tIV))), 8)
  val AppendMethod = SMethod(this, "append", SFunc(IndexedSeq(SCollection(tIV), SCollection(tIV)), SCollection(tIV), Seq(STypeParam(tIV))), 9)
  val ApplyMethod = SMethod(this, "apply", SFunc(IndexedSeq(SCollection(tIV), SInt), tIV, Seq(STypeParam(tIV))), 10)

  val methods = Seq(
    SizeMethod,
    GetOrElseMethod,
    MapMethod,
    ExistsMethod,
    FoldMethod,
    ForallMethod,
    SliceMethod,
    FilterMethod,
    AppendMethod,
    ApplyMethod
  )
  def apply[T <: SType](elemType: T): SCollection[T] = SCollectionType(elemType)
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SCollection[T] = SCollectionType(elemType)
  def unapply[T <: SType](tCol: SCollection[T]): Option[T] = Some(tCol.elemType)

  type SBooleanArray      = SCollection[SBoolean.type]
  type SByteArray         = SCollection[SByte.type]
  type SShortArray        = SCollection[SShort.type]
  type SIntArray          = SCollection[SInt.type]
  type SLongArray         = SCollection[SLong.type]
  type SBigIntArray       = SCollection[SBigInt.type]
  type SGroupElementArray = SCollection[SGroupElement.type]
  type SBoxArray          = SCollection[SBox.type]
  type SAvlTreeArray      = SCollection[SAvlTree.type]

  val SBooleanArray      = SCollection(SBoolean)
  val SByteArray         = SCollection(SByte)
  val SShortArray        = SCollection(SShort)
  val SIntArray          = SCollection(SInt)
  val SLongArray         = SCollection(SLong)
  val SBigIntArray       = SCollection(SBigInt)
  val SGroupElementArray = SCollection(SGroupElement)
  val SSigmaPropArray    = SCollection(SSigmaProp)
  val SBoxArray          = SCollection(SBox)
  val SAvlTreeArray      = SCollection(SAvlTree)
}

/** Type description of optional values. Instances of `Option`
  *  are either constructed by `Some` or by `None` constructors. */
case class SOption[ElemType <: SType](elemType: ElemType) extends SProduct {
  override type WrappedType = Option[ElemType#WrappedType]
  override val typeCode: TypeCode = SOption.OptionTypeCode
  override def dataSize(v: SType#WrappedType) = {
    val opt = v.asInstanceOf[Option[ElemType#WrappedType]]
    1L + opt.fold(0L)(x => elemType.dataSize(x))
  }
  override def isConstantSize = elemType.isConstantSize
  def ancestors = Nil
  override lazy val methods: Seq[SMethod] = {
    val subst = Map(SOption.tT -> elemType)
    SOption.methods.map { method =>
      method.copy(stype = SigmaTyper.applySubst(method.stype, subst))
    }
  }
  override def toString = s"Option[$elemType]"
}

object SOption extends STypeCompanion {
  val OptionTypeConstrId = 3
  val OptionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionTypeConstrId).toByte
  val OptionCollectionTypeConstrId = 4
  val OptionCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionCollectionTypeConstrId).toByte
  override def typeId = OptionTypeCode

  implicit val optionTypeByte = SOption(SByte)
  implicit val optionTypeByteArray = SOption(SByteArray)
  implicit val optionTypeShort = SOption(SShort)
  implicit val optionTypeInt = SOption(SInt)
  implicit val optionTypeLong = SOption(SLong)
  implicit val optionTypeBigInt = SOption(SBigInt)
  implicit val optionTypeBoolean = SOption(SBoolean)
  implicit val optionTypeAvlTree = SOption(SAvlTree)
  implicit val optionTypeGroupElement = SOption(SGroupElement)
  implicit val optionTypeSigmaProp = SOption(SSigmaProp)
  implicit val optionTypeBox = SOption(SBox)

  implicit def optionTypeCollection[V <: SType](implicit tV: V): SOption[SCollection[V]] = SOption(SCollection[V])

  val IsEmpty = "isEmpty"
  val IsDefined = "isDefined"
  val Get = "get"
  val GetOrElse = "getOrElse"

  private val tT = STypeIdent("T")
  val IsEmptyMethod   = SMethod(this, IsEmpty, SBoolean, 1)
  val IsDefinedMethod = SMethod(this, IsDefined, SBoolean, 2)
  val GetMethod       = SMethod(this, Get, tT, 3)
  val GetOrElseMethod = SMethod(this, GetOrElse, SFunc(IndexedSeq(SOption(tT), tT), tT, Seq(STypeParam(tT))), 4)
  val methods: Seq[SMethod] = Seq(IsEmptyMethod, IsDefinedMethod, GetMethod, GetOrElseMethod)
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SOption[T] = SOption(elemType)
  def unapply[T <: SType](tOpt: SOption[T]): Option[T] = Some(tOpt.elemType)
}

case class STuple(items: IndexedSeq[SType]) extends SCollection[SAny.type] {
  import STuple._
  override val typeCode = STuple.TupleTypeCode

  override def dataSize(v: SType#WrappedType) = {
    val arr = (v match { case col: Col[_] => col.arr case _ => v}).asInstanceOf[Array[Any]]
    assert(arr.length == items.length)
    var sum: Long = 2 // header
    for (i <- arr.indices) {
      sum += items(i).dataSize(arr(i).asInstanceOf[SType#WrappedType])
    }
    sum
  }

  override def elemType: SAny.type = SAny

  override val methods: Seq[SMethod] = {
    val tupleMethods = Array.tabulate(items.size) { i =>
      SMethod(STuple, componentNames(i), items(i), (i + 1).toByte)
    }
    colMethods ++ tupleMethods
  }

  /** Construct tree node Constant for a given data object. */
  override def mkConstant(v: Array[Any]): Value[this.type] =
    Constant[STuple](v, this).asValue[this.type]

  val typeParams = Seq()
  val tparamSubst = Map()

  override def toString = s"(${items.mkString(",")})"
}

object STuple extends STypeCompanion {
  val Pair1TypeConstrId = 5
  val Pair1TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair1TypeConstrId).toByte

  val Pair2TypeConstrId = 6
  val Pair2TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair2TypeConstrId).toByte
  val TripleTypeCode: TypeCode = Pair2TypeCode

  val PairSymmetricTypeConstrId = 7
  val PairSymmetricTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * PairSymmetricTypeConstrId).toByte
  val QuadrupleTypeCode: TypeCode = PairSymmetricTypeCode

  val TupleTypeCode = ((SPrimType.MaxPrimTypeCode + 1) * 8).toByte

  def typeId = TupleTypeCode

  val colMethods = {
    val subst = Map(SCollection.tIV -> SAny)
    SCollection.methods.map { m =>
      m.copy(stype = SigmaTyper.applySubst(m.stype, subst))
    }
  }

  def methods: Seq[SMethod] = sys.error(s"Shouldn't be called.")

  def apply(items: SType*): STuple = STuple(items.toIndexedSeq)
  val componentNames = Array.tabulate(31){ i => s"_${i + 1}" }
}

case class SFunc(tDom: IndexedSeq[SType],  tRange: SType, tpeParams: Seq[STypeParam] = Nil)
      extends SType with SGenericType
{
  override type WrappedType = Seq[Any] => tRange.WrappedType
  override val typeCode = SFunc.FuncTypeCode
  override def isConstantSize = false
  override def toString = {
    val args = if (tpeParams.isEmpty) "" else tpeParams.mkString("[", ",", "]")
    s"$args(${tDom.mkString(",")}) => $tRange"
  }
  override def dataSize(v: SType#WrappedType) = 8L
  import SFunc._
  val typeParams: Seq[STypeParam] = (tDom.zipWithIndex.map { case (t, i) => STypeParam(tD.name + (i + 1)) }) :+ STypeParam(tR.name)
  val tparamSubst = typeParams.zip(tDom).map { case (p, t) => p.ident.name -> t }.toMap + (tR.name -> tRange)

  def getGenericType: SFunc = {
    val ts = typeParams.map(_.ident)
    SFunc(ts.init.toIndexedSeq, ts.last, Nil)
  }
}

object SFunc {
  val tD = STypeIdent("D")
  val tR = STypeIdent("R")
  final val FuncTypeCode: TypeCode = OpCodes.FirstFuncType
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(IndexedSeq(tDom), tRange)
}


case class STypeApply(name: String, args: IndexedSeq[SType] = IndexedSeq()) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeApply.TypeCode
  override def isConstantSize = false
}
object STypeApply {
  val TypeCode = 94: Byte
}

case class STypeIdent(name: String) extends SType {
  require(name.length <= 255, "name is too long")
  override type WrappedType = Any
  override val typeCode = STypeIdent.TypeCode
  override def isConstantSize = false
  override def toString = name
}
object STypeIdent {
  val TypeCode = 102: Byte
  implicit def liftString(n: String): STypeIdent = STypeIdent(n)
}

