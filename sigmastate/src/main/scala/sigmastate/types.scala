package sigmastate

import java.math.BigInteger
import java.util

import org.ergoplatform._
import org.ergoplatform.validation._
import scalan.{RType, Nullable}
import scalan.RType.GeneralType
import sigmastate.SType.{TypeCode, AnyOps}
import sigmastate.interpreter.CryptoConstants
import sigmastate.utils.Overloading.Overload1
import scalan.util.Extensions._
import sigmastate.SBigInt.MaxSizeInBytes
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.{SigmaBuilder, SigmaTyper}
import sigmastate.SCollection._
import sigmastate.interpreter.CryptoConstants.{EcPointType, hashLength}
import sigmastate.serialization.OpCodes
import special.collection.Coll
import special.sigma._
import sigmastate.eval.RuntimeCosting

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import sigmastate.SMethod.MethodCallIrBuilder
import sigmastate.utxo.ByIndex
import sigmastate.utxo.ExtractCreationInfo
import sigmastate.utxo._
import special.sigma.{Header, Box, SigmaProp, AvlTree, SigmaDslBuilder, PreHeader}
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.eval.Evaluation.stypeToRType
import sigmastate.eval._
import sigmastate.lang.exceptions.SerializerException

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
  /** Type code used in serialization of SType values.
    * @see TypeSerializer
    */
  val typeCode: SType.TypeCode

  /** Approximate size of the given value in bytes. It is actual size only for primitive types.*/
  def dataSize(v: SType#WrappedType): Long

  /** Returns true if this type embeddable, i.e. a type that can be combined with type
    * constructor for optimized encoding. For each embeddable type `T`, and type
    * constructor `C`, the type `C[T]` can be represented by a single byte.
    * @see [[sigmastate.serialization.TypeSerializer]]
    */
  def isEmbeddable: Boolean = false

  /** Returns true if dataSize doesn't depend on data value.
    * This is useful for optimizations of calculating sizes of collections. */
  def isConstantSize: Boolean

  /** Elvis operator for types. See https://en.wikipedia.org/wiki/Elvis_operator*/
  def ?:(whenNoType: => SType): SType = if (this == NoType) whenNoType else this

  def withSubstTypes(subst: Map[STypeVar, SType]): SType =
    if (subst.isEmpty) this
    else
      SigmaTyper.applySubst(this, subst)

  /** Returns parsable type term string of the type described by this type descriptor.
    * For every type it should be inverse to SigmaTyper.parseType.
    * This is default fallback implementation, should be overriden if it
    * is not correct for a particular type. */
  def toTermString: String = {
    val t = Evaluation.stypeToRType(this)
    t.name
  }
}

object SType {
  /** Representation of type codes used in serialization. */
  type TypeCode = Byte

  val DummyValue = 0.asWrappedType

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

  implicit val SigmaBooleanRType: RType[SigmaBoolean] = RType.fromClassTag(classTag[SigmaBoolean])
  implicit val ErgoBoxRType: RType[ErgoBox] = RType.fromClassTag(classTag[ErgoBox])
  implicit val ErgoBoxCandidateRType: RType[ErgoBoxCandidate] = RType.fromClassTag(classTag[ErgoBoxCandidate])
  implicit val AvlTreeDataRType: RType[AvlTreeData] = new GeneralType(classTag[AvlTreeData]) {
    override def isConstantSize: Boolean = true
  }
  implicit val ErgoLikeContextRType: RType[ErgoLikeContext] = RType.fromClassTag(classTag[ErgoLikeContext])

  /** Named type variables and parameters used in generic types and method signatures.
    * Generic type terms like `(Coll[IV],(IV) => Boolean) => Boolean` are used to represent
    * method types of `Coll`` and `Option`` types. Each such type is an instance of [[SFunc]].
    * To represent variables (such as `IV` in the example above) [[STypeVar]] instances
    * are used.
    *
    * Generic types are not supported by ErgoTree serialization format and STypeVars are
    * used internally and never serialized (there is no serializer for STypeVar).
    * Thus the usage of type variables is limited.
    *
    * All necessary type variables can be declared in advance and reused across all code
    * base. This allows to avoid allocation of many duplicates and also improve
    * performance of SType values.
    */
  val tT = STypeVar("T")
  val tR = STypeVar("R")
  val tK = STypeVar("K")
  val tL = STypeVar("L")
  val tO = STypeVar("O")
  val tD = STypeVar("D")
  val tV = STypeVar("V")
  val tIV = STypeVar("IV")
  val tOV = STypeVar("OV")

  val paramT = STypeParam(tT)
  val paramR = STypeParam(tR)
  val paramIV = STypeParam(tIV)
  val paramOV = STypeParam(tOV)

  val IndexedSeqOfT1: IndexedSeq[SType] = Array(SType.tT)
  val IndexedSeqOfT2: IndexedSeq[SType] = Array(SType.tT, SType.tT)
  val EmptyArray = Array.empty[SType]
  val EmptySeq: IndexedSeq[SType] = EmptyArray

  /** All pre-defined types should be listed here. Note, NoType is not listed.
    * Should be in sync with sigmastate.lang.Types.predefTypes. */
  val allPredefTypes = Seq(SBoolean, SByte, SShort, SInt, SLong, SBigInt, SContext, SGlobal, SHeader, SPreHeader, SAvlTree, SGroupElement, SSigmaProp, SString, SBox, SUnit, SAny)
  val typeCodeToType = allPredefTypes.map(t => t.typeCode -> t).toMap

  /** A mapping of object types supporting MethodCall operations. For each serialized typeId this map contains
    * a companion object which can be used to access the list of corresponding methods.
    * NOTE: in the current implementation only monomorphic methods are supported (without type parameters)*/
  val types: Map[Byte, STypeCompanion] = Seq(
    SBoolean, SNumericType, SString, STuple, SGroupElement, SSigmaProp, SContext, SGlobal, SHeader, SPreHeader,
    SAvlTree, SBox, SOption, SCollection, SBigInt
  ).map { t => (t.typeId, t) }.toMap

  implicit class STypeOps(val tpe: SType) extends AnyVal {
    def isCollectionLike: Boolean = tpe.isInstanceOf[SCollection[_]]
    def isCollection: Boolean = tpe.isInstanceOf[SCollectionType[_]]
    def isOption: Boolean = tpe.isInstanceOf[SOption[_]]
    def isBox: Boolean = tpe.isInstanceOf[SBox.type]
    def isGroupElement: Boolean = tpe.isInstanceOf[SGroupElement.type]
    def isSigmaProp: Boolean = tpe.isInstanceOf[SSigmaProp.type]
    def isAvlTree: Boolean = tpe.isInstanceOf[SAvlTree.type]
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

    /** Returns true if this type is numeric (Byte, Short, etc.)
      * @see [[sigmastate.SNumericType]]
      */
    def isNumType: Boolean = tpe.isInstanceOf[SNumericType]

    /** Returns true if this type is either numeric (Byte, Short, etc.) or is NoType.
      * @see [[sigmastate.SNumericType]]
      */
    def isNumTypeOrNoType: Boolean = isNumType || tpe == NoType

    def asNumType: SNumericType = tpe.asInstanceOf[SNumericType]
    def asFunc: SFunc = tpe.asInstanceOf[SFunc]
    def asProduct: SProduct = tpe.asInstanceOf[SProduct]
    def asTuple: STuple = tpe.asInstanceOf[STuple]
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
      case SAvlTree => reflect.classTag[AvlTree]
      case SGroupElement => reflect.classTag[EcPointType]
      case SSigmaProp => reflect.classTag[SigmaBoolean]
      case SUnit => reflect.classTag[Unit]
      case SBox => reflect.classTag[ErgoBox]
      case SAny => reflect.classTag[Any]
      case opt: SOption[a] => reflect.classTag[Option[a]]
      case _: STuple => reflect.classTag[Array[Any]]
      case tColl: SCollection[a] =>
        val elemType = tColl.elemType
        implicit val ca = elemType.classTag[elemType.WrappedType]
        reflect.classTag[Array[elemType.WrappedType]]
      case _ => sys.error(s"Cannot get ClassTag for type $tpe")
    }).asInstanceOf[ClassTag[T]]
  }

  implicit class AnyOps(val x: Any) extends AnyVal {
    def asWrappedType: SType#WrappedType = x.asInstanceOf[SType#WrappedType]
  }
}

/** Basic interface for all type companions.
  * This is necessary to make distinction between concrete type descriptor of a type like Coll[Int]
  * and generic descriptor of Coll[T] type constructor.
  * Some simple types like Int, GroupElement inherit from both SType and STypeCompanion.
  * @see SInt, SGroupElement, SType
  */
trait STypeCompanion {

  /** Type identifier to use in method serialization */
  def typeId: Byte

  def typeName: String = {
    this match {
      case t: SType =>
        val rtype = stypeToRType(t)
        rtype.name
      case _ => this.getClass.getSimpleName.replace("$", "")
    }
  }

  /** List of methods defined for instances of this type. */
  def methods: Seq[SMethod]

  lazy val _methodsMap: Map[Byte, Map[Byte, SMethod]] = methods
    .groupBy(_.objType.typeId)
    .map { case (typeId, ms) => (typeId -> ms.map(m => m.methodId -> m).toMap) }

  /** Lookup method by its id in this type. */
  @inline def getMethodById(methodId: Byte): Option[SMethod] =
    _methodsMap.get(typeId)
        .flatMap(ms => ms.get(methodId))

  /** Lookup method in this type by method's id or throw ValidationException.
    * This method can be used in trySoftForkable section to either obtain valid method
    * or catch ValidatioinException which can be checked for soft-fork condition.
    * It delegate to getMethodById to lookup method.
    * @see getMethodById
    */
  def methodById(methodId: Byte): SMethod = {
    ValidationRules.CheckAndGetMethod(this, methodId)
  }

  def getMethodByName(name: String): SMethod = methods.find(_.name == name).get

  def coster: Option[CosterFactory] = None
}

trait MethodByNameUnapply extends STypeCompanion {
  def unapply(methodName: String): Option[SMethod] = methods.find(_.name == methodName)
}

/** Base trait for all types which have methods (and properties) */
trait SProduct extends SType {
  /** Returns -1 if `method` is not found. */
  def methodIndex(name: String): Int = methods.indexWhere(_.name == name)
  def hasMethod(name: String): Boolean = methodIndex(name) != -1

  /** This method should be overriden in derived classes to add new methods in addition to inherited.
    * Typical override: `super.getMethods() ++ Seq(m1, m2, m3)` */
  protected def getMethods(): Seq[SMethod] = Nil

  /** Returns all the methods of this type. */
  lazy val methods: Seq[SMethod] = {
    val ms = getMethods()
    assert(ms.map(_.name).distinct.length == ms.length, s"Duplicate method names in $this")
    ms.groupBy(_.objType).foreach { case (comp, ms) =>
      assert(ms.map(_.methodId).distinct.length == ms.length, s"Duplicate method ids in $comp: $ms")
    }
    ms
  }

  def method(methodName: String): Option[SMethod] = methods.find(_.name == methodName)
}

/** Base trait implemented by all generic types (those which has type parameters,
  * e.g. Coll[T], Option[T], etc.)*/
trait SGenericType {
  /** Type parameters of this generic type. */
  def typeParams: Seq[STypeParam]
}

/** Special interface to access CostingHandler.
  * Each `STypeCompanion.coster` property optionally defines an instance of this interface to provide
  * access to Coster for its methods. If not specified (which is default) then generic costing mechanism
  * is not used for methods of the corresponding type. (e.g. SInt, SLong)*/
trait CosterFactory {
  def apply[Ctx <: RuntimeCosting](IR: Ctx): IR.CostingHandler[_]
}

/** An instance of this class is created in each `STypeCompaion.coster` property implementation.
  * @see SBox, SContext
  */
case class Coster(selector: RuntimeCosting => RuntimeCosting#CostingHandler[_]) extends CosterFactory {
   def apply[Ctx <: RuntimeCosting](IR: Ctx): IR.CostingHandler[_] = selector(IR).asInstanceOf[IR.CostingHandler[_]]
}

/** Meta information which can be attached to each argument of SMethod.
  * @param name  name of the argument
  * @param description argument description. */
case class ArgInfo(name: String, description: String)

/** Meta information which can be attached to SMethod.
  * @param description  human readable description of the method
  * @param args         one item for each argument */
case class OperationInfo(opDesc: Option[ValueCompanion], description: String, args: Seq[ArgInfo]) {
  def isFrontendOnly: Boolean = opDesc.isEmpty
  def opTypeName: String = opDesc.map(_.typeName).getOrElse("(FRONTEND ONLY)")
}

object OperationInfo {
  def apply(opDesc: ValueCompanion, description: String, args: Seq[ArgInfo]): OperationInfo =
    OperationInfo(Some(opDesc), description, args)
}

/** Meta information connecting SMethod with ErgoTree.
  * @param  irBuilder  optional recognizer and ErgoTree node builder.    */
case class MethodIRInfo(
    irBuilder: Option[PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue]]
)


/** Method info including name, arg type and result type.
  * Here stype.tDom - arg type and stype.tRange - result type.
  * `methodId` should be unique among methods of the same objType. */
case class SMethod(
    objType: STypeCompanion,
    name: String,
    stype: SFunc,
    methodId: Byte,
    irInfo: MethodIRInfo,
    docInfo: Option[OperationInfo]) {

  def withSType(newSType: SFunc): SMethod = copy(stype = newSType)

  def withConcreteTypes(subst: Map[STypeVar, SType]): SMethod =
    withSType(stype.withSubstTypes(subst).asFunc)

  def opId: OperationId = {
    val opName = objType.getClass.getSimpleName + "." + name
    OperationId(opName, stype)
  }

  def specializeFor(objTpe: SType, args: Seq[SType]): SMethod = {
    SigmaTyper.unifyTypeLists(stype.tDom, objTpe +: args) match {
      case Some(subst) if subst.nonEmpty =>
        withConcreteTypes(subst)
      case _ => this
    }
  }
  def withInfo(opDesc: ValueCompanion, desc: String, args: ArgInfo*): SMethod = {
    this.copy(docInfo = Some(OperationInfo(opDesc, desc, ArgInfo("this", "this instance") +: args.toSeq)))
  }
  def withInfo(desc: String, args: ArgInfo*): SMethod = {
    this.copy(docInfo = Some(OperationInfo(None, desc, ArgInfo("this", "this instance") +: args.toSeq)))
  }
  def withIRInfo(
      irBuilder: PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue]): SMethod = {
    this.copy(irInfo = MethodIRInfo(Some(irBuilder)))
  }
  def argInfo(argName: String): ArgInfo =
    docInfo.get.args.find(_.name == argName).get
}


object SMethod {
  type RCosted[A] = RuntimeCosting#RCosted[A]
  val MethodCallIrBuilder: PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue] = {
    case (builder, obj, method, args, tparamSubst) =>
      builder.mkMethodCall(obj, method, args.toIndexedSeq, tparamSubst)
  }

  def apply(objType: STypeCompanion, name: String, stype: SFunc, methodId: Byte): SMethod = {
    SMethod(objType, name, stype, methodId, MethodIRInfo(None), None)
  }

  def fromIds(typeId: Byte, methodId: Byte): SMethod = {
    ValidationRules.CheckTypeWithMethods(typeId, SType.types.contains(typeId))
    val typeCompanion = SType.types(typeId)
    val method = typeCompanion.methodById(methodId)
    method
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
  override val typeCode = 0: Byte
  override def isConstantSize = true
  override def dataSize(v: SType#WrappedType): Long =
    sys.error(s"$this.dataSize($v) is not defined")
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
  def unapply(t: SType): Option[SType] = SType.allPredefTypes.find(_ == t)

  /** Type code of the last valid prim type so that (1 to LastPrimTypeCode) is a range of valid codes. */
  final val LastPrimTypeCode: Byte = 8: Byte

  /** Upper limit of the interval of valid type codes for primitive types */
  final val MaxPrimTypeCode: Byte = 11: Byte
  final val PrimRange: Byte = (MaxPrimTypeCode + 1).toByte
}

/** Marker trait for all numeric types. */
trait SNumericType extends SProduct {
  import SNumericType._
  protected override def getMethods(): Seq[SMethod] = {
    super.getMethods() ++ SNumericType.methods.map {
      m => m.copy(stype = SigmaTyper.applySubst(m.stype, Map(tNum -> this)).asFunc)
    }
  }
  def isCastMethod (name: String): Boolean = castMethods.contains(name)

  def upcast(i: AnyVal): WrappedType
  def downcast(i: AnyVal): WrappedType

  /** Returns a type which is larger. */
  @inline def max(that: SNumericType): SNumericType =
    if (this.typeIndex > that.typeIndex) this else that

  /** Number of bytes to store values of this type. */
  @inline private def typeIndex: Int = allNumericTypes.indexOf(this)

  override def toString: Idn = this.getClass.getSimpleName
}
object SNumericType extends STypeCompanion {
  final val allNumericTypes = Array(SByte, SShort, SInt, SLong, SBigInt)
  def typeId: TypeCode = 106: Byte
  val tNum = STypeVar("TNum")

  val ToByteMethod: SMethod = SMethod(this, "toByte", SFunc(tNum, SByte), 1)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Byte}, throwing exception if overflow.")
  val ToShortMethod: SMethod = SMethod(this, "toShort", SFunc(tNum, SShort), 2)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Short}, throwing exception if overflow.")
  val ToIntMethod: SMethod = SMethod(this, "toInt", SFunc(tNum, SInt), 3)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Int}, throwing exception if overflow.")
  val ToLongMethod: SMethod = SMethod(this, "toLong", SFunc(tNum, SLong), 4)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Long}, throwing exception if overflow.")
  val ToBigIntMethod: SMethod = SMethod(this, "toBigInt", SFunc(tNum, SBigInt), 5)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{BigInt}")
  val ToBytesMethod: SMethod = SMethod(this, "toBytes", SFunc(tNum, SByteArray), 6)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall,
      """ Returns a big-endian representation of this numeric value in a collection of bytes.
        | For example, the \lst{Int} value \lst{0x12131415} would yield the
        | collection of bytes \lst{[0x12, 0x13, 0x14, 0x15]}.
          """.stripMargin)
  val ToBitsMethod: SMethod = SMethod(this, "toBits", SFunc(tNum, SBooleanArray), 7)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall,
      """ Returns a big-endian representation of this numeric in a collection of Booleans.
        |  Each boolean corresponds to one bit.
          """.stripMargin)

  override val methods: Seq[SMethod] = Array(
    ToByteMethod,  // see Downcast
    ToShortMethod,  // see Downcast
    ToIntMethod,  // see Downcast
    ToLongMethod,  // see Downcast
    ToBigIntMethod,  // see Downcast
    ToBytesMethod,
    ToBitsMethod
  )
  val castMethods: Array[String] =
    Array(ToByteMethod, ToShortMethod, ToIntMethod, ToLongMethod, ToBigIntMethod)
      .map(_.name)
}

trait SLogical extends SType {
}

/** Monomorphic type descriptor i.e. a type without generic parameters.
  * @see `SGenericType`
  */
trait SMonoType extends SType with STypeCompanion {
  protected def property(name: String, tpeRes: SType, id: Byte): SMethod =
    SMethod(this, name, SFunc(this, tpeRes), id)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall, "")

  protected def property(name: String, tpeRes: SType, id: Byte, valueCompanion: ValueCompanion): SMethod =
    SMethod(this, name, SFunc(this, tpeRes), id)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(valueCompanion, "")
}

case object SBoolean extends SPrimType with SEmbeddable with SLogical with SProduct with SMonoType {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 1: Byte
  override def typeId = typeCode
  val ToByte = "toByte"
  protected override def getMethods() = super.getMethods()
  /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ++ Seq(
    SMethod(this, ToByte, SFunc(this, SByte), 1)
      .withInfo(PropertyCall, "Convert true to 1 and false to 0"),
  )
  */
  override def dataSize(v: SType#WrappedType): Long = 1
  override def isConstantSize = true
}

case object SByte extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Byte
  override val typeCode: TypeCode = 2: Byte
  override def typeId = typeCode
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

case object SShort extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Short
  override val typeCode: TypeCode = 3: Byte
  override def typeId = typeCode
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

case object SInt extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Int
  override val typeCode: TypeCode = 4: Byte
  override def typeId = typeCode
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

case object SLong extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 5: Byte
  override def typeId = typeCode
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

/** Type of 256 bit integet values. Implemented using [[java.math.BigInteger]]. */
case object SBigInt extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = BigInt
  override val typeCode: TypeCode = 6: Byte
  override def typeId = typeCode

  /** Type of Relation binary op like GE, LE, etc. */
  val RelationOpType = SFunc(Array(SBigInt, SBigInt), SBoolean)

  /** The maximum size of BigInteger value in byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxBigIntSizeInBytes.value

  override def dataSize(v: SType#WrappedType): Long = MaxSizeInBytes

  /** While the size of BigInteger values is limited by the available memory this is not the case with sigma BigInt.
    * In sigma we limit the size by the fixed constant and thus BigInt is a constant size type. */
  override def isConstantSize = true

  val Max: BigInt = SigmaDsl.BigInt(CryptoConstants.dlogGroup.order)

  override def upcast(v: AnyVal): BigInt = {
    val bi = v match {
      case x: Byte => BigInteger.valueOf(x.toLong)
      case x: Short => BigInteger.valueOf(x.toLong)
      case x: Int => BigInteger.valueOf(x.toLong)
      case x: Long => BigInteger.valueOf(x)
      case _ => sys.error(s"Cannot upcast value $v to the type $this")
    }
    SigmaDsl.BigInt(bi)
  }
  override def downcast(v: AnyVal): BigInt = {
    val bi = v match {
      case x: Byte => BigInteger.valueOf(x.toLong)
      case x: Short => BigInteger.valueOf(x.toLong)
      case x: Int => BigInteger.valueOf(x.toLong)
      case x: Long => BigInteger.valueOf(x)
      case _ => sys.error(s"Cannot downcast value $v to the type $this")
    }
    SigmaDsl.BigInt(bi)
  }

  val ModQMethod = SMethod(this, "modQ", SFunc(this, SBigInt), 1)
      .withInfo(ModQ, "Returns this \\lst{mod} Q, i.e. remainder of division by Q, where Q is an order of the cryprographic group.")
  val PlusModQMethod = SMethod(this, "plusModQ", SFunc(IndexedSeq(this, SBigInt), SBigInt), 2)
      .withInfo(ModQArithOp.PlusModQ, "Adds this number with \\lst{other} by module Q.", ArgInfo("other", "Number to add to this."))
  val MinusModQMethod = SMethod(this, "minusModQ", SFunc(IndexedSeq(this, SBigInt), SBigInt), 3)
      .withInfo(ModQArithOp.MinusModQ, "Subtracts \\lst{other} number from this by module Q.", ArgInfo("other", "Number to subtract from this."))
  val MultModQMethod = SMethod(this, "multModQ", SFunc(IndexedSeq(this, SBigInt), SBigInt), 4)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall, "Multiply this number with \\lst{other} by module Q.", ArgInfo("other", "Number to multiply with this."))
  protected override def getMethods() = super.getMethods() ++ Seq(
//    ModQMethod,
//    PlusModQMethod,
//    MinusModQMethod,
    // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    // MultModQMethod,
  )
}

/** NOTE: this descriptor both type and type companion */
case object SString extends SProduct with SMonoType {
  override type WrappedType = String
  override val typeCode: TypeCode = 102: Byte
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = v.asInstanceOf[String].length
  override def isConstantSize = false
}

/** NOTE: this descriptor both type and type companion */
case object SGroupElement extends SProduct with SPrimType with SEmbeddable with SMonoType {
  override type WrappedType = GroupElement
  override val typeCode: TypeCode = 7: Byte
  override def typeId = typeCode
  override def coster: Option[CosterFactory] = Some(Coster(_.GroupElementCoster))

  lazy val GetEncodedMethod: SMethod = SMethod(this, "getEncoded", SFunc(IndexedSeq(this), SByteArray), 2)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall, "Get an encoding of the point value.")
  lazy val ExponentiateMethod: SMethod = SMethod(this, "exp", SFunc(IndexedSeq(this, SBigInt), this), 3)
    .withIRInfo({ case (builder, obj, _, Seq(arg), _) =>
      builder.mkExponentiate(obj.asGroupElement, arg.asBigInt)
    })
    .withInfo(Exponentiate,
      "Exponentiate this \\lst{GroupElement} to the given number. Returns this to the power of k",
      ArgInfo("k", "The power"))
  lazy val MultiplyMethod: SMethod = SMethod(this, "multiply", SFunc(IndexedSeq(this, SGroupElement), this), 4)
    .withIRInfo({ case (builder, obj, _, Seq(arg), _) =>
      builder.mkMultiplyGroup(obj.asGroupElement, arg.asGroupElement)
    })
    .withInfo(MultiplyGroup, "Group operation.", ArgInfo("other", "other element of the group"))
  lazy val NegateMethod: SMethod = SMethod(this, "negate", SFunc(this, this), 5)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall, "Inverse element of the group.")

  protected override def getMethods(): Seq[SMethod] = super.getMethods() ++ Seq(
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    SMethod(this, "isIdentity", SFunc(this, SBoolean),   1)
        .withInfo(PropertyCall, "Checks if this value is identity element of the eliptic curve group."),
    */
    GetEncodedMethod,
    ExponentiateMethod,
    MultiplyMethod,
    NegateMethod
  )
  override def dataSize(v: SType#WrappedType): Long = CryptoConstants.EncodedGroupElementLength.toLong
  override def isConstantSize = true
}

case object SSigmaProp extends SProduct with SPrimType with SEmbeddable with SLogical with SMonoType {
  import SType._
  override type WrappedType = SigmaProp
  override val typeCode: TypeCode = 8: Byte
  override def typeId = typeCode

  /** The maximum size of SigmaProp value in serialized byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxSigmaPropSizeInBytes.value

  override def dataSize(v: SType#WrappedType): Long = MaxSizeInBytes

  override def isConstantSize = true
  val PropBytes = "propBytes"
  val IsProven = "isProven"
  lazy val PropBytesMethod = SMethod(this, PropBytes, SFunc(this, SByteArray), 1)
      .withInfo(SigmaPropBytes, "Serialized bytes of this sigma proposition taken as ErgoTree.")
  lazy val IsProvenMethod = SMethod(this, IsProven, SFunc(this, SBoolean), 2)
      .withInfo(// available only at frontend of ErgoScript
        "Verify that sigma proposition is proven.")
  protected override def getMethods() = super.getMethods() ++ Seq(
    PropBytesMethod, IsProvenMethod
  )
}

/** Any other type is implicitly subtype of this type. */
case object SAny extends SPrimType {
  override type WrappedType = Any
  override val typeCode: TypeCode = 97: Byte
  override def isConstantSize = false
  override def dataSize(v: SType#WrappedType): Long =
    sys.error(s"$this.dataSize($v) is not defined")
}

/** The type with single inhabitant value `()` */
case object SUnit extends SPrimType {
  override type WrappedType = Unit
  override val typeCode: TypeCode = 98: Byte
  override def dataSize(v: SType#WrappedType) = 1
  override def isConstantSize = true
}

/** Type description of optional values. Instances of `Option`
  *  are either constructed by `Some` or by `None` constructors. */
case class SOption[ElemType <: SType](elemType: ElemType) extends SProduct with SGenericType {
  import SOption._
  override type WrappedType = Option[ElemType#WrappedType]
  override val typeCode: TypeCode = SOption.OptionTypeCode
  override def dataSize(v: SType#WrappedType) = {
    val opt = v.asInstanceOf[Option[ElemType#WrappedType]]
    1L + opt.fold(0L)(x => elemType.dataSize(x))
  }
  override def isConstantSize = elemType.isConstantSize
  protected override def getMethods() = super.getMethods() ++ SOption.methods
//  override lazy val methods: Seq[SMethod] = {
//    val subst = Map(SOption.tT -> elemType)
//    SOption.methods.map { method =>
//      method.copy(stype = SigmaTyper.applySubst(method.stype, subst))
//    }
//  }
  override def toString = s"Option[$elemType]"
  override def toTermString: String = s"Option[${elemType.toTermString}]"

  lazy val typeParams: Seq[STypeParam] = Array(SType.paramT)
}

object SOption extends STypeCompanion {
  val OptionTypeConstrId = 3
  val OptionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionTypeConstrId).toByte
  val OptionCollectionTypeConstrId = 4
  val OptionCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionCollectionTypeConstrId).toByte
  override def typeId = OptionTypeCode

  override val coster: Option[CosterFactory] = Some(Coster(_.OptionCoster))

  type SBooleanOption      = SOption[SBoolean.type]
  type SByteOption         = SOption[SByte.type]
  type SShortOption        = SOption[SShort.type]
  type SIntOption          = SOption[SInt.type]
  type SLongOption         = SOption[SLong.type]
  type SBigIntOption       = SOption[SBigInt.type]
  type SGroupElementOption = SOption[SGroupElement.type]
  type SBoxOption          = SOption[SBox.type]
  type SAvlTreeOption      = SOption[SAvlTree.type]

  implicit val SByteOption = SOption(SByte)
  implicit val SByteArrayOption = SOption(SByteArray)
  implicit val SShortOption = SOption(SShort)
  implicit val SIntOption = SOption(SInt)
  implicit val SLongOption = SOption(SLong)
  implicit val SBigIntOption = SOption(SBigInt)
  implicit val SBooleanOption = SOption(SBoolean)
  implicit val SAvlTreeOption = SOption(SAvlTree)
  implicit val SGroupElementOption = SOption(SGroupElement)
  implicit val SSigmaPropOption = SOption(SSigmaProp)
  implicit val SBoxOption = SOption(SBox)

  val IsDefined = "isDefined"
  val Get = "get"
  val GetOrElse = "getOrElse"
  val Fold = "fold"

  import SType.{tT, tR, paramT, paramR}
  val ThisType = SOption(tT)

  val IsDefinedMethod = SMethod(this, IsDefined, SFunc(ThisType, SBoolean), 2)
      .withInfo(OptionIsDefined,
        "Returns \\lst{true} if the option is an instance of \\lst{Some}, \\lst{false} otherwise.")

  val GetMethod       = SMethod(this, Get, SFunc(ThisType, tT), 3)
      .withInfo(OptionGet,
      """Returns the option's value. The option must be nonempty. Throws exception if the option is empty.""")

  val GetOrElseMethod = SMethod(this, GetOrElse, SFunc(IndexedSeq(ThisType, tT), tT, Seq(tT)), 4)
      .withInfo(OptionGetOrElse,
        """Returns the option's value if the option is nonempty, otherwise
         |return the result of evaluating \lst{default}.
        """.stripMargin, ArgInfo("default", "the default value"))

  val FoldMethod      = SMethod(this, Fold, SFunc(Array(ThisType, tR, SFunc(tT, tR)), tR, Seq(tT, tR)), 5)
      .withInfo(MethodCall,
        """Returns the result of applying \lst{f} to this option's
         |  value if the option is nonempty.  Otherwise, evaluates
         |  expression \lst{ifEmpty}.
         |  This is equivalent to \lst{option map f getOrElse ifEmpty}.
        """.stripMargin,
        ArgInfo("ifEmpty", "the expression to evaluate if empty"),
        ArgInfo("f", "the function to apply if nonempty"))

  val MapMethod       = SMethod(this, "map",
    SFunc(Array(ThisType, SFunc(tT, tR)), SOption(tR), Array(paramT, paramR)), 7)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """Returns a \lst{Some} containing the result of applying \lst{f} to this option's
         |   value if this option is nonempty.
         |   Otherwise return \lst{None}.
        """.stripMargin, ArgInfo("f", "the function to apply"))

  val FilterMethod    = SMethod(this, "filter",
    SFunc(Array(ThisType, SFunc(tT, SBoolean)), ThisType, Array(paramT)), 8)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """Returns this option if it is nonempty and applying the predicate \lst{p} to
         |  this option's value returns true. Otherwise, return \lst{None}.
        """.stripMargin, ArgInfo("p", "the predicate used for testing"))

  val methods: Seq[SMethod] = Seq(
    IsDefinedMethod,
    GetMethod,
    GetOrElseMethod,
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    FoldMethod,
    */
    MapMethod,
    FilterMethod
  )
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SOption[T] = SOption(elemType)
//  def unapply[T <: SType](tOpt: SOption[T]): Option[T] = Some(tOpt.elemType)
}

trait SCollection[T <: SType] extends SProduct with SGenericType {
  def elemType: T
  override type WrappedType = Coll[T#WrappedType]
  override def isConstantSize = false
}

case class SCollectionType[T <: SType](elemType: T) extends SCollection[T] {
  override val typeCode: TypeCode = SCollectionType.CollectionTypeCode

  override def dataSize(v: SType#WrappedType): Long = {
    val coll = v.asInstanceOf[Coll[T#WrappedType]]
    implicit val sT = Sized.typeToSized(Evaluation.stypeToRType(elemType))
    Sized.sizeOf(coll).dataSize
  }
  def typeParams: Seq[STypeParam] = SCollectionType.typeParams
  protected override def getMethods() = super.getMethods() ++ SCollection.methods
  override def toString = s"Coll[$elemType]"
  override def toTermString = s"Coll[${elemType.toTermString}]"
}

object SCollectionType {
  val CollectionTypeConstrId = 1
  val CollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * CollectionTypeConstrId).toByte
  val NestedCollectionTypeConstrId = 2
  val NestedCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * NestedCollectionTypeConstrId).toByte
  val typeParams: Seq[STypeParam] = Array(SType.paramIV)
}

object SCollection extends STypeCompanion with MethodByNameUnapply {
  override def typeId = SCollectionType.CollectionTypeCode
  override def coster: Option[CosterFactory] = Some(Coster(_.CollCoster))

  import SType.{tK, tV, paramIV, paramOV}

  def tIV = SType.tIV
  def tOV = SType.tOV

  val ThisType = SCollection(tIV)
  val tOVColl = SCollection(tOV)
  val tPredicate = SFunc(tIV, SBoolean)

  val SizeMethod = SMethod(this, "size", SFunc(ThisType, SInt), 1)
      .withInfo(SizeOf, "The size of the collection in elements.")

  val GetOrElseMethod = SMethod(this, "getOrElse", SFunc(IndexedSeq(ThisType, SInt, tIV), tIV, Seq(paramIV)), 2)
      .withIRInfo({ case (builder, obj, _, Seq(index, defaultValue), _) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        builder.mkByIndex(obj.asValue[SCollection[SType]], index1, Some(defaultValue1))
      })
      .withInfo(ByIndex, "Return the element of collection if \\lst{index} is in range \\lst{0 .. size-1}",
        ArgInfo("index", "index of the element of this collection"),
        ArgInfo("default", "value to return when \\lst{index} is out of range"))

  val MapMethod = SMethod(this, "map", SFunc(IndexedSeq(ThisType, SFunc(tIV, tOV)), tOVColl, Seq(paramIV, paramOV)), 3)
      .withInfo(MapCollection,
        """ Builds a new collection by applying a function to all elements of this collection.
         | Returns a new collection of type \lst{Coll[B]} resulting from applying the given function
         | \lst{f} to each element of this collection and collecting the results.
        """.stripMargin,
        ArgInfo("f", "the function to apply to each element"))

  val ExistsMethod = SMethod(this, "exists", SFunc(IndexedSeq(ThisType, tPredicate), SBoolean, Seq(paramIV)), 4)
      .withInfo(Exists,
        """Tests whether a predicate holds for at least one element of this collection.
         |Returns \lst{true} if the given predicate \lst{p} is satisfied by at least one element of this collection, otherwise \lst{false}
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements"))

  val FoldMethod = SMethod(this, "fold", SFunc(IndexedSeq(ThisType, tOV, SFunc(IndexedSeq(tOV, tIV), tOV)), tOV, Seq(paramIV, paramOV)), 5)
      .withInfo(Fold, "Applies a binary operator to a start value and all elements of this collection, going left to right.",
        ArgInfo("zero", "a starting value"),
        ArgInfo("op", "the binary operator"))

  val ForallMethod = SMethod(this, "forall", SFunc(IndexedSeq(ThisType, tPredicate), SBoolean, Seq(paramIV)), 6)
      .withInfo(ForAll,
        """Tests whether a predicate holds for all elements of this collection.
         |Returns \lst{true} if this collection is empty or the given predicate \lst{p}
         |holds for all elements of this collection, otherwise \lst{false}.
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements"))

  val SliceMethod = SMethod(this, "slice", SFunc(IndexedSeq(ThisType, SInt, SInt), ThisType, Seq(paramIV)), 7)
      .withInfo(Slice,
        """Selects an interval of elements.  The returned collection is made up
         |  of all elements \lst{x} which satisfy the invariant:
         |  \lst{
         |     from <= indexOf(x) < until
         |  }
        """.stripMargin,
        ArgInfo("from", "the lowest index to include from this collection"),
        ArgInfo("until", "the lowest index to EXCLUDE from this collection"))

  val FilterMethod = SMethod(this, "filter", SFunc(IndexedSeq(ThisType, tPredicate), ThisType, Seq(paramIV)), 8)
      .withIRInfo({
        case (builder, obj, _, Seq(l), _) => builder.mkFilter(obj.asValue[SCollection[SType]], l.asFunc)
      })
      .withInfo(Filter,
        """Selects all elements of this collection which satisfy a predicate.
         | Returns  a new collection consisting of all elements of this collection that satisfy the given
         | predicate \lst{p}. The order of the elements is preserved.
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements."))

  val AppendMethod = SMethod(this, "append", SFunc(IndexedSeq(ThisType, ThisType), ThisType, Seq(paramIV)), 9)
  .withIRInfo({
    case (builder, obj, _, Seq(xs), _) =>
      builder.mkAppend(obj.asCollection[SType], xs.asCollection[SType])
  })
      .withInfo(Append, "Puts the elements of other collection after the elements of this collection (concatenation of 2 collections)",
        ArgInfo("other", "the collection to append at the end of this"))
  val ApplyMethod = SMethod(this, "apply", SFunc(IndexedSeq(ThisType, SInt), tIV, Seq(tIV)), 10)
      .withInfo(ByIndex,
        """The element at given index.
         | Indices start at \lst{0}; \lst{xs.apply(0)} is the first element of collection \lst{xs}.
         | Note the indexing syntax \lst{xs(i)} is a shorthand for \lst{xs.apply(i)}.
         | Returns the element at the given index.
         | Throws an exception if \lst{i < 0} or \lst{length <= i}
        """.stripMargin, ArgInfo("i", "the index"))

  val BitShiftLeftMethod = SMethod(this, "<<",
    SFunc(IndexedSeq(ThisType, SInt), ThisType, Seq(paramIV)), 11)
  val BitShiftRightMethod = SMethod(this, ">>",
    SFunc(IndexedSeq(ThisType, SInt), ThisType, Seq(paramIV)), 12)
  val BitShiftRightZeroedMethod = SMethod(this, ">>>",
    SFunc(IndexedSeq(SCollection(SBoolean), SInt), SCollection(SBoolean)), 13)

  val IndicesMethod = SMethod(this, "indices", SFunc(ThisType, SCollection(SInt)), 14)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """Produces the range of all indices of this collection as a new collection
         | containing [0 .. length-1] values.
        """.stripMargin)

  val FlatMapMethod = SMethod(this, "flatMap",
    SFunc(IndexedSeq(ThisType, SFunc(tIV, tOVColl)), tOVColl, Seq(paramIV, paramOV)), 15)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """ Builds a new collection by applying a function to all elements of this collection
         | and using the elements of the resulting collections.
         | Function \lst{f} is constrained to be of the form \lst{x => x.someProperty}, otherwise
         | it is illegal.
         | Returns a new collection of type \lst{Coll[B]} resulting from applying the given collection-valued function
         | \lst{f} to each element of this collection and concatenating the results.
        """.stripMargin, ArgInfo("f", "the function to apply to each element."))


  val PatchMethod = SMethod(this, "patch",
    SFunc(IndexedSeq(ThisType, SInt, ThisType, SInt), ThisType, Seq(paramIV)), 19)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall, "")

  val UpdatedMethod = SMethod(this, "updated",
    SFunc(IndexedSeq(ThisType, SInt, tIV), ThisType, Seq(paramIV)), 20)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val UpdateManyMethod = SMethod(this, "updateMany",
    SFunc(IndexedSeq(ThisType, SCollection(SInt), ThisType), ThisType, Seq(paramIV)), 21)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val UnionSetsMethod = SMethod(this, "unionSets",
    SFunc(IndexedSeq(ThisType, ThisType), ThisType, Seq(paramIV)), 22)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val DiffMethod = SMethod(this, "diff",
    SFunc(IndexedSeq(ThisType, ThisType), ThisType, Seq(paramIV)), 23)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")
  val IntersectMethod = SMethod(this, "intersect",
    SFunc(IndexedSeq(ThisType, ThisType), ThisType, Seq(paramIV)), 24)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val PrefixLengthMethod = SMethod(this, "prefixLength",
    SFunc(IndexedSeq(ThisType, tPredicate), SInt, Seq(paramIV)), 25)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val IndexOfMethod = SMethod(this, "indexOf",
    SFunc(IndexedSeq(ThisType, tIV, SInt), SInt, Seq(paramIV)), 26)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val LastIndexOfMethod = SMethod(this, "lastIndexOf",
    SFunc(IndexedSeq(ThisType, tIV, SInt), SInt, Seq(paramIV)), 27)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  // TODO HF: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  lazy val FindMethod = SMethod(this, "find",
    SFunc(IndexedSeq(ThisType, tPredicate), SOption(tIV), Seq(paramIV)), 28)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val ZipMethod = SMethod(this, "zip",
    SFunc(IndexedSeq(ThisType, tOVColl), SCollection(STuple(tIV, tOV)), Seq(tIV, tOV)), 29)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val DistinctMethod = SMethod(this, "distinct",
    SFunc(IndexedSeq(ThisType), ThisType, Seq(tIV)), 30)
      .withIRInfo(MethodCallIrBuilder).withInfo(PropertyCall, "")

  val StartsWithMethod = SMethod(this, "startsWith",
    SFunc(IndexedSeq(ThisType, ThisType, SInt), SBoolean, Seq(paramIV)), 31)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val EndsWithMethod = SMethod(this, "endsWith",
    SFunc(IndexedSeq(ThisType, ThisType), SBoolean, Seq(paramIV)), 32)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  val MapReduceMethod = SMethod(this, "mapReduce",
    SFunc(
      IndexedSeq(ThisType, SFunc(tIV, STuple(tK, tV)), SFunc(STuple(tV, tV), tV)),
      SCollection(STuple(tK, tV)),
      Seq(paramIV, STypeParam(tK), STypeParam(tV))), 34)
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  lazy val methods: Seq[SMethod] = Seq(
    SizeMethod,
    GetOrElseMethod,
    MapMethod,
    ExistsMethod,
    FoldMethod,
    ForallMethod,
    SliceMethod,
    FilterMethod,
    AppendMethod,
    ApplyMethod,
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    BitShiftLeftMethod,
    BitShiftRightMethod,
    BitShiftRightZeroedMethod,
    */
    IndicesMethod,
    FlatMapMethod,
    PatchMethod,
    UpdatedMethod,
    UpdateManyMethod,
    /*TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    UnionSetsMethod,
    DiffMethod,
    IntersectMethod,
    PrefixLengthMethod,
    */
    IndexOfMethod,
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    LastIndexOfMethod,
    FindMethod,
    */
    ZipMethod
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    DistinctMethod,
    StartsWithMethod,
    EndsWithMethod,
    MapReduceMethod,
    */
  )
  def apply[T <: SType](elemType: T): SCollection[T] = SCollectionType(elemType)
  def apply[T <: SType](implicit elemType: T, ov: Overload1): SCollection[T] = SCollectionType(elemType)

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
  val SByteArray2        = SCollection(SCollection(SByte))
  val SShortArray        = SCollection(SShort)
  val SIntArray          = SCollection(SInt)
  val SLongArray         = SCollection(SLong)
  val SBigIntArray       = SCollection(SBigInt)
  val SGroupElementArray = SCollection(SGroupElement)
  val SSigmaPropArray    = SCollection(SSigmaProp)
  val SBoxArray          = SCollection(SBox)
  val SAvlTreeArray      = SCollection(SAvlTree)
  val SHeaderArray       = SCollection(SHeader)
}

case class STuple(items: IndexedSeq[SType]) extends SCollection[SAny.type] {
  import STuple._
  override val typeCode = STuple.TupleTypeCode

  override def isConstantSize: Boolean = {
    items.forall(t => t.isConstantSize)
  }

  override def dataSize(v: SType#WrappedType) = {
    if (isConstantSize) {
      var sum: Long = 2 // header
      for (item <- items) {
        sum += item.dataSize(v)
      }
      sum
    } else {
      val arr = (v match {
        case col: Coll[_] => col.toArray
        case p: Tuple2[_,_] => p.toArray
        case _ => v
      }).asInstanceOf[Array[Any]]
      assert(arr.length == items.length)
      var sum: Long = 2 // header
      for (i <- arr.indices) {
        sum += items(i).dataSize(arr(i).asInstanceOf[SType#WrappedType])
      }
      sum
    }
  }

  override def elemType: SAny.type = SAny

  protected override def getMethods() = {
    val tupleMethods = Array.tabulate(items.size) { i =>
      SMethod(STuple, componentNameByIndex(i), SFunc(this, items(i)), (i + 1).toByte)
    }
    colMethods ++ tupleMethods
  }

  val typeParams = Nil

  override def toTermString = s"(${items.map(_.toTermString).mkString(",")})"
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

  lazy val colMethods = {
    val subst = Map(SType.tIV -> SAny)
    // TODO: implement other
    val activeMethods = Set(1.toByte, 10.toByte)
    SCollection.methods.filter(m => activeMethods.contains(m.methodId)).map { m =>
      m.copy(stype = SigmaTyper.applySubst(m.stype, subst).asFunc)
    }
  }

  def methods: Seq[SMethod] = sys.error(s"Shouldn't be called.")

  def apply(items: SType*): STuple = STuple(items.toArray)
  val MaxTupleLength: Int = SigmaConstants.MaxTupleLength.value
  private val componentNames = Array.tabulate(MaxTupleLength){ i => s"_${i + 1}" }
  def componentNameByIndex(i: Int): String =
    try componentNames(i)
    catch {
      case e: IndexOutOfBoundsException =>
        throw new IllegalArgumentException(
          s"Tuple component '_${i+1}' is not defined: valid range (1 .. $MaxTupleLength)", e)
    }
}

/** Helper constuctor/extractor for tuples of two types. */
object SPair {
  def apply(l: SType, r: SType) = STuple(Array(l, r))
  def unapply(t: STuple): Nullable[(SType, SType)] = t match {
    case STuple(IndexedSeq(l, r)) => Nullable((l, r))
    case _ => Nullable.None
  }
}

case class SFunc(tDom: IndexedSeq[SType],  tRange: SType, tpeParams: Seq[STypeParam] = Nil)
      extends SType with SGenericType
{
  override type WrappedType = Any => tRange.WrappedType
  override val typeCode = SFunc.FuncTypeCode
  override def isConstantSize = false
  override def toString = {
    val args = if (tpeParams.isEmpty) "" else tpeParams.mkString("[", ",", "]")
    s"$args(${tDom.mkString(",")}) => $tRange"
  }
  override def toTermString = {
    val args = if (tpeParams.isEmpty) "" else tpeParams.mkString("[", ",", "]")
    s"$args(${tDom.map(_.toTermString).mkString(",")}) => ${tRange.toTermString}"
  }
  override def dataSize(v: SType#WrappedType) = 8L
  import SFunc._
  val typeParams: Seq[STypeParam] = tpeParams

  def getGenericType: SFunc = {
    val typeParams: Seq[STypeParam] = tDom.zipWithIndex
      .map { case (_, i) => STypeParam(SType.tD.name + (i + 1)) } :+ STypeParam(SType.tR.name)
    val ts = typeParams.map(_.ident)
    SFunc(ts.init.toIndexedSeq, ts.last, Nil)
  }
  def withReceiverType(objType: SType) = this.copy(tDom = objType +: tDom)
}

object SFunc {
  final val FuncTypeCode: TypeCode = OpCodes.FirstFuncType
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(Array(tDom), tRange) // @hotspot
  val identity = { x: Any => x }
}


case class STypeApply(name: String, args: IndexedSeq[SType] = IndexedSeq()) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeApply.TypeCode
  override def isConstantSize = false
  override def dataSize(v: SType#WrappedType): Long =
    sys.error(s"$this.dataSize($v) is not defined")
}
object STypeApply {
  val TypeCode = 94: Byte
}

/** Type variable which is used in generic method/func signatures. */
case class STypeVar(name: String) extends SType {
  require(name.length <= 255, "name is too long")
  override type WrappedType = Any
  override val typeCode = STypeVar.TypeCode
  override def isConstantSize = false
  override def toString = name
  override def toTermString: String = name
  override def dataSize(v: SType#WrappedType): Long =
    sys.error(s"$this.dataSize($v) is not defined")
}
object STypeVar {
  val TypeCode: TypeCode = 103: Byte
  implicit def liftString(n: String): STypeVar = STypeVar(n)
  val EmptyArray = Array.empty[STypeVar]
  val EmptySeq: IndexedSeq[STypeVar] = EmptyArray
}

case object SBox extends SProduct with SPredefType with SMonoType {
  import ErgoBox._
  override type WrappedType = Box
  override val typeCode: TypeCode = 99: Byte
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = {
    val box = v.asInstanceOf[this.WrappedType]
    Sized.sizeOf(box).dataSize
  }
  override def isConstantSize = false

  import SType.{tT, paramT}

  lazy val GetRegFuncType = SFunc(Array(SBox), SOption(tT), Array(paramT))

  def registers(idOfs: Int): Seq[SMethod] = {
    allRegisters.map { i =>
      i match {
        case r: MandatoryRegisterId =>
          SMethod(this, s"R${i.asIndex}", GetRegFuncType, (idOfs + i.asIndex + 1).toByte)
              .withInfo(ExtractRegisterAs, r.purpose)
        case _ =>
          SMethod(this, s"R${i.asIndex}", GetRegFuncType, (idOfs + i.asIndex + 1).toByte)
              .withInfo(ExtractRegisterAs, "Non-mandatory register")
      }
    }
  }
  val PropositionBytes = "propositionBytes"
  val Value = "value"
  val Id = "id"
  val Bytes = "bytes"
  val BytesWithoutRef = "bytesWithoutRef"
  val CreationInfo = "creationInfo"
  val GetReg = "getReg"

  // should be lazy, otherwise lead to initialization error
  lazy val ValueMethod = SMethod(this, Value, SFunc(SBox, SLong), 1)
      .withInfo(ExtractAmount,
        "Mandatory: Monetary value, in Ergo tokens (NanoErg unit of measure)")

  lazy val PropositionBytesMethod = SMethod(this, PropositionBytes, SFunc(SBox, SByteArray), 2)
      .withInfo(ExtractScriptBytes,
        "Serialized bytes of guarding script, which should be evaluated to true in order to\n" +
        " open this box. (aka spend it in a transaction)")

  lazy val BytesMethod = SMethod(this, Bytes, SFunc(SBox, SByteArray), 3)
      .withInfo(ExtractBytes, "Serialized bytes of this box's content, including proposition bytes.")

  lazy val BytesWithoutRefMethod = SMethod(this, BytesWithoutRef, SFunc(SBox, SByteArray), 4)
      .withInfo(ExtractBytesWithNoRef,
        "Serialized bytes of this box's content, excluding transactionId and index of output.")

  lazy val IdMethod = SMethod(this, Id, SFunc(SBox, SByteArray), 5)
      .withInfo(ExtractId,
        "Blake2b256 hash of this box's content, basically equals to \\lst{blake2b256(bytes)}")

  lazy val creationInfoMethod = SMethod(this, CreationInfo, ExtractCreationInfo.OpType, 6)
      .withInfo(ExtractCreationInfo,
        """ If \lst{tx} is a transaction which generated this box, then \lst{creationInfo._1}
         | is a height of the tx's block. The \lst{creationInfo._2} is a serialized transaction
         | identifier followed by box index in the transaction outputs.
        """.stripMargin ) // see ExtractCreationInfo

  lazy val getRegMethod = SMethod(this, "getReg", SFunc(Array(SBox, SInt), SOption(tT), Array(paramT)), 7)
      .withInfo(ExtractRegisterAs,
        """ Extracts register by id and type.
         | Type param \lst{T} expected type of the register.
         | Returns \lst{Some(value)} if the register is defined and has given type and \lst{None} otherwise
        """.stripMargin,
        ArgInfo("regId", "zero-based identifier of the register."))

  lazy val tokensMethod = SMethod(this, "tokens", SFunc(SBox, ErgoBox.STokensRegType), 8)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall, "Secondary tokens")


  // should be lazy to solve recursive initialization
  protected override def getMethods() = super.getMethods() ++ Array(
    ValueMethod, // see ExtractAmount
    PropositionBytesMethod, // see ExtractScriptBytes
    BytesMethod, // see ExtractBytes
    BytesWithoutRefMethod, // see ExtractBytesWithNoRef
    IdMethod, // see ExtractId
    creationInfoMethod,
    getRegMethod,
    tokensMethod
  ) ++ registers(8)

  override val coster = Some(Coster(_.BoxCoster))
}

case object SAvlTree extends SProduct with SPredefType with SMonoType {
  override type WrappedType = AvlTree
  override val typeCode: TypeCode = 100: Byte
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = AvlTreeData.TreeDataSize
  override def isConstantSize = true

  import SOption._
  lazy val TCollOptionCollByte = SCollection(SByteArrayOption)
  lazy val CollKeyValue = SCollection(STuple(SByteArray, SByteArray))

  lazy val digestMethod            = SMethod(this, "digest", SFunc(this, SByteArray),            1)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """Returns digest of the state represented by this tree.
         | Authenticated tree \lst{digest} = \lst{root hash bytes} ++ \lst{tree height}
        """.stripMargin)

  lazy val enabledOperationsMethod = SMethod(this, "enabledOperations", SFunc(this, SByte),      2)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """ Flags of enabled operations packed in single byte.
         | \lst{isInsertAllowed == (enabledOperations & 0x01) != 0}\newline
         | \lst{isUpdateAllowed == (enabledOperations & 0x02) != 0}\newline
         | \lst{isRemoveAllowed == (enabledOperations & 0x04) != 0}
        """.stripMargin)
  lazy val keyLengthMethod         = SMethod(this, "keyLength", SFunc(this, SInt),               3)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
    """
     |
        """.stripMargin)
  lazy val valueLengthOptMethod    = SMethod(this, "valueLengthOpt", SFunc(this, SIntOption),    4)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)
  lazy val isInsertAllowedMethod   = SMethod(this, "isInsertAllowed", SFunc(this, SBoolean),     5)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)
  lazy val isUpdateAllowedMethod   = SMethod(this, "isUpdateAllowed", SFunc(this, SBoolean),     6)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)
  lazy val isRemoveAllowedMethod   = SMethod(this, "isRemoveAllowed", SFunc(this, SBoolean),     7)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val updateOperationsMethod  = SMethod(this, "updateOperations",
    SFunc(IndexedSeq(SAvlTree, SByte), SAvlTree),                 8)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |
        """.stripMargin)

  lazy val containsMethod          = SMethod(this, "contains",
    SFunc(IndexedSeq(SAvlTree, SByteArray, SByteArray), SBoolean),             9)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |   /** Checks if an entry with key `key` exists in this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |
         |    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
         |    * Return `true` if a leaf with the key `key` exists
         |    * Return `false` if leaf with provided key does not exist.
         |    * @param key    a key of an element of this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  lazy val getMethod               = SMethod(this, "get",
    SFunc(IndexedSeq(SAvlTree, SByteArray, SByteArray), SByteArrayOption),     10)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform a lookup of key `key` in this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    *
         |    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
         |    * Return Some(bytes) of leaf with key `key` if it exists
         |    * Return None if leaf with provided key does not exist.
         |    * @param key    a key of an element of this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  lazy val getManyMethod           = SMethod(this, "getMany",
    SFunc(IndexedSeq(SAvlTree, SByteArray2, SByteArray), TCollOptionCollByte), 11)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform a lookup of many keys `keys` in this tree using proof `proof`.
         |    *
         |    * @note CAUTION! Keys must be ordered the same way they were in lookup before proof was generated.
         |    * For each key return Some(bytes) of leaf if it exists and None if is doesn't.
         |    * @param keys    keys of elements of this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  lazy val insertMethod            = SMethod(this, "insert",
    SFunc(IndexedSeq(SAvlTree, CollKeyValue, SByteArray), SAvlTreeOption),     12)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform insertions of key-value entries into this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    *
         |    * @note CAUTION! Pairs must be ordered the same way they were in insert ops before proof was generated.
         |    * Return Some(newTree) if successful
         |    * Return None if operations were not performed.
         |    * @param operations   collection of key-value pairs to insert in this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  lazy val updateMethod            = SMethod(this, "update",
    SFunc(IndexedSeq(SAvlTree, CollKeyValue, SByteArray), SAvlTreeOption),     13)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform updates of key-value entries into this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    *
         |    * @note CAUTION! Pairs must be ordered the same way they were in update ops before proof was generated.
         |    * Return Some(newTree) if successful
         |    * Return None if operations were not performed.
         |    * @param operations   collection of key-value pairs to update in this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  lazy val removeMethod            = SMethod(this, "remove",
    SFunc(IndexedSeq(SAvlTree, SByteArray2, SByteArray), SAvlTreeOption),      14)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |  /** Perform removal of entries into this tree using proof `proof`.
         |    * Throws exception if proof is incorrect
         |    * Return Some(newTree) if successful
         |    * Return None if operations were not performed.
         |    *
         |    * @note CAUTION! Keys must be ordered the same way they were in remove ops before proof was generated.
         |    * @param operations   collection of keys to remove from this authenticated dictionary.
         |    * @param proof
         |    */
         |
        """.stripMargin)

  lazy val updateDigestMethod  = SMethod(this, "updateDigest",
    SFunc(IndexedSeq(SAvlTree, SByteArray), SAvlTree),                       15)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |
        """.stripMargin)

  protected override def getMethods(): Seq[SMethod] = super.getMethods() ++ Seq(
    digestMethod,
    enabledOperationsMethod,
    keyLengthMethod,
    valueLengthOptMethod,
    isInsertAllowedMethod,
    isUpdateAllowedMethod,
    isRemoveAllowedMethod,
    updateOperationsMethod,
    containsMethod,
    getMethod,
    getManyMethod,
    insertMethod,
    updateMethod,
    removeMethod,
    updateDigestMethod
  )
  override val coster = Some(Coster(_.AvlTreeCoster))
}

case object SContext extends SProduct with SPredefType with SMonoType {
  override type WrappedType = ErgoLikeContext
  override val typeCode: TypeCode = 101: Byte
  override def typeId = typeCode

  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    sys.error(s"$this.dataSize($v) is not defined")
  }
  override def isConstantSize = false

  import SType.{tT, paramT}

  lazy val dataInputsMethod = property("dataInputs", SBoxArray, 1)
  lazy val headersMethod    = property("headers", SHeaderArray, 2)
  lazy val preHeaderMethod  = property("preHeader", SPreHeader, 3)
  lazy val inputsMethod     = property("INPUTS", SBoxArray, 4, Inputs)
  lazy val outputsMethod    = property("OUTPUTS", SBoxArray, 5, Outputs)
  lazy val heightMethod     = property("HEIGHT", SInt, 6, Height)
  lazy val selfMethod       = property("SELF", SBox, 7, Self)
  lazy val selfBoxIndexMethod = property("selfBoxIndex", SInt, 8)
  lazy val lastBlockUtxoRootHashMethod = property("LastBlockUtxoRootHash", SAvlTree, 9, LastBlockUtxoRootHash)
  lazy val minerPubKeyMethod = property("minerPubKey", SByteArray, 10, MinerPubkey)
  lazy val getVarMethod = SMethod(this, "getVar", SFunc(Array(SContext, SByte), SOption(tT), Array(paramT)), 11)
    .withInfo(GetVar, "Get context variable with given \\lst{varId} and type.",
      ArgInfo("varId", "\\lst{Byte} identifier of context variable"))

  protected override def getMethods() = super.getMethods() ++ Seq(
    dataInputsMethod, headersMethod, preHeaderMethod, inputsMethod, outputsMethod, heightMethod, selfMethod,
    selfBoxIndexMethod, lastBlockUtxoRootHashMethod, minerPubKeyMethod, getVarMethod
  )
  override val coster = Some(Coster(_.ContextCoster))
}

case object SHeader extends SProduct with SPredefType with SMonoType {
  override type WrappedType = Header
  override val typeCode: TypeCode = 104: Byte
  override def typeId = typeCode

  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    hashLength + // parentId
    1 + // version
    hashLength + // parentId
    hashLength + // ADProofsRoot
    AvlTreeData.TreeDataSize +
    hashLength + // transactionsRoot
    8 + // timestamp
    8 + // nBits
    4 + // height
    hashLength + // extensionRoot
    CryptoConstants.EncodedGroupElementLength + // minerPk
    CryptoConstants.EncodedGroupElementLength + // powOnetimePk,
    8 + // powNonce
    SBigInt.dataSize(0.asWrappedType) + // powDistance
    3   // votes
  }
  override def isConstantSize = true

  lazy val idMethod               = property("id", SByteArray, 1)
  lazy val versionMethod          = property("version",  SByte,      2)
  lazy val parentIdMethod         = property("parentId", SByteArray, 3)
  lazy val ADProofsRootMethod     = property("ADProofsRoot", SByteArray, 4)
  lazy val stateRootMethod        = property("stateRoot", SAvlTree, 5)
  lazy val transactionsRootMethod = property("transactionsRoot", SByteArray, 6)
  lazy val timestampMethod        = property("timestamp", SLong, 7)
  lazy val nBitsMethod            = property("nBits", SLong, 8)
  lazy val heightMethod           = property("height", SInt, 9)
  lazy val extensionRootMethod    = property("extensionRoot", SByteArray, 10)
  lazy val minerPkMethod          = property("minerPk", SGroupElement, 11)
  lazy val powOnetimePkMethod     = property("powOnetimePk", SGroupElement, 12)
  lazy val powNonceMethod         = property("powNonce", SByteArray, 13)
  lazy val powDistanceMethod      = property("powDistance", SBigInt, 14)
  lazy val votesMethod            = property("votes", SByteArray, 15)

  protected override def getMethods() = super.getMethods() ++ Seq(
    idMethod, versionMethod, parentIdMethod, ADProofsRootMethod, stateRootMethod, transactionsRootMethod,
    timestampMethod, nBitsMethod, heightMethod, extensionRootMethod, minerPkMethod, powOnetimePkMethod,
    powNonceMethod, powDistanceMethod, votesMethod
  )
  override val coster = Some(Coster(_.HeaderCoster))
}

case object SPreHeader extends SProduct with SPredefType with SMonoType {
  override type WrappedType = PreHeader
  override val typeCode: TypeCode = 105: Byte
  override def typeId = typeCode

  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    1 + // version
        hashLength + // parentId
        8 + // timestamp
        8 + // nBits
        4 + // height
        CryptoConstants.EncodedGroupElementLength + // minerPk
        3   // votes
  }
  override def isConstantSize = true

  lazy val versionMethod          = property("version",  SByte,      1)
  lazy val parentIdMethod         = property("parentId", SByteArray, 2)
  lazy val timestampMethod        = property("timestamp", SLong, 3)
  lazy val nBitsMethod            = property("nBits", SLong, 4)
  lazy val heightMethod           = property("height", SInt, 5)
  lazy val minerPkMethod          = property("minerPk", SGroupElement, 6)
  lazy val votesMethod            = property("votes", SByteArray, 7)

  protected override def getMethods() = super.getMethods() ++ Seq(
    versionMethod, parentIdMethod, timestampMethod, nBitsMethod, heightMethod, minerPkMethod, votesMethod
  )
  override val coster = Some(Coster(_.PreHeaderCoster))
}

/** This type is introduced to unify handling of global and non-global (i.e. methods) operations.
  * It unifies implementation of global operation with implementation of methods and avoids code
  * duplication (following DRY principle https://en.wikipedia.org/wiki/Don%27t_repeat_yourself).
  * The WrappedType is `special.sigma.SigmaDslBuilder`, which is an interface implemented by
  * the singleton sigmastate.eval.CostingSigmaDslBuilder
  *
  * The Constant(...) tree node of this type are not allowed, as well as using it in register and
  * context variables (aka ContextExtension)
  *
  * When new methods are added to this type via a soft-fork, they will be serialized as part
  * of ErgoTree using MethodCallSerializer, where SGlobal.typeCode will be used.
  *
  * @see sigmastate.lang.SigmaPredef
  * */
case object SGlobal extends SProduct with SPredefType with SMonoType {
  override type WrappedType = SigmaDslBuilder
  override val typeCode: TypeCode = 106: Byte
  override def typeId = typeCode
  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    sys.error(s"$this.dataSize($v) is not defined")
  }
  override def isConstantSize = true  // only fixed amount of global information is allowed

  import SType.tT

  lazy val groupGeneratorMethod = SMethod(this, "groupGenerator", SFunc(this, SGroupElement), 1)
    .withIRInfo({ case (builder, obj, method, args, tparamSubst) => GroupGenerator })
    .withInfo(GroupGenerator, "")
  lazy val xorMethod = SMethod(this, "xor", SFunc(IndexedSeq(this, SByteArray, SByteArray), SByteArray), 2)
    .withIRInfo({
        case (_, _, _, Seq(l, r), _) => Xor(l.asByteArray, r.asByteArray)
    })
    .withInfo(Xor, "Byte-wise XOR of two collections of bytes",
      ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))

  protected override def getMethods() = super.getMethods() ++ Seq(
    groupGeneratorMethod,
    xorMethod
  )
  override val coster = Some(Coster(_.SigmaDslBuilderCoster))
}

