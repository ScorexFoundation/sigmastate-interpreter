package sigmastate

import java.lang.reflect.Method
import java.math.BigInteger

import org.ergoplatform._
import org.ergoplatform.validation._
import scalan.{Nullable, RType}
import scalan.RType.GeneralType
import sigmastate.SType.{TypeCode, AnyOps}
import sigmastate.interpreter._
import sigmastate.utils.Overloading.Overload1
import sigmastate.utils.SparseArrayContainer
import scalan.util.Extensions._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{Lookup, Insert, Update, Remove}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate.lang.Terms.{MethodCall, _}
import sigmastate.lang.{SigmaBuilder, SigmaTyper}
import sigmastate.SCollection._
import sigmastate.interpreter.CryptoConstants.{EcPointType, hashLength}
import sigmastate.serialization.OpCodes
import special.collection.Coll
import special.sigma._
import sigmastate.eval.RuntimeCosting

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import sigmastate.SMethod.{InvokeDescBuilder, MethodCostFunc, givenCost, javaMethodOf, MethodCallIrBuilder}
import sigmastate.utxo.ByIndex
import sigmastate.utxo.ExtractCreationInfo
import sigmastate.utxo._
import special.sigma.{Header, Box, SigmaProp, AvlTree, SigmaDslBuilder, PreHeader}
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.eval.Evaluation.stypeToRType
import sigmastate.eval._
import sigmastate.lang.exceptions.MethodNotFound
import spire.syntax.all.cfor

import scala.collection.mutable
import scala.util.{Success, Failure}

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
  /** The underlying Scala type of data values described by this type descriptor.
    * E.g. scala.Int for SInt descriptor.
    */
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
    * This is useful for optimizations of calculating sizes of collections.
    * The method should have O(1) amortized complexity over n invocations to avoid
    * over-cost attacks on ErgoTree interpretation.
    */
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

  /** Costructs a collection type with the given type of elements. */
  implicit def typeCollection[V <: SType](implicit tV: V): SCollection[V] = SCollection[V](tV)

  /** RType descriptors for predefined types used in AOTC-based interpreter. */
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
  val paramIVSeq: Seq[STypeParam] = Array(paramIV)

  val IndexedSeqOfT1: IndexedSeq[SType] = Array(SType.tT)
  val IndexedSeqOfT2: IndexedSeq[SType] = Array(SType.tT, SType.tT)

  /** Immutable empty array, can be used to avoid repeated allocations. */
  val EmptyArray = Array.empty[SType]

  /** Immutable empty IndexedSeq, can be used to avoid repeated allocations. */
  val EmptySeq: IndexedSeq[SType] = EmptyArray

  /** All pre-defined types should be listed here. Note, NoType is not listed.
    * Should be in sync with sigmastate.lang.Types.predefTypes. */
  val allPredefTypes: Seq[SType] = Array(SBoolean, SByte, SShort, SInt, SLong, SBigInt, SContext,
    SGlobal, SHeader, SPreHeader, SAvlTree, SGroupElement, SSigmaProp, SString, SBox,
    SUnit, SAny)

  /** A mapping of object types supporting MethodCall operations. For each serialized
    * typeId this map contains a companion object which can be used to access the list of
    * corresponding methods.
    *
    * NOTE: in the current implementation only monomorphic methods are supported (without
    * type parameters)
    *
    * NOTE2: in v3.x SNumericType.typeId is silently shadowed by SGlobal.typeId as part of
    * `toMap` operation. As a result, the methods collected into SByte.methods cannot be
    * resolved (using SMethod.fromIds()) for all numeric types (SByte, SShort, SInt,
    * SLong, SBigInt). See the corresponding regression `property("MethodCall on numerics")`.
    * However, this "shadowing" is not a problem since all casting methods are implemented
    * via Downcast, Upcast opcodes and the remaining `toBytes`, `toBits` methods are not
    * implemented at all.
    * In order to allow MethodCalls on numeric types in future versions the SNumericType.typeId
    * should be changed and SGlobal.typeId should be preserved. The regression tests in
    * `property("MethodCall Codes")` should pass.
    */
  // TODO v6.0 (h4): should contain all numeric types (including also SNumericType)
  //  to support method calls like 10.toByte which encoded as MethodCall with typeId = 4, methodId = 1
  //  see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/667
  lazy val types: Map[Byte, STypeCompanion] = Seq(
    SBoolean, SNumericType, SString, STuple, SGroupElement, SSigmaProp, SContext, SGlobal, SHeader, SPreHeader,
    SAvlTree, SBox, SOption, SCollection, SBigInt
  ).map { t => (t.typeId, t) }.toMap

  /** Checks that the type of the value corresponds to the descriptor `tpe`.
    * If the value has complex structure only root type constructor is checked.
    * NOTE, this method is used in ErgoTree evaluation to systematically check that each
    * tree node evaluates to a value of the expected type.
    * Shallow runtime checks are enough if:
    * 1) ErgoTree is well-typed, i.e. each sub-expression has correct types (agree with
    *    the argument type).
    * 2) `isValueOfType == true` for each tree leaf
    * 3) `isValueOfType == true` for each sub-expression
    *
    * @param value value to check type
    * @param tpe   type descriptor to check value against
    * @return true if the given `value` is of type tpe`
    */
  def isValueOfType[T <: SType](x: Any, tpe: T): Boolean = tpe match {
    case SBoolean => x.isInstanceOf[Boolean]
    case SByte => x.isInstanceOf[Byte]
    case SShort => x.isInstanceOf[Short]
    case SInt => x.isInstanceOf[Int]
    case SLong => x.isInstanceOf[Long]
    case SBigInt => x.isInstanceOf[BigInt]
    case SGroupElement => x.isInstanceOf[GroupElement]
    case SSigmaProp => x.isInstanceOf[SigmaProp]
    case SBox => x.isInstanceOf[Box]
    case _: SCollectionType[_] => x.isInstanceOf[Coll[_]]
    case _: SOption[_] => x.isInstanceOf[Option[_]]
    case t: STuple =>
      if (t.items.length == 2) x.isInstanceOf[Tuple2[_,_]]
      else sys.error(s"Unsupported tuple type $t")
    case tF: SFunc =>
      if (tF.tDom.length == 1) x.isInstanceOf[Function1[_,_]]
      else sys.error(s"Unsupported function type $tF")
    case SContext => x.isInstanceOf[Context]
    case SAvlTree => x.isInstanceOf[AvlTree]
    case SGlobal => x.isInstanceOf[SigmaDslBuilder]
    case SHeader => x.isInstanceOf[Header]
    case SPreHeader => x.isInstanceOf[PreHeader]
    case SUnit => x.isInstanceOf[Unit]
    case _ => sys.error(s"Unknown type $tpe")
  }

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

    /** Returns the [[ClassTag]] for the given [[SType]]. */
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
    /** Helper method to simplify type casts. */
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

  /** If this is SType instance then returns the name of the corresponding RType.
    * Otherwise returns the name of type companion object (e.g. SCollection).
    */
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

  private lazy val _methodsMap: Map[Byte, Map[Byte, SMethod]] = methods
    .groupBy(_.objType.typeId)
    .map { case (typeId, ms) => (typeId -> ms.map(m => m.methodId -> m).toMap) }

  /** Lookup method by its id in this type. */
  @inline def getMethodById(methodId: Byte): Option[SMethod] =
    _methodsMap.get(typeId) match {
      case Some(ms) => ms.get(methodId)
      case None => None
    }

  /** Lookup method in this type by method's id or throw ValidationException.
    * This method can be used in trySoftForkable section to either obtain valid method
    * or catch ValidatioinException which can be checked for soft-fork condition.
    * It delegate to getMethodById to lookup method.
    * @see getMethodById
    */
  def methodById(methodId: Byte): SMethod = {
    ValidationRules.CheckAndGetMethod(this, methodId)
  }

  /** Looks up the method descriptor by the method name. */
  def getMethodByName(name: String): SMethod = methods.find(_.name == name).get

  /** CosterFactory associated with this type. */
  def coster: Option[CosterFactory] = None

  /** Class which represents values of this type. When method call is executed, the corresponding method
    * of this class is invoked via reflection [[java.lang.reflect.Method]].invoke(). */
  def reprClass: Class[_]
}

/** Defines recognizer method which allows the derived object to be used in patterns
  * to recognize method descriptors by method name.
  * @see SCollecton
  */
trait MethodByNameUnapply extends STypeCompanion {
  def unapply(methodName: String): Option[SMethod] = methods.find(_.name == methodName)
}

/** Base trait for all types which have methods (and properties) */
trait SProduct extends SType {
  /** Returns -1 if `method` is not found. */
  def methodIndex(name: String): Int = methods.indexWhere(_.name == name)

  /** Returns true if this type has a method with the given name. */
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

  /** Finds a method descriptor [[SMethod]] for the given name. */
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
  * @param opDesc  optional operation descriptor
  * @param description  human readable description of the method
  * @param args         one item for each argument */
case class OperationInfo(opDesc: Option[ValueCompanion], description: String, args: Seq[ArgInfo]) {
  def isFrontendOnly: Boolean = opDesc.isEmpty
  def opTypeName: String = opDesc.map(_.typeName).getOrElse("(FRONTEND ONLY)")
}

object OperationInfo {
  /** Convenience factory method. */
  def apply(opDesc: ValueCompanion, description: String, args: Seq[ArgInfo]): OperationInfo =
    OperationInfo(Some(opDesc), description, args)
}

/** Meta information connecting SMethod with ErgoTree.
  * The optional builder is used by front-end ErgoScript compiler to replace method calls
  * with ErgoTree nodes. In many cases [[SMethod.MethodCallIrBuilder]] builder is used.
  * However there are specific cases where more complex builders are used, see for example
  * usage of `withIRInfo` in the declaration of [[SCollection.GetOrElseMethod]].
  * @param  irBuilder  optional method call recognizer and ErgoTree node builder.
  *                    When the partial function is defined on a tuple
  *                    (builder, obj, m, args, subst) it transforms it to a new ErgoTree
  *                    node, which is then used in the resuting ErgoTree coming out of
  *                    the ErgoScript compiler.
  * @param javaMethod  Java [[Method]] which should be used to evaluate
  *                    [[sigmastate.lang.Terms.MethodCall]] node of ErgoTree.
  * @param invokeDescsBuilder optional builder of additional type descriptors (see extraDescriptors)
  */
case class MethodIRInfo(
    irBuilder: Option[PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue]],
    javaMethod: Option[Method],
    invokeDescsBuilder: Option[InvokeDescBuilder]
)

/** Represents method descriptor.
  *
  * @param objType type or type constructor descriptor
  * @param name    method name
  * @param stype   method signature type,
  *                where `stype.tDom`` - argument type and
  *                `stype.tRange` - method result type.
  * @param methodId method code, it should be unique among methods of the same objType.
  * @param costKind cost descriptor for this method
  * @param irInfo  meta information connecting SMethod with ErgoTree (see [[MethodIRInfo]])
  * @param docInfo optional human readable method description data
  * @param costFunc optional specification of how the cost should be computed for the
  *                 given method call (See ErgoTreeEvaluator.calcCost method).
  */
case class SMethod(
    objType: STypeCompanion,
    name: String,
    stype: SFunc,
    methodId: Byte,
    costKind: CostKind,
    irInfo: MethodIRInfo,
    docInfo: Option[OperationInfo],
    costFunc: Option[MethodCostFunc]) {

  /** Operation descriptor of this method. */
  lazy val opDesc = MethodDesc(this)

  /** Finds and keeps the [[Method]] instance which corresponds to this method descriptor.
    * The lazy value is forced only if irInfo.javaMethod == None
    */
  lazy val javaMethod: Method = {
    irInfo.javaMethod.getOrElse {
      val paramTypes = stype.tDom.drop(1).map(t => t match {
        case _: STypeVar => classOf[AnyRef]
        case _: SFunc => classOf[_ => _]
        case _ => Evaluation.stypeToRType(t).classTag.runtimeClass
      }).toArray
      val m = objType.reprClass.getMethod(name, paramTypes:_*)
      m
    }
  }

  /** Additional type descriptors, which are necessary to perform invocation of Method
    * associated with this instance.
    * @see MethodCall.eval
    */
  lazy val extraDescriptors: Seq[RType[_]] = {
    irInfo.invokeDescsBuilder match {
      case Some(builder) =>
        builder(stype).map(Evaluation.stypeToRType)
      case None =>
        mutable.WrappedArray.empty[RType[_]]
    }
  }

  /** Invoke this method on the given object with the arguments.
    * This is used for methods with FixedCost costKind. */
  def invokeFixed(obj: Any, args: Array[Any])(implicit E: ErgoTreeEvaluator): AnyRef = {
    javaMethod.invoke(obj, args.asInstanceOf[Array[AnyRef]]:_*)
  }

  // TODO optimize: avoid lookup when this SMethod is created via `specializeFor`
  /** Return generic template of this method. */
  @inline final def genericMethod: SMethod = {
    objType.getMethodById(methodId).get
  }

  /** Returns Java refection [[Method]] which must be invoked to evaluate this method.
    * The method is resolved by its name using `name + "_eval"` naming convention.
    * @see `map_eval`, `flatMap_eval` and other `*_eval` methods.
    * @hotspot don't beautify the code */
  lazy val evalMethod: Method = {
    val argTypes = stype.tDom
    val nArgs = argTypes.length
    val paramTypes = new Array[Class[_]](nArgs + 2)
    paramTypes(0) = classOf[MethodCall]
    cfor(0)(_ < nArgs, _ + 1) { i =>
      paramTypes(i + 1) = argTypes(i) match {
        case _: STypeVar => classOf[AnyRef]
        case _: SFunc => classOf[_ => _]
        case _: SCollectionType[_] => classOf[Coll[_]]
        case _: SOption[_] => classOf[Option[_]]
        case t =>
          Evaluation.stypeToRType(t).classTag.runtimeClass
      }
    }
    paramTypes(paramTypes.length - 1) = classOf[ErgoTreeEvaluator]

    val methodName = name + "_eval"
    val m = try {
      objType.getClass().getMethod(methodName, paramTypes:_*)
    }
    catch { case e: MethodNotFound =>
      throw new RuntimeException(s"Cannot find eval method def $methodName(${Seq(paramTypes:_*)})", e)
    }
    m
  }

  /** Create a new instance with the given stype. */
  def withSType(newSType: SFunc): SMethod = copy(stype = newSType)

  /** Create a new instance with the given cost function. */
  def withCost(costFunc: MethodCostFunc): SMethod = copy(costFunc = Some(costFunc))

  /** Create a new instance in which the `stype` field transformed using
    * the given substitution. */
  def withConcreteTypes(subst: Map[STypeVar, SType]): SMethod =
    withSType(stype.withSubstTypes(subst).asFunc)

  /** Name of a language operation represented by this method. */
  def opName = objType.getClass.getSimpleName + "." + name

  /** Returns [[OperationId]] for AOT costing. */
  def opId: OperationId = {
    OperationId(opName, stype)
  }

  /** Specializes this instance by creating a new [[SMethod]] instance where signature
    * is specialized with respect to the given object and args types. It is used in
    * [[sigmastate.serialization.MethodCallSerializer]] `parse` method, so it is part of
    * consensus protocol.
    *
    * @param objTpe specific type of method receiver (aka object)
    * @param args   specific types of method arguments
    * @return new instance of method descriptor with specialized signature
    * @consensus
    */
  def specializeFor(objTpe: SType, args: Seq[SType]): SMethod = {
    SigmaTyper.unifyTypeLists(stype.tDom, objTpe +: args) match {
      case Some(subst) if subst.nonEmpty =>
        withConcreteTypes(subst)
      case _ => this
    }
  }

  /** Create a new instance with the given [[OperationInfo]] parameters. */
  def withInfo(opDesc: ValueCompanion, desc: String, args: ArgInfo*): SMethod = {
    this.copy(docInfo = Some(OperationInfo(opDesc, desc, ArgInfo("this", "this instance") +: args.toSeq)))
  }

  /** Create a new instance with the given [[OperationInfo]] parameters.
    * NOTE: opDesc parameter is not defined and falls back to None.
    */
  def withInfo(desc: String, args: ArgInfo*): SMethod = {
    this.copy(docInfo = Some(OperationInfo(None, desc, ArgInfo("this", "this instance") +: args.toSeq)))
  }

  /** Create a new instance with the given IR builder (aka MethodCall rewriter) parameter. */
  def withIRInfo(
      irBuilder: PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue],
      javaMethod: Method = null,
      invokeHandler: InvokeDescBuilder = null): SMethod = {
    this.copy(irInfo = MethodIRInfo(Some(irBuilder), Option(javaMethod), Option(invokeHandler)))
  }

  /** Lookup [[ArgInfo]] for the given argName or throw an exception. */
  def argInfo(argName: String): ArgInfo =
    docInfo.get.args.find(_.name == argName).get
}


object SMethod {
  type RCosted[A] = RuntimeCosting#RCosted[A]

  /** Type of functions used to assign cost to method call nodes.
    * For a function `f: (mc, obj, args) => cost` it is called before the evaluation of
    * the `mc` node with the given `obj` as method receiver and `args` as method
    * arguments.
    */
  abstract class MethodCostFunc extends Function4[ErgoTreeEvaluator, MethodCall, Any, Array[Any], CostDetails] {
    /**
      * The function returns an estimated cost of evaluation BEFORE actual evaluation of
      * the method. For this reason [[MethodCostFunc]] is not used for higher-order
      * operations like `Coll.map`, `Coll.filter` etc.
      */
    def apply(E: ErgoTreeEvaluator, mc: MethodCall, obj: Any, args: Array[Any]): CostDetails
  }

  /** Returns a cost function which always returs the given cost. */
  def givenCost(costKind: FixedCost): MethodCostFunc = new MethodCostFunc {
    override def apply(E: ErgoTreeEvaluator,
                       mc: MethodCall,
                       obj: Any, args: Array[Any]): CostDetails = {
      if (E.settings.costTracingEnabled)
        TracedCost(Array(FixedCostItem(MethodDesc(mc.method), costKind)))
      else
        GivenCost(costKind.cost)
    }
  }

  /** Returns a cost function which expects `obj` to be of `Coll[T]` type and
    * uses its length to compute SeqCostItem  */
  def perItemCost(costKind: PerItemCost): MethodCostFunc = new MethodCostFunc {
    override def apply(E: ErgoTreeEvaluator,
                       mc: MethodCall,
                       obj: Any, args: Array[Any]): CostDetails = obj match {
      case coll: Coll[a] =>
        if (E.settings.costTracingEnabled) {
          val desc = MethodDesc(mc.method)
          TracedCost(Array(SeqCostItem(desc, costKind, coll.length)))
        }
        else
          GivenCost(costKind.cost(coll.length))
      case _ =>
        ErgoTreeEvaluator.error(
          s"Invalid object $obj of method call $mc: Coll type is expected")
    }
  }

  /** Some runtime methods (like Coll.map, Coll.flatMap) require additional RType descriptors.
    * The builder can extract those descriptors from the given type of the method signature.
    */
  type InvokeDescBuilder = SFunc => Seq[SType]

  /** Return [[Method]] descriptor for the given `methodName` on the given `cT` type.
    * @param methodName the name of the method to lookup
    * @param cT the class where to search the methodName
    * @param cA1 the class of the method argument
    */
  def javaMethodOf[T, A1](methodName: String)
                         (implicit cT: ClassTag[T], cA1: ClassTag[A1]): Method =
    cT.runtimeClass.getMethod(methodName, cA1.runtimeClass)

  /** Return [[Method]] descriptor for the given `methodName` on the given `cT` type.
    * @param methodName the name of the method to lookup
    * @param cT the class where to search the methodName
    * @param cA1 the class of the method's first argument
    * @param cA2 the class of the method's second argument
    */
  def javaMethodOf[T, A1, A2]
        (methodName: String)
        (implicit cT: ClassTag[T], cA1: ClassTag[A1], cA2: ClassTag[A2]): Method =
    cT.runtimeClass.getMethod(methodName, cA1.runtimeClass, cA2.runtimeClass)

  /** Default fallback method call recognizer which builds MethodCall ErgoTree nodes. */
  val MethodCallIrBuilder: PartialFunction[(SigmaBuilder, SValue, SMethod, Seq[SValue], STypeSubst), SValue] = {
    case (builder, obj, method, args, tparamSubst) =>
      builder.mkMethodCall(obj, method, args.toIndexedSeq, tparamSubst)
  }

  /** Convenience factory method. */
  def apply(objType: STypeCompanion, name: String, stype: SFunc,
            methodId: Byte,
            costKind: CostKind): SMethod = {
    SMethod(
      objType, name, stype, methodId, costKind,
      MethodIRInfo(None, None, None), None, None)
  }

  /** Looks up [[SMethod]] instance for the given type and method ids.
    *
    * @param typeId   id of a type which can contain methods
    * @param methodId id of a method of the type given by `typeId`
    * @return an instance of [[SMethod]] which may contain generic type variables in the
    *         signature (see SMethod.stype). As a result `specializeFor` is called by
    *         deserializer to obtain monomorphic method descriptor.
    * @consensus this is method is used in [[sigmastate.serialization.MethodCallSerializer]]
    *            `parse` method and hence it is part of consensus protocol
    */
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

  /** Max possible number of primitive types. */
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

  /** Checks if the given name is a cast method name.
    * @return true if it is. */
  def isCastMethod (name: String): Boolean = castMethods.contains(name)

  /** Upcasts the given value of a smaller type to this larger type.
    * Corresponds to section 5.1.2 Widening Primitive Conversion of Java Language Spec.
    * @param n numeric value to be converted
    * @return a value of WrappedType of this type descriptor's instance.
    * @throw exception if `n` has actual type which is larger than this type.
    */
  def upcast(n: AnyVal): WrappedType

  /** Downcasts the given value of a larger type to this smaller type.
    * Corresponds to section 5.1.3 Narrowing Primitive Conversion of Java Language Spec.
    * @param n numeric value to be converted
    * @return a value of WrappedType of this type descriptor's instance.
    * @throw exception if the actual value of `i` cannot fit into this type.
    */
  def downcast(n: AnyVal): WrappedType

  /** Returns a type which is larger. */
  @inline def max(that: SNumericType): SNumericType =
    if (this.numericTypeIndex > that.numericTypeIndex) this else that

  /** Returns true if this numeric type is larger than that. */
  @inline final def >(that: SNumericType): Boolean = this.numericTypeIndex > that.numericTypeIndex

  /** Numeric types are ordered by the number of bytes to store the numeric values.
    * @return index in the array of all numeric types. */
  protected def numericTypeIndex: Int

  override def toString: String = this.getClass.getSimpleName
}
object SNumericType extends STypeCompanion {
  /** Array of all numeric types ordered by number of bytes in the representation. */
  final val allNumericTypes = Array(SByte, SShort, SInt, SLong, SBigInt)

  // TODO v6.0 (4h): this typeId is now shadowed by SGlobal.typeId
  //  see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/667
  override def typeId: TypeCode = 106: Byte

  /** Since this object is not used in SMethod instances. */
  override def reprClass: Class[_] = sys.error(s"Shouldn't be called.")

  /** Type variable used in generic signatures of method descriptors. */
  val tNum = STypeVar("TNum")

  /** Cost function which is assigned for numeric cast MethodCall nodes in ErgoTree.
    * It is called as part of MethodCall.eval method. */
  val costOfNumericCast: MethodCostFunc = new MethodCostFunc {
    override def apply(E: ErgoTreeEvaluator,
                       mc: MethodCall,
                       obj: Any,
                       args: Array[Any]): CostDetails = {
      val targetTpe = mc.method.stype.tRange
      val cast = getNumericCast(mc.obj.tpe, mc.method.name, targetTpe).get
      val costKind = if (cast == Downcast) Downcast.costKind else Upcast.costKind
      TracedCost(Array(TypeBasedCostItem(MethodDesc(mc.method), costKind, targetTpe)))
    }
  }

  /** The following SMethod instances are descriptors of methods available on all numeric
    * types.
    * @see `val methods` below
    * */
  val ToByteMethod: SMethod = SMethod(this, "toByte", SFunc(tNum, SByte), 1, null)
    .withCost(costOfNumericCast)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Byte}, throwing exception if overflow.")
  val ToShortMethod: SMethod = SMethod(this, "toShort", SFunc(tNum, SShort), 2, null)
    .withCost(costOfNumericCast)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Short}, throwing exception if overflow.")
  val ToIntMethod: SMethod = SMethod(this, "toInt", SFunc(tNum, SInt), 3, null)
    .withCost(costOfNumericCast)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Int}, throwing exception if overflow.")
  val ToLongMethod: SMethod = SMethod(this, "toLong", SFunc(tNum, SLong), 4, null)
    .withCost(costOfNumericCast)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{Long}, throwing exception if overflow.")
  val ToBigIntMethod: SMethod = SMethod(this, "toBigInt", SFunc(tNum, SBigInt), 5, null)
    .withCost(costOfNumericCast)
    .withInfo(PropertyCall, "Converts this numeric value to \\lst{BigInt}")

  /** Cost of: 1) creating Byte collection from a numeric value */
  val ToBytes_CostKind = FixedCost(JitCost(5))

  val ToBytesMethod: SMethod = SMethod(
    this, "toBytes", SFunc(tNum, SByteArray), 6, ToBytes_CostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall,
      """ Returns a big-endian representation of this numeric value in a collection of bytes.
        | For example, the \lst{Int} value \lst{0x12131415} would yield the
        | collection of bytes \lst{[0x12, 0x13, 0x14, 0x15]}.
          """.stripMargin)

  /** Cost of: 1) creating Boolean collection (one bool for each bit) from a numeric
    * value. */
  val ToBits_CostKind = FixedCost(JitCost(5))

  val ToBitsMethod: SMethod = SMethod(
    this, "toBits", SFunc(tNum, SBooleanArray), 7, ToBits_CostKind)
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

  /** Collection of names of numeric casting methods (like `toByte`, `toInt`, etc). */
  val castMethods: Array[String] =
    Array(ToByteMethod, ToShortMethod, ToIntMethod, ToLongMethod, ToBigIntMethod)
      .map(_.name)

  /** Checks the given name is numeric type cast method (like toByte, toInt, etc.).*/
  def isCastMethod(name: String): Boolean = castMethods.contains(name)

  /** Convert the given method to a cast operation from fromTpe to resTpe. */
  def getNumericCast(fromTpe: SType, methodName: String, resTpe: SType): Option[NumericCastCompanion] = (fromTpe, resTpe) match {
    case (from: SNumericType, to: SNumericType) if isCastMethod(methodName) =>
      val op = if (to > from) Upcast else Downcast
      Some(op)
    case _ => None  // the method in not numeric type cast
  }

}

/** Base type for SBoolean and SSigmaProp. */
trait SLogical extends SType {
}

/** Monomorphic type descriptor i.e. a type without generic parameters.
  * @see `SGenericType`
  */
trait SMonoType extends SType with STypeCompanion {
  /** Helper method to create method descriptors for properties (i.e. methods without args). */
  protected def propertyCall(name: String, tpeRes: SType, id: Byte, costKind: CostKind): SMethod =
    SMethod(this, name, SFunc(this, tpeRes), id, costKind)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall, "")

  /** Helper method to create method descriptors for properties (i.e. methods without args). */
  protected def property(name: String, tpeRes: SType, id: Byte, valueCompanion: ValueCompanion): SMethod =
    SMethod(this, name, SFunc(this, tpeRes), id, valueCompanion.costKind)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(valueCompanion, "")
}

/** Descriptor of ErgoTree type `Boolean` holding `true` or `false` values. */
case object SBoolean extends SPrimType with SEmbeddable with SLogical with SProduct with SMonoType {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 1: Byte
  override def typeId = typeCode
  override val reprClass: Class[_] = classOf[Boolean]

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

/** Descriptor of ErgoTree type `Byte` - 8-bit signed integer. */
case object SByte extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Byte
  override val typeCode: TypeCode = 2: Byte
  override val reprClass: Class[_] = classOf[Byte]
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = 1
  override def isConstantSize = true
  override protected def numericTypeIndex: Int = 0
  override def upcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case _ => sys.error(s"Cannot upcast value $v to the type $this")
  }
  override def downcast(v: AnyVal): Byte = v match {
    case b: Byte => b
    case s: Short => s.toByteExact
    case i: Int => i.toByteExact
    case l: Long => l.toByteExact
    case _ => sys.error(s"Cannot downcast value $v to the type $this")
  }
}

/** Descriptor of ErgoTree type `Short` - 16-bit signed integer. */
case object SShort extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Short
  override val typeCode: TypeCode = 3: Byte
  override val reprClass: Class[_] = classOf[Short]
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = 2
  override def isConstantSize = true
  override protected def numericTypeIndex: Int = 1
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

/** Descriptor of ErgoTree type `Int` - 32-bit signed integer. */
case object SInt extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Int
  override val typeCode: TypeCode = 4: Byte
  override val reprClass: Class[_] = classOf[Int]
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = 4
  override def isConstantSize = true
  override protected def numericTypeIndex: Int = 2
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

/** Descriptor of ErgoTree type `Long` - 64-bit signed integer. */
case object SLong extends SPrimType with SEmbeddable with SNumericType with SMonoType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 5: Byte
  override val reprClass: Class[_] = classOf[Long]
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = 8
  override def isConstantSize = true
  override protected def numericTypeIndex: Int = 3
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
  override val reprClass: Class[_] = classOf[BigInt]

  override def typeId = typeCode

  /** Type of Relation binary op like GE, LE, etc. */
  val RelationOpType = SFunc(Array(SBigInt, SBigInt), SBoolean)

  /** The maximum size of BigInteger value in byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxBigIntSizeInBytes.value

  override def dataSize(v: SType#WrappedType): Long = MaxSizeInBytes

  /** While the size of BigInteger values is limited by the available memory this is not the case with sigma BigInt.
    * In sigma we limit the size by the fixed constant and thus BigInt is a constant size type. */
  override def isConstantSize = true

  override protected def numericTypeIndex: Int = 4

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

  /** The following `modQ` methods are not fully implemented in v4.x and this descriptors.
    * This descritors are remain here in the code and are waiting for full implementation
    * is upcoming soft-forks at which point the cost parameters should be calculated and
    * changed.
    */
  val ModQMethod = SMethod(this, "modQ", SFunc(this, SBigInt), 1, FixedCost(JitCost(1)))
      .withInfo(ModQ, "Returns this \\lst{mod} Q, i.e. remainder of division by Q, where Q is an order of the cryprographic group.")
  val PlusModQMethod = SMethod(this, "plusModQ", SFunc(IndexedSeq(this, SBigInt), SBigInt), 2, FixedCost(JitCost(1)))
      .withInfo(ModQArithOp.PlusModQ, "Adds this number with \\lst{other} by module Q.", ArgInfo("other", "Number to add to this."))
  val MinusModQMethod = SMethod(this, "minusModQ", SFunc(IndexedSeq(this, SBigInt), SBigInt), 3, FixedCost(JitCost(1)))
      .withInfo(ModQArithOp.MinusModQ, "Subtracts \\lst{other} number from this by module Q.", ArgInfo("other", "Number to subtract from this."))
  val MultModQMethod = SMethod(this, "multModQ", SFunc(IndexedSeq(this, SBigInt), SBigInt), 4, FixedCost(JitCost(1)))
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

/** Descriptor of type `String` which is not used in ErgoTree, but used in ErgoScript.
  * NOTE: this descriptor both type and type companion */
case object SString extends SProduct with SMonoType {
  override type WrappedType = String
  override val typeCode: TypeCode = 102: Byte
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = v.asInstanceOf[String].length
  override def isConstantSize = false
  override def reprClass: Class[_] = classOf[String]
}

/** Descriptor of ErgoTree type `GroupElement`.
  * NOTE: this descriptor both type and type companion */
case object SGroupElement extends SProduct with SPrimType with SEmbeddable with SMonoType {
  override type WrappedType = GroupElement
  override val typeCode: TypeCode = 7: Byte
  override val reprClass: Class[_] = classOf[GroupElement]

  override def typeId = typeCode
  override def coster: Option[CosterFactory] = Some(Coster(_.GroupElementCoster))

  /** Cost of: 1) serializing EcPointType to bytes 2) packing them in Coll. */
  val GetEncodedCostKind = FixedCost(JitCost(250))

  /** The following SMethod instances are descriptors of methods defined in `GroupElement` type. */
  lazy val GetEncodedMethod: SMethod = SMethod(
    this, "getEncoded", SFunc(Array(this), SByteArray), 2, GetEncodedCostKind)
    .withIRInfo(MethodCallIrBuilder)
    .withInfo(PropertyCall, "Get an encoding of the point value.")

  lazy val ExponentiateMethod: SMethod = SMethod(
    this, "exp", SFunc(Array(this, SBigInt), this), 3, Exponentiate.costKind)
    .withIRInfo({ case (builder, obj, _, Seq(arg), _) =>
      builder.mkExponentiate(obj.asGroupElement, arg.asBigInt)
    })
    .withInfo(Exponentiate,
      "Exponentiate this \\lst{GroupElement} to the given number. Returns this to the power of k",
      ArgInfo("k", "The power"))

  lazy val MultiplyMethod: SMethod = SMethod(
    this, "multiply", SFunc(Array(this, SGroupElement), this), 4, MultiplyGroup.costKind)
    .withIRInfo({ case (builder, obj, _, Seq(arg), _) =>
      builder.mkMultiplyGroup(obj.asGroupElement, arg.asGroupElement)
    })
    .withInfo(MultiplyGroup, "Group operation.", ArgInfo("other", "other element of the group"))

  /** Cost of: 1) calling EcPoint.negate 2) wrapping in GroupElement. */
  val Negate_CostKind = FixedCost(JitCost(45))

  lazy val NegateMethod: SMethod = SMethod(
    this, "negate", SFunc(this, this), 5, Negate_CostKind)
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

/** Descriptor of ErgoTree type `SigmaProp` which represent sigma-protocol propositions. */
case object SSigmaProp extends SProduct with SPrimType with SEmbeddable with SLogical with SMonoType {
  import SType._
  override type WrappedType = SigmaProp
  override val typeCode: TypeCode = 8: Byte
  override val reprClass: Class[_] = classOf[SigmaProp]
  override def typeId = typeCode

  /** The maximum size of SigmaProp value in serialized byte array representation. */
  val MaxSizeInBytes: Long = SigmaConstants.MaxSigmaPropSizeInBytes.value

  override def dataSize(v: SType#WrappedType): Long = MaxSizeInBytes

  override def isConstantSize = true
  val PropBytes = "propBytes"
  val IsProven = "isProven"
  lazy val PropBytesMethod = SMethod(
    this, PropBytes, SFunc(this, SByteArray), 1, SigmaPropBytes.costKind)
    .withInfo(SigmaPropBytes, "Serialized bytes of this sigma proposition taken as ErgoTree.")

  lazy val IsProvenMethod = SMethod(this, IsProven, SFunc(this, SBoolean), 2, null)
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
  override def toString = s"Option[$elemType]"
  override def toTermString: String = s"Option[${elemType.toTermString}]"

  override lazy val typeParams: Seq[STypeParam] = Array(SType.paramT)
}

object SOption extends STypeCompanion {
  /** Code of `Option[_]` type constructor. */
  val OptionTypeConstrId = 3
  /** Type code for `Option[T] for some T` type used in TypeSerializer. */
  val OptionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionTypeConstrId).toByte
  /** Code of `Option[Coll[_]]` type constructor. */
  val OptionCollectionTypeConstrId = 4
  /** Type code for `Option[Coll[T]] for some T` type used in TypeSerializer. */
  val OptionCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * OptionCollectionTypeConstrId).toByte

  override def typeId = OptionTypeCode

  override val coster: Option[CosterFactory] = Some(Coster(_.OptionCoster))
  override val reprClass: Class[_] = classOf[Option[_]]

  type SBooleanOption      = SOption[SBoolean.type]
  type SByteOption         = SOption[SByte.type]
  type SShortOption        = SOption[SShort.type]
  type SIntOption          = SOption[SInt.type]
  type SLongOption         = SOption[SLong.type]
  type SBigIntOption       = SOption[SBigInt.type]
  type SGroupElementOption = SOption[SGroupElement.type]
  type SBoxOption          = SOption[SBox.type]
  type SAvlTreeOption      = SOption[SAvlTree.type]

  /** This descriptors are instantiated once here and then reused. */
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

  /** Type descriptor of `this` argument used in the methods below. */
  val ThisType = SOption(tT)

  /** The following SMethod instances are descriptors of methods defined in `Option` type. */
  val IsDefinedMethod = SMethod(
    this, IsDefined, SFunc(ThisType, SBoolean), 2, OptionIsDefined.costKind)
      .withInfo(OptionIsDefined,
        "Returns \\lst{true} if the option is an instance of \\lst{Some}, \\lst{false} otherwise.")

  val GetMethod = SMethod(this, Get, SFunc(ThisType, tT), 3, OptionGet.costKind)
      .withInfo(OptionGet,
      """Returns the option's value. The option must be nonempty. Throws exception if the option is empty.""")

  lazy val GetOrElseMethod = SMethod(
    this, GetOrElse, SFunc(Array(ThisType, tT), tT, Array[STypeParam](tT)), 4, OptionGetOrElse.costKind)
      .withInfo(OptionGetOrElse,
        """Returns the option's value if the option is nonempty, otherwise
         |return the result of evaluating \lst{default}.
        """.stripMargin, ArgInfo("default", "the default value"))

// TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
//  val FoldMethod = SMethod(
//    this, Fold, SFunc(Array(ThisType, tR, SFunc(tT, tR)), tR, Array[STypeParam](tT, tR)), 5, FixedCost(JitCost(1)))
//      .withInfo(MethodCall,
//        """Returns the result of applying \lst{f} to this option's
//         |  value if the option is nonempty.  Otherwise, evaluates
//         |  expression \lst{ifEmpty}.
//         |  This is equivalent to \lst{option map f getOrElse ifEmpty}.
//        """.stripMargin,
//        ArgInfo("ifEmpty", "the expression to evaluate if empty"),
//        ArgInfo("f", "the function to apply if nonempty"))

  val MapMethod = SMethod(this, "map",
    SFunc(Array(ThisType, SFunc(tT, tR)), SOption(tR), Array(paramT, paramR)), 7, FixedCost(JitCost(20)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """Returns a \lst{Some} containing the result of applying \lst{f} to this option's
         |   value if this option is nonempty.
         |   Otherwise return \lst{None}.
        """.stripMargin, ArgInfo("f", "the function to apply"))

  val FilterMethod = SMethod(this, "filter",
    SFunc(Array(ThisType, SFunc(tT, SBoolean)), ThisType, Array(paramT)), 8, FixedCost(JitCost(20)))
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
}

/** Base class for descriptors of `Coll[T]` ErgoTree type for some elemType T. */
trait SCollection[T <: SType] extends SProduct with SGenericType {
  def elemType: T
  override type WrappedType = Coll[T#WrappedType]
  override def isConstantSize = false
}

/** Descriptor of `Coll[T]` ErgoTree type for some elemType T. */
case class SCollectionType[T <: SType](elemType: T) extends SCollection[T] {
  override val typeCode: TypeCode = SCollectionType.CollectionTypeCode

  override def dataSize(v: SType#WrappedType): Long = {
    val coll = v.asInstanceOf[Coll[T#WrappedType]]
    implicit val sT = Sized.typeToSized(Evaluation.stypeToRType(elemType))
    Sized.sizeOf(coll).dataSize
  }
  override def typeParams: Seq[STypeParam] = SCollectionType.typeParams
  protected override def getMethods() = super.getMethods() ++ SCollection.methods
  override def toString = s"Coll[$elemType]"
  override def toTermString = s"Coll[${elemType.toTermString}]"
}

object SCollectionType {
  /** Code of `Coll[_]` type constructor. */
  val CollectionTypeConstrId = 1

  /** Type code for `Coll[T] for some T` type used in TypeSerializer. */
  val CollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * CollectionTypeConstrId).toByte

  /** Code of `Coll[Coll[_]]` type constructor. */
  val NestedCollectionTypeConstrId = 2

  /** Type code for `Coll[Coll[T]] for some T` type used in TypeSerializer. */
  val NestedCollectionTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * NestedCollectionTypeConstrId).toByte

  /** Array of generic type parameters reused in all SCollectionType instances. */
  val typeParams: Seq[STypeParam] = Array(SType.paramIV)
}

object SCollection extends STypeCompanion with MethodByNameUnapply {
  override val reprClass: Class[_] = classOf[Coll[_]]
  override def typeId = SCollectionType.CollectionTypeCode
  override def coster: Option[CosterFactory] = Some(Coster(_.CollCoster))

  import SType.{tK, tV, paramIV, paramIVSeq, paramOV}

  /** Helper descriptors reused across different method descriptors. */
  def tIV = SType.tIV
  def tOV = SType.tOV

  /** This descriptors are instantiated once here and then reused. */
  val ThisType = SCollection(tIV)
  val tOVColl = SCollection(tOV)
  val tPredicate = SFunc(tIV, SBoolean)

  /** The following SMethod instances are descriptors of methods defined in `Coll` type. */
  val SizeMethod = SMethod(this, "size", SFunc(ThisType, SInt), 1, SizeOf.costKind)
      .withInfo(SizeOf, "The size of the collection in elements.")

  val GetOrElseMethod = SMethod(
    this, "getOrElse", SFunc(Array(ThisType, SInt, tIV), tIV, paramIVSeq), 2, ByIndex.costKind)
      .withIRInfo({ case (builder, obj, _, Seq(index, defaultValue), _) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        builder.mkByIndex(obj.asValue[SCollection[SType]], index1, Some(defaultValue1))
      })
      .withInfo(ByIndex, "Return the element of collection if \\lst{index} is in range \\lst{0 .. size-1}",
        ArgInfo("index", "index of the element of this collection"),
        ArgInfo("default", "value to return when \\lst{index} is out of range"))

  val MapMethod = SMethod(this, "map",
    SFunc(Array(ThisType, SFunc(tIV, tOV)), tOVColl, Array(paramIV, paramOV)), 3, MapCollection.costKind)
      .withInfo(MapCollection,
        """ Builds a new collection by applying a function to all elements of this collection.
         | Returns a new collection of type \lst{Coll[B]} resulting from applying the given function
         | \lst{f} to each element of this collection and collecting the results.
        """.stripMargin,
        ArgInfo("f", "the function to apply to each element"))

  /** Implements evaluation of Coll.map method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def map_eval[A,B](mc: MethodCall, xs: Coll[A], f: A => B)(implicit E: ErgoTreeEvaluator): Coll[B] = {
    val tpeB = mc.tpe.asInstanceOf[SCollection[SType]].elemType
    val tB = Evaluation.stypeToRType(tpeB).asInstanceOf[RType[B]]
    E.addSeqCostNoOp(MapCollection.costKind, xs.length, mc.method.opDesc)
    xs.map(f)(tB)
  }

  val ExistsMethod = SMethod(this, "exists",
    SFunc(Array(ThisType, tPredicate), SBoolean, paramIVSeq), 4, Exists.costKind)
      .withInfo(Exists,
        """Tests whether a predicate holds for at least one element of this collection.
         |Returns \lst{true} if the given predicate \lst{p} is satisfied by at least one element of this collection, otherwise \lst{false}
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements"))

  val FoldMethod = SMethod(
    this, "fold",
    SFunc(Array(ThisType, tOV, SFunc(Array(tOV, tIV), tOV)), tOV, Array(paramIV, paramOV)),
    5, Fold.costKind)
      .withInfo(Fold, "Applies a binary operator to a start value and all elements of this collection, going left to right.",
        ArgInfo("zero", "a starting value"),
        ArgInfo("op", "the binary operator"))

  val ForallMethod = SMethod(this, "forall",
    SFunc(Array(ThisType, tPredicate), SBoolean, paramIVSeq), 6, ForAll.costKind)
      .withInfo(ForAll,
        """Tests whether a predicate holds for all elements of this collection.
         |Returns \lst{true} if this collection is empty or the given predicate \lst{p}
         |holds for all elements of this collection, otherwise \lst{false}.
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements"))

  val SliceMethod = SMethod(this, "slice",
    SFunc(Array(ThisType, SInt, SInt), ThisType, paramIVSeq), 7, Slice.costKind)
      .withInfo(Slice,
        """Selects an interval of elements.  The returned collection is made up
         |  of all elements \lst{x} which satisfy the invariant:
         |  \lst{
         |     from <= indexOf(x) < until
         |  }
        """.stripMargin,
        ArgInfo("from", "the lowest index to include from this collection"),
        ArgInfo("until", "the lowest index to EXCLUDE from this collection"))

  val FilterMethod = SMethod(this, "filter",
    SFunc(Array(ThisType, tPredicate), ThisType, paramIVSeq), 8, Filter.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(l), _) => builder.mkFilter(obj.asValue[SCollection[SType]], l.asFunc)
      })
      .withInfo(Filter,
        """Selects all elements of this collection which satisfy a predicate.
         | Returns  a new collection consisting of all elements of this collection that satisfy the given
         | predicate \lst{p}. The order of the elements is preserved.
        """.stripMargin,
        ArgInfo("p", "the predicate used to test elements."))

  val AppendMethod = SMethod(this, "append",
    SFunc(Array(ThisType, ThisType), ThisType, paramIVSeq), 9, Append.costKind)
      .withIRInfo({
        case (builder, obj, _, Seq(xs), _) =>
          builder.mkAppend(obj.asCollection[SType], xs.asCollection[SType])
      })
      .withInfo(Append, "Puts the elements of other collection after the elements of this collection (concatenation of 2 collections)",
        ArgInfo("other", "the collection to append at the end of this"))

  val ApplyMethod = SMethod(this, "apply",
    SFunc(Array(ThisType, SInt), tIV, Array[STypeParam](tIV)), 10, ByIndex.costKind)
      .withInfo(ByIndex,
        """The element at given index.
         | Indices start at \lst{0}; \lst{xs.apply(0)} is the first element of collection \lst{xs}.
         | Note the indexing syntax \lst{xs(i)} is a shorthand for \lst{xs.apply(i)}.
         | Returns the element at the given index.
         | Throws an exception if \lst{i < 0} or \lst{length <= i}
        """.stripMargin, ArgInfo("i", "the index"))

  /** Cost of creating a collection of indices */
  val IndicesMethod_CostKind = PerItemCost(
    baseCost = JitCost(20), perChunkCost = JitCost(2), chunkSize = 16)

  val IndicesMethod = SMethod(
    this, "indices", SFunc(ThisType, SCollection(SInt)), 14, IndicesMethod_CostKind)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """Produces the range of all indices of this collection as a new collection
         | containing [0 .. length-1] values.
        """.stripMargin)

  /** Implements evaluation of Coll.indices method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def indices_eval[A, B](mc: MethodCall, xs: Coll[A])
                        (implicit E: ErgoTreeEvaluator): Coll[Int] = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.indices
    }
  }
  /** BaseCost:
    * 1) base cost of Coll.flatMap
    * PerChunkCost:
    * 1) cost of Coll.flatMap (per item)
    * 2) new collection is allocated for each item
    * 3) each collection is then appended to the resulting collection */
  val FlatMapMethod_CostKind = PerItemCost(
    baseCost = JitCost(30), perChunkCost = JitCost(5), chunkSize = 8)

  val FlatMapMethod = SMethod(this, "flatMap",
    SFunc(Array(ThisType, SFunc(tIV, tOVColl)), tOVColl, Array(paramIV, paramOV)),
    15, FlatMapMethod_CostKind)
      .withIRInfo(
        MethodCallIrBuilder,
        javaMethodOf[Coll[_], Function1[_,_], RType[_]]("flatMap"),
        { mtype => Array(mtype.tRange.asCollection[SType].elemType) })
      .withInfo(MethodCall,
        """ Builds a new collection by applying a function to all elements of this collection
         | and using the elements of the resulting collections.
         | Function \lst{f} is constrained to be of the form \lst{x => x.someProperty}, otherwise
         | it is illegal.
         | Returns a new collection of type \lst{Coll[B]} resulting from applying the given collection-valued function
         | \lst{f} to each element of this collection and concatenating the results.
        """.stripMargin, ArgInfo("f", "the function to apply to each element."))

  /** We assume all flatMap body patterns have similar executon cost. */
  final val CheckFlatmapBody_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(20), chunkSize = 1),
    NamedDesc("CheckFlatmapBody"))


  /** This patterns recognize all expressions, which are allowed as lambda body
    * of flatMap. Other bodies are rejected with throwing exception.
    */
  val flatMap_BodyPatterns = Array[PartialFunction[SValue, Int]](
    { case MethodCall(ValUse(id, tpe), m, args, _) if args.isEmpty => id },
    { case ExtractScriptBytes(ValUse(id, _)) => id },
    { case ExtractId(ValUse(id, _)) => id },
    { case SigmaPropBytes(ValUse(id, _)) => id },
    { case ExtractBytes(ValUse(id, _)) => id },
    { case ExtractBytesWithNoRef(ValUse(id, _)) => id }
  )

  /** Check the given expression is valid body of flatMap argument lambda.
    * @param varId id of lambda variable (see [[FuncValue]].args)
    * @param expr expression with is expected to use varId in ValUse node.
    * @return true if the body is allowed
    */
  def isValidPropertyAccess(varId: Int, expr: SValue)
                           (implicit E: ErgoTreeEvaluator): Boolean = {
    var found = false
    // NOTE: the cost depends on the position of the pattern since
    // we are checking until the first matching pattern found.
    E.addSeqCost(CheckFlatmapBody_Info) { () =>
      // the loop is bounded because flatMap_BodyPatterns is fixed
      var i = 0
      val nPatterns = flatMap_BodyPatterns.length
      while (i < nPatterns && !found) {
        val p = flatMap_BodyPatterns(i)
        found = p.lift(expr) match {
          case Some(id) => id == varId  // `id` in the pattern is equal to lambda `varId`
          case None => false
        }
        i += 1
      }
      i // how many patterns checked
    }
    found
  }

  /** Operation descriptor for matching `flatMap` method calls with valid lambdas. */
  final val MatchSingleArgMethodCall_Info = OperationCostInfo(
    FixedCost(JitCost(30)), NamedDesc("MatchSingleArgMethodCall"))

  /** Recognizer of `flatMap` method calls with valid lambdas. */
  object IsSingleArgMethodCall {
    def unapply(mc:MethodCall)
               (implicit E: ErgoTreeEvaluator): Nullable[(Int, SValue)] = {
      var res: Nullable[(Int, SValue)] = Nullable.None
      E.addFixedCost(MatchSingleArgMethodCall_Info) {
        res = mc match {
          case MethodCall(_, m, Seq(FuncValue(args, body)), _) if args.length == 1 =>
            val id = args(0)._1
            Nullable((id, body))
          case _ =>
            Nullable.None
        }
      }
      res
    }
  }

  /** Checks that the given [[MethodCall]] operation is valid flatMap. */
  def checkValidFlatmap(mc: MethodCall)(implicit E: ErgoTreeEvaluator) = {
    mc match {
      case IsSingleArgMethodCall(varId, lambdaBody)
            if isValidPropertyAccess(varId, lambdaBody) =>
        // ok, do nothing
      case _ =>
        ErgoTreeEvaluator.error(
          s"Unsupported lambda in flatMap: allowed usage `xs.flatMap(x => x.property)`: $mc")
    }
  }

  /** Implements evaluation of Coll.flatMap method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def flatMap_eval[A, B](mc: MethodCall, xs: Coll[A], f: A => Coll[B])
                        (implicit E: ErgoTreeEvaluator): Coll[B] = {
    checkValidFlatmap(mc)
    val m = mc.method
    var res: Coll[B] = null
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], m.opDesc) { () =>
      val tpeB = mc.tpe.asInstanceOf[SCollection[SType]].elemType
      val tB = Evaluation.stypeToRType(tpeB).asInstanceOf[RType[B]]
      res = xs.flatMap(f)(tB)
      res.length
    }
    res
  }

  val PatchMethod = SMethod(this, "patch",
    SFunc(Array(ThisType, SInt, ThisType, SInt), ThisType, paramIVSeq),
      19, PerItemCost(baseCost = JitCost(30), perChunkCost = JitCost(2), chunkSize = 10))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        "Produces a new Coll where a slice of elements in this Coll is replaced by another Coll.")

  /** Implements evaluation of Coll.patch method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def patch_eval[A](mc: MethodCall, xs: Coll[A], from: Int, patch: Coll[A], replaced: Int)
                   (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val m = mc.method
    val nItems = xs.length + patch.length
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], nItems, m.opDesc) { () =>
      xs.patch(from, patch, replaced)
    }
  }

  val UpdatedMethod = SMethod(this, "updated",
    SFunc(Array(ThisType, SInt, tIV), ThisType, paramIVSeq),
    20, PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(1), chunkSize = 10))
      .withIRInfo(MethodCallIrBuilder, javaMethodOf[Coll[_], Int, Any]("updated"))
      .withInfo(MethodCall,
        "A copy of this Coll with one single replaced element.")

  /** Implements evaluation of Coll.updated method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def updated_eval[A](mc: MethodCall, coll: Coll[A], index: Int, elem: A)
                     (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val m = mc.method
    val costKind = m.costKind.asInstanceOf[PerItemCost]
    E.addSeqCost(costKind, coll.length, m.opDesc) { () =>
      coll.updated(index, elem)
    }
  }

  val UpdateManyMethod = SMethod(this, "updateMany",
    SFunc(Array(ThisType, SCollection(SInt), ThisType), ThisType, paramIVSeq),
    21, PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(2), chunkSize = 10))
      .withIRInfo(MethodCallIrBuilder).withInfo(MethodCall, "")

  /** Implements evaluation of Coll.updateMany method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def updateMany_eval[A](mc: MethodCall, coll: Coll[A], indexes: Coll[Int], values: Coll[A])
                        (implicit E: ErgoTreeEvaluator): Coll[A] = {
    val costKind = mc.method.costKind.asInstanceOf[PerItemCost]
    E.addSeqCost(costKind, coll.length, mc.method.opDesc) { () =>
      coll.updateMany(indexes, values)
    }
  }

  val IndexOfMethod = SMethod(this, "indexOf",
    SFunc(Array(ThisType, tIV, SInt), SInt, paramIVSeq),
      26, PerItemCost(baseCost = JitCost(20), perChunkCost = JitCost(10), chunkSize = 2))
      .withIRInfo(MethodCallIrBuilder, javaMethodOf[Coll[_], Any, Int]("indexOf"))
      .withInfo(MethodCall, "")

  // TODO mainnet v5.0: optimize using specialization for numeric and predefined types
  /** Implements evaluation of Coll.indexOf method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def indexOf_eval[A](mc: MethodCall, xs: Coll[A], elem: A, from: Int)
                     (implicit E: ErgoTreeEvaluator): Int = {
    val costKind = mc.method.costKind.asInstanceOf[PerItemCost]
    var res: Int = -1
    E.addSeqCost(costKind, mc.method.opDesc) { () =>
      // TODO v5.0: this loop is bounded when MaxCollSize limit is enforced
      val len = xs.length
      val start = math.max(from, 0)
      var i = start
      var different = true
      while (i < len && different) {
        different = !DataValueComparer.equalDataValues(xs(i), elem)
        i += 1
      }
      if (!different)
        res = i - 1
      i - start  // return number of performed iterations
    }
    res
  }

  /** Cost descriptor of Coll.zip operation. */
  val Zip_CostKind = PerItemCost(
    baseCost = JitCost(10), perChunkCost = JitCost(1), chunkSize = 10)

  val ZipMethod = SMethod(this, "zip",
    SFunc(Array(ThisType, tOVColl), SCollection(STuple(tIV, tOV)), Array[STypeParam](tIV, tOV)),
    29, Zip_CostKind)
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall, "")

  /** Implements evaluation of Coll.zip method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def zip_eval[A, B](mc: MethodCall, xs: Coll[A], ys: Coll[B])
                    (implicit E: ErgoTreeEvaluator): Coll[(A,B)] = {
    val m = mc.method
    E.addSeqCost(m.costKind.asInstanceOf[PerItemCost], xs.length, m.opDesc) { () =>
      xs.zip(ys)
    }
  }

  override lazy val methods: Seq[SMethod] = Seq(
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
    IndicesMethod,
    FlatMapMethod,
    PatchMethod,
    UpdatedMethod,
    UpdateManyMethod,
    IndexOfMethod,
    ZipMethod
  )

  /** Helper constructors. */
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

  /** This descriptors are instantiated once here and then reused. */
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

/** Type descriptor of tuple type. */
case class STuple(items: IndexedSeq[SType]) extends SCollection[SAny.type] {
  import STuple._
  override val typeCode = STuple.TupleTypeCode

  /** Lazily computed value representing true | false | none.
    * 0 - none, 1 - false, 2 - true
    */
  @volatile
  private var _isConstantSizeCode: Byte = 0.toByte

  /** use lazy pattern to support O(1) amortized complexity over n invocations.
    * Not thread safe!
    */
  override def isConstantSize: Boolean = {
    val code = _isConstantSizeCode
    if (code == 0) {
      val len = items.length
      var isConst: Boolean = true
      // looking for a first non-const item type, or run out of items
      cfor(0)(_ < len && isConst, _ + 1) { i =>
        val t = items(i)
        isConst = t.isConstantSize
      }
      if (isConst) {
        _isConstantSizeCode = 2
      } else {
        _isConstantSizeCode = 1
      }
      return isConst
    }
    code == 2.toByte
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
      SMethod(
        STuple, componentNameByIndex(i), SFunc(this, items(i)),
        (i + 1).toByte, SelectField.costKind)
    }
    colMethods ++ tupleMethods
  }

  override val typeParams = Nil

  override def toTermString = s"(${items.map(_.toTermString).mkString(",")})"
  override def toString = s"(${items.mkString(",")})"
}

object STuple extends STypeCompanion {
  /** Code of `(_, T) for some embeddable T` type constructor. */
  val Pair1TypeConstrId = 5
  /** Type code for `(E, T) for some embeddable T` type used in TypeSerializer. */
  val Pair1TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair1TypeConstrId).toByte

  /** Code of `(T, _) for some embeddable T` type constructor. */
  val Pair2TypeConstrId = 6
  /** Type code for `(T, E) for some embeddable T` type used in TypeSerializer. */
  val Pair2TypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * Pair2TypeConstrId).toByte
  val TripleTypeCode: TypeCode = Pair2TypeCode

  /** Type constructor code of symmetric pair `(T, T)` for some embeddable T. */
  val PairSymmetricTypeConstrId = 7
  /** Type code of symmetric pair `(T, T)` for some embeddable T. */
  val PairSymmetricTypeCode: TypeCode = ((SPrimType.MaxPrimTypeCode + 1) * PairSymmetricTypeConstrId).toByte
  val QuadrupleTypeCode: TypeCode = PairSymmetricTypeCode

  /** Type code of generic tuple type. */
  val TupleTypeCode = ((SPrimType.MaxPrimTypeCode + 1) * 8).toByte

  override def typeId = TupleTypeCode

  override val reprClass: Class[_] = classOf[Product2[_,_]]

  /** A list of Coll methods inherited from Coll type and available as method of tuple. */
  lazy val colMethods: Seq[SMethod] = {
    val subst = Map(SType.tIV -> SAny)
    // TODO: implement other methods
    val activeMethods = Set(1.toByte /*Coll.size*/, 10.toByte /*Coll.apply*/)
    SCollection.methods.filter(m => activeMethods.contains(m.methodId)).map { m =>
      m.copy(stype = SigmaTyper.applySubst(m.stype, subst).asFunc)
    }
  }

  override def methods: Seq[SMethod] = sys.error(s"Shouldn't be called.")

  /** Helper factory method. */
  def apply(items: SType*): STuple = STuple(items.toArray)

  private val MaxTupleLength: Int = SigmaConstants.MaxTupleLength.value
  private val componentNames = Array.tabulate(MaxTupleLength){ i => s"_${i + 1}" }

  /** Returns method name for the tuple component accessor (i.e. `_1`, `_2`, etc.) */
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

/** Type descriptor of lambda types. */
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
  override val typeParams: Seq[STypeParam] = tpeParams

  /** Generalize this type and return a new descriptor. */
  def getGenericType: SFunc = {
    val typeParams: Seq[STypeParam] = tDom.zipWithIndex
      .map { case (_, i) => STypeParam(SType.tD.name + (i + 1)) } :+ STypeParam(SType.tR.name)
    val ts = typeParams.map(_.ident)
    SFunc(ts.init.toIndexedSeq, ts.last, Nil)
  }

  /** Transform function into method type by adding the given `objType` as the first
    * argument type (aka method receiver type).
    */
  def withReceiverType(objType: SType) = this.copy(tDom = objType +: tDom)
}

object SFunc {
  final val FuncTypeCode: TypeCode = OpCodes.FirstFuncType
  def apply(tDom: SType, tRange: SType): SFunc = SFunc(Array(tDom), tRange) // HOTSPOT:
  val identity = { x: Any => x }
}

/** Used by ErgoScript compiler IR and eliminated during compilation.
  * It is not used in ErgoTree.
  */
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

/** Type variable which is used in generic method/func signatures.
  * Used by ErgoScript compiler IR and eliminated during compilation.
  * It is not used in ErgoTree.
  */
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

  /** Immutable empty array, can be used to avoid repeated allocations. */
  val EmptyArray = Array.empty[STypeVar]

  /** Immutable empty IndexedSeq, can be used to avoid repeated allocations. */
  val EmptySeq: IndexedSeq[STypeVar] = EmptyArray
}

/** Type descriptor of `Box` type of ErgoTree. */
case object SBox extends SProduct with SPredefType with SMonoType {
  import ErgoBox._
  override type WrappedType = Box
  override val typeCode: TypeCode = 99: Byte
  override val reprClass: Class[_] = classOf[Box]
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = {
    val box = v.asInstanceOf[this.WrappedType]
    Sized.sizeOf(box).dataSize
  }
  override def isConstantSize = false

  import SType.{tT, paramT}

  /** Defined once here and then reused in SMethod descriptors. */
  lazy val GetRegFuncType = SFunc(Array(SBox), SOption(tT), Array(paramT))

  /** Creates a descriptor for the given register method. (i.e. R1, R2, etc) */
  def registers(idOfs: Int): Seq[SMethod] = {
    allRegisters.map { i =>
      i match {
        case r: MandatoryRegisterId =>
          SMethod(this, s"R${i.asIndex}",
            GetRegFuncType, (idOfs + i.asIndex + 1).toByte, ExtractRegisterAs.costKind)
              .withInfo(ExtractRegisterAs, r.purpose)
        case _ =>
          SMethod(this, s"R${i.asIndex}",
            GetRegFuncType, (idOfs + i.asIndex + 1).toByte, ExtractRegisterAs.costKind)
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
  lazy val ValueMethod = SMethod(
    this, Value, SFunc(SBox, SLong), 1, ExtractAmount.costKind)
      .withInfo(ExtractAmount,
        "Mandatory: Monetary value, in Ergo tokens (NanoErg unit of measure)")

  lazy val PropositionBytesMethod = SMethod(
    this, PropositionBytes, SFunc(SBox, SByteArray), 2, ExtractScriptBytes.costKind)
      .withInfo(ExtractScriptBytes,
        "Serialized bytes of guarding script, which should be evaluated to true in order to\n" +
        " open this box. (aka spend it in a transaction)")

  lazy val BytesMethod = SMethod(
    this, Bytes, SFunc(SBox, SByteArray), 3, ExtractBytes.costKind)
      .withInfo(ExtractBytes, "Serialized bytes of this box's content, including proposition bytes.")

  lazy val BytesWithoutRefMethod = SMethod(
    this, BytesWithoutRef, SFunc(SBox, SByteArray), 4, ExtractBytesWithNoRef.costKind)
      .withInfo(ExtractBytesWithNoRef,
        "Serialized bytes of this box's content, excluding transactionId and index of output.")

  lazy val IdMethod = SMethod(this, Id, SFunc(SBox, SByteArray), 5, ExtractId.costKind)
      .withInfo(ExtractId,
        "Blake2b256 hash of this box's content, basically equals to \\lst{blake2b256(bytes)}")

  lazy val creationInfoMethod = SMethod(
    this, CreationInfo, ExtractCreationInfo.OpType, 6, ExtractCreationInfo.costKind)
      .withInfo(ExtractCreationInfo,
        """ If \lst{tx} is a transaction which generated this box, then \lst{creationInfo._1}
         | is a height of the tx's block. The \lst{creationInfo._2} is a serialized transaction
         | identifier followed by box index in the transaction outputs.
        """.stripMargin ) // see ExtractCreationInfo

  lazy val getRegMethod = SMethod(this, "getReg",
    SFunc(Array(SBox, SInt), SOption(tT), Array(paramT)), 7, ExtractRegisterAs.costKind)
      .withInfo(ExtractRegisterAs,
        """ Extracts register by id and type.
         | Type param \lst{T} expected type of the register.
         | Returns \lst{Some(value)} if the register is defined and has given type and \lst{None} otherwise
        """.stripMargin,
        ArgInfo("regId", "zero-based identifier of the register."))

  lazy val tokensMethod = SMethod(
    this, "tokens", SFunc(SBox, ErgoBox.STokensRegType), 8, FixedCost(JitCost(15)))
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

/** Type descriptor of `AvlTree` type of ErgoTree. */
case object SAvlTree extends SProduct with SPredefType with SMonoType {
  override type WrappedType = AvlTree
  override val typeCode: TypeCode = 100: Byte
  override val reprClass: Class[_] = classOf[AvlTree]
  override def typeId = typeCode
  override def dataSize(v: SType#WrappedType): Long = AvlTreeData.TreeDataSize
  override def isConstantSize = true

  import SOption._
  lazy val TCollOptionCollByte = SCollection(SByteArrayOption)
  lazy val CollKeyValue = SCollection(STuple(SByteArray, SByteArray))

  lazy val digestMethod = SMethod(this, "digest", SFunc(this, SByteArray), 1, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """Returns digest of the state represented by this tree.
         | Authenticated tree \lst{digest} = \lst{root hash bytes} ++ \lst{tree height}
        """.stripMargin)

  /** Cost descriptor of `digest` method. */
  lazy val digest_Info = {
    val m = digestMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val enabledOperationsMethod = SMethod(
    this, "enabledOperations", SFunc(this, SByte), 2, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """ Flags of enabled operations packed in single byte.
         | \lst{isInsertAllowed == (enabledOperations & 0x01) != 0}\newline
         | \lst{isUpdateAllowed == (enabledOperations & 0x02) != 0}\newline
         | \lst{isRemoveAllowed == (enabledOperations & 0x04) != 0}
        """.stripMargin)

  lazy val keyLengthMethod = SMethod(
    this, "keyLength", SFunc(this, SInt), 3, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
            """.stripMargin)

  lazy val valueLengthOptMethod = SMethod(
    this, "valueLengthOpt", SFunc(this, SIntOption), 4, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isInsertAllowedMethod = SMethod(
    this, "isInsertAllowed", SFunc(this, SBoolean), 5, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isInsertAllowed_Info = {
    val m = isInsertAllowedMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val isUpdateAllowedMethod = SMethod(
    this, "isUpdateAllowed", SFunc(this, SBoolean), 6, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isUpdateAllowed_Info = {
    val m = isUpdateAllowedMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val isRemoveAllowedMethod = SMethod(
    this, "isRemoveAllowed", SFunc(this, SBoolean), 7, FixedCost(JitCost(15)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(PropertyCall,
        """
         |
        """.stripMargin)

  lazy val isRemoveAllowed_Info = {
    val m = isRemoveAllowedMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

  lazy val updateOperationsMethod  = SMethod(this, "updateOperations",
    SFunc(Array(SAvlTree, SByte), SAvlTree), 8, FixedCost(JitCost(45)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |
        """.stripMargin)

  lazy val containsMethod = SMethod(this, "contains",
    SFunc(Array(SAvlTree, SByteArray, SByteArray), SBoolean), 9, DynamicCost)
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

  /** The proof may contain keys, labels and values, we don't know for sure how many,
    * but we assume the cost is O(proof.length).
    * So the following is an approximation of the proof parsing cost.
    */
  final val CreateAvlVerifier_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(110), perChunkCost = JitCost(20), chunkSize = 64),
    NamedDesc("CreateAvlVerifier"))

  final val LookupAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(40), perChunkCost = JitCost(10), chunkSize = 1),
    NamedDesc("LookupAvlTree"))

  final val InsertIntoAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(40), perChunkCost = JitCost(10), chunkSize = 1),
    NamedDesc("InsertIntoAvlTree"))

  final val UpdateAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(120), perChunkCost = JitCost(20), chunkSize = 1),
    NamedDesc("UpdateAvlTree"))

  final val RemoveAvlTree_Info = OperationCostInfo(
    PerItemCost(baseCost = JitCost(100), perChunkCost = JitCost(15), chunkSize = 1),
    NamedDesc("RemoveAvlTree"))

  /** Creates [[AvlTreeVerifier]] for the given tree and proof. */
  def createVerifier(tree: AvlTree, proof: Coll[Byte])(implicit E: ErgoTreeEvaluator) = {
    // the cost of tree reconstruction from proof is O(proof.length)
    E.addSeqCost(CreateAvlVerifier_Info, proof.length) { () =>
      tree.createVerifier(proof)
    }
  }

  /** Implements evaluation of AvlTree.contains method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def contains_eval(mc: MethodCall, tree: AvlTree, key: Coll[Byte], proof: Coll[Byte])
                   (implicit E: ErgoTreeEvaluator): Boolean = {
    val bv = createVerifier(tree, proof)
    val nItems = bv.treeHeight

    var res = false
    // the cost of tree lookup is O(bv.treeHeight)
    E.addSeqCost(LookupAvlTree_Info, nItems) { () =>
      res = bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
        case Success(r) => r match {
          case Some(_) => true
          case _ => false
        }
        case Failure(_) => false
      }
    }
    res
  }

  lazy val getMethod = SMethod(this, "get",
    SFunc(Array(SAvlTree, SByteArray, SByteArray), SByteArrayOption), 10, DynamicCost)
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

  /** Implements evaluation of AvlTree.get method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def get_eval(mc: MethodCall, tree: AvlTree, key: Coll[Byte], proof: Coll[Byte])
              (implicit E: ErgoTreeEvaluator): Option[Coll[Byte]] = {
    val bv = createVerifier(tree, proof)
    val nItems = bv.treeHeight

    // the cost of tree lookup is O(bv.treeHeight)
    E.addSeqCost(LookupAvlTree_Info, nItems) { () =>
      bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
        case Success(r) => r match {
          case Some(v) => Some(Colls.fromArray(v))
          case _ => None
        }
        case Failure(_) => Interpreter.error(s"Tree proof is incorrect $tree")
      }
    }
  }

  lazy val getManyMethod = SMethod(this, "getMany",
    SFunc(Array(SAvlTree, SByteArray2, SByteArray), TCollOptionCollByte), 11, DynamicCost)
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

  /** Implements evaluation of AvlTree.getMany method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def getMany_eval(mc: MethodCall, tree: AvlTree, keys: Coll[Coll[Byte]], proof: Coll[Byte])
                  (implicit E: ErgoTreeEvaluator): Coll[Option[Coll[Byte]]] = {
    val bv = createVerifier(tree, proof)
    val nItems = bv.treeHeight
    keys.map { key =>
      // the cost of tree lookup is O(bv.treeHeight)
      E.addSeqCost(LookupAvlTree_Info, nItems) { () =>
        bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
          case Success(r) => r match {
            case Some(v) => Some(Colls.fromArray(v))
            case _ => None
          }
          case Failure(_) => Interpreter.error(s"Tree proof is incorrect $tree")
        }
      }
    }
  }

  lazy val insertMethod = SMethod(this, "insert",
    SFunc(Array(SAvlTree, CollKeyValue, SByteArray), SAvlTreeOption), 12, DynamicCost)
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

  /** Implements evaluation of AvlTree.insert method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def insert_eval(mc: MethodCall, tree: AvlTree, entries: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte])
                 (implicit E: ErgoTreeEvaluator): Option[AvlTree] = {
    E.addCost(isInsertAllowed_Info)
    if (!tree.isInsertAllowed) {
      None
    } else {
      val bv = createVerifier(tree, proof)
      // when the tree is empty we still need to add the insert cost
      val nItems = Math.max(bv.treeHeight, 1)

      entries.forall { case (key, value) =>
        var res = true
        // the cost of tree lookup is O(bv.treeHeight)
        E.addSeqCost(InsertIntoAvlTree_Info, nItems) { () =>
          val insert = Insert(ADKey @@ key.toArray, ADValue @@ value.toArray)
          val insertRes = bv.performOneOperation(insert)
          // TODO v6.0: throwing exception is not consistent with update semantics
          //  however it preserves v4.0 semantics
          if (insertRes.isFailure) {
            Interpreter.error(s"Incorrect insert for $tree (key: $key, value: $value, digest: ${tree.digest}): ${insertRes.failed.get}}")
          }
          res = insertRes.isSuccess
        }
        res
      }
      bv.digest match {
        case Some(d) =>
          E.addCost(updateDigest_Info)
          Some(tree.updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  lazy val updateMethod = SMethod(this, "update",
    SFunc(Array(SAvlTree, CollKeyValue, SByteArray), SAvlTreeOption), 13, DynamicCost)
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

  /** Implements evaluation of AvlTree.update method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def update_eval(mc: MethodCall, tree: AvlTree,
                  operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte])
                 (implicit E: ErgoTreeEvaluator): Option[AvlTree] = {
    E.addCost(isUpdateAllowed_Info)
    if (!tree.isUpdateAllowed) {
      None
    } else {
      val bv = createVerifier(tree, proof)
      // when the tree is empty we still need to add the insert cost
      val nItems = Math.max(bv.treeHeight, 1)

      // here we use forall as looping with fast break on first failed tree oparation
      operations.forall { case (key, value) =>
        var res = true
        // the cost of tree update is O(bv.treeHeight)
        E.addSeqCost(UpdateAvlTree_Info, nItems) { () =>
          val op = Update(ADKey @@ key.toArray, ADValue @@ value.toArray)
          val updateRes = bv.performOneOperation(op)
          res = updateRes.isSuccess
        }
        res
      }
      bv.digest match {
        case Some(d) =>
          E.addCost(updateDigest_Info)
          Some(tree.updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  lazy val removeMethod = SMethod(this, "remove",
    SFunc(Array(SAvlTree, SByteArray2, SByteArray), SAvlTreeOption), 14, DynamicCost)
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

  /** Implements evaluation of AvlTree.remove method call ErgoTree node.
    * Called via reflection based on naming convention.
    * @see SMethod.evalMethod
    */
  def remove_eval(mc: MethodCall, tree: AvlTree,
                  operations: Coll[Coll[Byte]], proof: Coll[Byte])
                 (implicit E: ErgoTreeEvaluator): Option[AvlTree] = {
    E.addCost(isRemoveAllowed_Info)
    if (!tree.isRemoveAllowed) {
      None
    } else {
      val bv = createVerifier(tree, proof)
      // when the tree is empty we still need to add the insert cost
      val nItems = Math.max(bv.treeHeight, 1)

      cfor(0)(_ < operations.length, _ + 1) { i =>
        E.addSeqCost(RemoveAvlTree_Info, nItems) { () =>
          val key = operations(i).toArray
          bv.performOneOperation(Remove(ADKey @@ key))
        }
      }

      E.addCost(digest_Info)
      bv.digest match {
        case Some(d) =>
          E.addCost(updateDigest_Info)
          Some(tree.updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  lazy val updateDigestMethod = SMethod(this, "updateDigest",
    SFunc(Array(SAvlTree, SByteArray), SAvlTree), 15, FixedCost(JitCost(40)))
      .withIRInfo(MethodCallIrBuilder)
      .withInfo(MethodCall,
        """
         |
        """.stripMargin)

  lazy val updateDigest_Info = {
    val m = updateDigestMethod
    OperationCostInfo(m.costKind.asInstanceOf[FixedCost], m.opDesc)
  }

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

/** Type descriptor of `Context` type of ErgoTree. */
case object SContext extends SProduct with SPredefType with SMonoType {
  override type WrappedType = Context
  override val typeCode: TypeCode = 101: Byte

  override def reprClass: Class[_] = classOf[Context]

  override def typeId = typeCode

  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    sys.error(s"$this.dataSize($v) is not defined")
  }
  override def isConstantSize = false

  /** Arguments on context operation such as getVar, DeserializeContext etc.
    * This value can be reused where necessary to avoid allocations. */
  val ContextFuncDom: IndexedSeq[SType] = Array(SContext, SByte)

  import SType.{tT, paramT}

  lazy val dataInputsMethod = propertyCall("dataInputs", SBoxArray, 1, FixedCost(JitCost(15)))
  lazy val headersMethod    = propertyCall("headers", SHeaderArray, 2, FixedCost(JitCost(15)))
  lazy val preHeaderMethod  = propertyCall("preHeader", SPreHeader, 3, FixedCost(JitCost(15)))
  lazy val inputsMethod     = property("INPUTS", SBoxArray, 4, Inputs)
  lazy val outputsMethod    = property("OUTPUTS", SBoxArray, 5, Outputs)
  lazy val heightMethod     = property("HEIGHT", SInt, 6, Height)
  lazy val selfMethod       = property("SELF", SBox, 7, Self)
  lazy val selfBoxIndexMethod = propertyCall("selfBoxIndex", SInt, 8, FixedCost(JitCost(20)))
  lazy val lastBlockUtxoRootHashMethod = property("LastBlockUtxoRootHash", SAvlTree, 9, LastBlockUtxoRootHash)
  lazy val minerPubKeyMethod = property("minerPubKey", SByteArray, 10, MinerPubkey)
  lazy val getVarMethod = SMethod(
    this, "getVar", SFunc(ContextFuncDom, SOption(tT), Array(paramT)), 11, GetVar.costKind)
    .withInfo(GetVar, "Get context variable with given \\lst{varId} and type.",
      ArgInfo("varId", "\\lst{Byte} identifier of context variable"))

  protected override def getMethods() = super.getMethods() ++ Seq(
    dataInputsMethod, headersMethod, preHeaderMethod, inputsMethod, outputsMethod, heightMethod, selfMethod,
    selfBoxIndexMethod, lastBlockUtxoRootHashMethod, minerPubKeyMethod, getVarMethod
  )
  override val coster = Some(Coster(_.ContextCoster))
}

/** Type descriptor of `Header` type of ErgoTree. */
case object SHeader extends SProduct with SPredefType with SMonoType {
  override type WrappedType = Header
  override val typeCode: TypeCode = 104: Byte
  override val reprClass: Class[_] = classOf[Header]
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

  lazy val idMethod               = propertyCall("id", SByteArray, 1, FixedCost(JitCost(10)))
  lazy val versionMethod          = propertyCall("version",  SByte,      2, FixedCost(JitCost(10)))
  lazy val parentIdMethod         = propertyCall("parentId", SByteArray, 3, FixedCost(JitCost(10)))
  lazy val ADProofsRootMethod     = propertyCall("ADProofsRoot", SByteArray, 4, FixedCost(JitCost(10)))
  lazy val stateRootMethod        = propertyCall("stateRoot", SAvlTree, 5, FixedCost(JitCost(10)))
  lazy val transactionsRootMethod = propertyCall("transactionsRoot", SByteArray, 6, FixedCost(JitCost(10)))
  lazy val timestampMethod        = propertyCall("timestamp", SLong, 7, FixedCost(JitCost(10)))
  lazy val nBitsMethod            = propertyCall("nBits", SLong, 8, FixedCost(JitCost(10)))
  lazy val heightMethod           = propertyCall("height", SInt, 9, FixedCost(JitCost(10)))
  lazy val extensionRootMethod    = propertyCall("extensionRoot", SByteArray, 10, FixedCost(JitCost(10)))
  lazy val minerPkMethod          = propertyCall("minerPk", SGroupElement, 11, FixedCost(JitCost(10)))
  lazy val powOnetimePkMethod     = propertyCall("powOnetimePk", SGroupElement, 12, FixedCost(JitCost(10)))
  lazy val powNonceMethod         = propertyCall("powNonce", SByteArray, 13, FixedCost(JitCost(10)))
  lazy val powDistanceMethod      = propertyCall("powDistance", SBigInt, 14, FixedCost(JitCost(10)))
  lazy val votesMethod            = propertyCall("votes", SByteArray, 15, FixedCost(JitCost(10)))

  protected override def getMethods() = super.getMethods() ++ Seq(
    idMethod, versionMethod, parentIdMethod, ADProofsRootMethod, stateRootMethod, transactionsRootMethod,
    timestampMethod, nBitsMethod, heightMethod, extensionRootMethod, minerPkMethod, powOnetimePkMethod,
    powNonceMethod, powDistanceMethod, votesMethod
  )
  override val coster = Some(Coster(_.HeaderCoster))
}

/** Type descriptor of `PreHeader` type of ErgoTree. */
case object SPreHeader extends SProduct with SPredefType with SMonoType {
  override type WrappedType = PreHeader
  override val typeCode: TypeCode = 105: Byte
  override val reprClass: Class[_] = classOf[PreHeader]

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

  lazy val versionMethod          = propertyCall("version",  SByte,      1, FixedCost(JitCost(10)))
  lazy val parentIdMethod         = propertyCall("parentId", SByteArray, 2, FixedCost(JitCost(10)))
  lazy val timestampMethod        = propertyCall("timestamp", SLong, 3, FixedCost(JitCost(10)))
  lazy val nBitsMethod            = propertyCall("nBits", SLong, 4, FixedCost(JitCost(10)))
  lazy val heightMethod           = propertyCall("height", SInt, 5, FixedCost(JitCost(10)))
  lazy val minerPkMethod          = propertyCall("minerPk", SGroupElement, 6, FixedCost(JitCost(10)))
  lazy val votesMethod            = propertyCall("votes", SByteArray, 7, FixedCost(JitCost(10)))

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
  override val reprClass: Class[_] = classOf[SigmaDslBuilder]
  override def typeId = typeCode
  /** Approximate data size of the given context without ContextExtension. */
  override def dataSize(v: SType#WrappedType): Long = {
    sys.error(s"$this.dataSize($v) is not defined")
  }
  override def isConstantSize = true  // only fixed amount of global information is allowed

  import SType.tT

  lazy val groupGeneratorMethod = SMethod(
    this, "groupGenerator", SFunc(this, SGroupElement), 1, GroupGenerator.costKind)
    .withIRInfo({ case (builder, obj, method, args, tparamSubst) => GroupGenerator })
    .withInfo(GroupGenerator, "")

  lazy val xorMethod = SMethod(
    this, "xor", SFunc(Array(this, SByteArray, SByteArray), SByteArray), 2, Xor.costKind)
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

