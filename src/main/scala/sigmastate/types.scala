package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.SType.TypeCode
import sigmastate.utils.Overloading.Overload1
import sigmastate.utxo.{SigmaStateBox, Box}
import sigmastate.Values._

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

  def isPrimitive: Boolean = SType.allPrimitiveTypes.contains(this)
  
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
  val allPrimitiveTypes = Seq(SInt, SBigInt, SBoolean, SByteArray, SAvlTree, SGroupElement, SBox, SUnit, SAny)
  val typeCodeToType = allPrimitiveTypes.map(t => t.typeCode -> t).toMap

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
  }

  def typeOfData(x: Any): SType = x match {
    case i: Int => SInt
    case l: Long => SInt
    case b: Boolean => SBoolean
    case arr: Array[Byte] => SByteArray
    case g: GroupElement => SGroupElement
    case box: SigmaStateBox => SBox
    case _: Unit => SUnit
    case _ => sys.error(s"Don't know how to return SType for $x")
  }

}

/** Primitive type recognizer to pattern match on TypeCode */
object PrimType {
  def unapply(tc: TypeCode): Option[SType] = SType.typeCodeToType.get(tc)
  def unapply(t: SType): Option[SType] = SType.allPrimitiveTypes.find(_ == t)
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

case object SInt extends SType {
  override type WrappedType = Long
  override val typeCode: TypeCode = 1: Byte
}

case object SBigInt extends SType {
  override type WrappedType = BigInteger
  override val typeCode: TypeCode = 2: Byte
}

case object SBoolean extends SType {
  override type WrappedType = Boolean
  override val typeCode: TypeCode = 3: Byte
}

case object SByteArray extends SType {
  override type WrappedType = Array[Byte]
  override val typeCode: TypeCode = 4: Byte
}

case object SAvlTree extends SType {
  override type WrappedType = AvlTreeData
  override val typeCode: TypeCode = 5: Byte
}

case object SGroupElement extends SType {
  override type WrappedType = GroupElement
  override val typeCode: TypeCode = 6: Byte
}

case object SBox extends SType {
  override type WrappedType = SigmaStateBox
  override val typeCode: TypeCode = 7: Byte
}

/** The type with single inhabitant value () */
case object SUnit extends SType {
  override type WrappedType = Unit
  override val typeCode: Byte = 8: Byte
}

/** Any other type is implicitly subtype of this type. */
case object SAny extends SType {
  override type WrappedType = Any
  override val typeCode: Byte = 9: Byte
}

case class SCollection[ElemType <: SType]()(implicit val elemType: ElemType) extends SType {
  override type WrappedType = IndexedSeq[Value[ElemType]]
  override val typeCode: TypeCode = SCollection.TypeCode

  override def equals(obj: scala.Any) = obj match {
    case that: SCollection[_] => that.elemType == elemType
    case _ => false
  }

  override def hashCode() = (31 + typeCode) * 31 + elemType.hashCode()
}

object SCollection {
  val TypeCode: TypeCode = 80: Byte
  def apply[T <: SType](elemType: T)(implicit ov: Overload1): SCollection[T] = SCollection()(elemType)
}

case class SFunc(tDom: IndexedSeq[SType],  tRange: SType) extends SType {
  override type WrappedType = Seq[Any] => tRange.WrappedType
  override val typeCode = SFunc.TypeCode
//  override def equals(obj: scala.Any) = obj match {
//    case that: SFunc[_,_] => that.tDom == tDom && that.tRange == tRange
//    case _ => false
//  }
//  override def hashCode() = ((31 + typeCode) * 31 + tDom.hashCode()) * 31 + tRange.hashCode()
}

object SFunc {
  val TypeCode = 90: Byte
}

case class STuple(items: IndexedSeq[SType]) extends SType {
  override type WrappedType = Seq[Any]
  override val typeCode = STuple.TypeCode
}

object STuple {
  val TypeCode = 100: Byte
  def apply(items: SType*): STuple = STuple(items.toIndexedSeq)
}

case class STypeApply(name: String, args: IndexedSeq[SType] = IndexedSeq()) extends SType {
  override type WrappedType = Any
  override val typeCode = STypeApply.TypeCode
}
object STypeApply {
  val TypeCode = 110: Byte
}

