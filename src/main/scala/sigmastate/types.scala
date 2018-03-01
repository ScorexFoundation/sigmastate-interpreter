package sigmastate

import java.math.BigInteger
import java.util.Objects
import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.SType.TypeCode
import sigmastate.utxo.SigmaStateBox

/** Every type descriptor is a tree represented by nodes in SType hierarchy.
  * In order to extend type family:
  * - Implement concrete class derived from SType
  * - Implement serializer (see SCollectionSerializer) and register it in STypeSerializer.table
  * Each SType is serialized to array of bytes by:
  * - emitting typeCode of each node
  * - then recursively serializing subtrees from left to right on each level
  * */
sealed trait SType {
  type WrappedType
  val typeCode: SType.TypeCode

  def isPrimitive: Boolean = SType.allPrimitiveTypes.contains(this)
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
}

/** Primitive type recognizer to pattern match on TypeCode */
object PrimType {
  def unapply(tc: TypeCode): Option[SType] = SType.typeCodeToType.get(tc)
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
  override val typeCode = 1: Byte
}

case object SBigInt extends SType {
  override type WrappedType = BigInteger
  override val typeCode = 2: Byte
}

case object SBoolean extends SType {
  override type WrappedType = Boolean
  override val typeCode = 3: Byte
}

case object SByteArray extends SType {
  override type WrappedType = Array[Byte]
  override val typeCode = 4: Byte
}

case object SAvlTree extends SType {
  override type WrappedType = AvlTreeData
  override val typeCode = 5: Byte
}

case object SGroupElement extends SType {
  override type WrappedType = GroupElement
  override val typeCode: Byte = 6: Byte
}

case object SBox extends SType {
  override type WrappedType = SigmaStateBox
  override val typeCode: Byte = 7: Byte
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
  override val typeCode = SCollection.TypeCode

  override def equals(obj: scala.Any) = obj match {
    case that: SCollection[_] => that.elemType == elemType
    case _ => false
  }

  override def hashCode() = (31 + typeCode) * 31 + elemType.hashCode()
}

object SCollection {
  val TypeCode = 80: Byte
}

case class SFunc[Dom <: SType, Range <: SType](tDom: Dom,  tRange: Range) extends SType {
  override type WrappedType = tDom.WrappedType => tRange.WrappedType
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
}



