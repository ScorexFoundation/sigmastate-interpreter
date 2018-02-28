package sigmastate

import java.math.BigInteger
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

  /** All primitive types should be listed here */
  val allPrimitiveTypes = Seq(SInt, SBigInt, SBoolean, SByteArray, SAvlTree, SGroupElement, SBox)
  val typeCodeToType = allPrimitiveTypes.map(t => t.typeCode -> t).toMap
}

/** Primitive type recognizer to pattern match on TypeCode */
object PrimType {
  def unapply(tc: TypeCode): Option[SType] = SType.typeCodeToType.get(tc)
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
}
