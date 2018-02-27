package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.SType.TypeCode
import sigmastate.utxo.SigmaStateBox



sealed trait SType {
  type WrappedType

  val typeCode: SType.TypeCode
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

case class  SCollection[ElemType <: SType]()(implicit val elemType: ElemType) extends SType {
  override type WrappedType = IndexedSeq[Value[ElemType]]

  override val typeCode = SCollection.collectionOf(elemType.typeCode)
}

object SCollection {
  val Base = 80: Byte

  //todo: SCollection[SCollection[]] is not possible!
  def collectionOf(typeCode: TypeCode) = (Base + typeCode).toByte
}
