package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.utxo.{BoxWithMetadata, SigmaStateBox}




sealed trait SType {
  type WrappedType

  val typeCode: SType.TypeCode
}

object SType {
  type TypeCode = Byte
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

case object SBoxWithMetadata extends SType {
  override type WrappedType = BoxWithMetadata

  override val typeCode: Byte = 8: Byte
}

case class  SCollection[ElemType <: SType]()(implicit val w: ElemType) extends SType {
  override type WrappedType = IndexedSeq[Value[ElemType]]

  override val typeCode = (80 + w.typeCode).toByte
}
