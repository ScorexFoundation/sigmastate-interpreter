package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.utxo.{BoxWithMetadata, SigmaStateBox}




sealed trait SType {
  type WrappedType
}

case object SInt extends SType {override type WrappedType = Long}
case object SBigInt extends SType {override type WrappedType = BigInteger}
case object SBoolean extends SType {override type WrappedType = Boolean}
case object SByteArray extends SType {override type WrappedType = Array[Byte]}
case class  SCollection[ElemType <: SType]() extends SType {override type WrappedType = IndexedSeq[Value[ElemType]]}
case object SAvlTree extends SType {override type WrappedType = AvlTreeData}
case object SGroupElement extends SType {override type WrappedType = GroupElement}
case object SBox extends SType {override type WrappedType = SigmaStateBox}
case object SBoxWithMetadata extends SType {override type WrappedType = BoxWithMetadata}

