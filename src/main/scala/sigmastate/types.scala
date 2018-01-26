package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import sigmastate.utxo.BoxWithMetadata

sealed trait SType[WrappedType]

case object SInt extends SType[Long]
case object SBigInt extends SType[BigInteger]
case object SBoolean extends SType[Boolean]
case object SByteArray extends SType[Array[Byte]]
case object SProp extends SType[Array[Byte]]
case class  SCollection[T, +ElemType <: SType[T]]() extends SType[IndexedSeq[Value[T, ElemType]]]
case object SAvlTree extends SType[AvlTreeData]
case object SGroupElement extends SType[GroupElement]
case object SBox extends SType[BoxWithMetadata]