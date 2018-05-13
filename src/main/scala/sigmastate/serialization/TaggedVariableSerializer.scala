package sigmastate.serialization

import sigmastate._
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer.Position
import OpCodes._

object TaggedVariableSerializer extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val tc = bytes(pos)
    val id = bytes(pos + 1)

    val consumed = 2

    (tc match {
      case b: Byte if b == SInt.typeCode => TaggedInt(id)
      case b: Byte if b == SBigInt.typeCode => TaggedBigInt(id)
      case b: Byte if b == SBoolean.typeCode => TaggedBoolean(id)
      case b: Byte if b == SCollection.SByteArrayTypeCode => TaggedByteArray(id)
      case b: Byte if b == SAvlTree.typeCode => TaggedAvlTree(id)
      case b: Byte if b == SGroupElement.typeCode => TaggedGroupElement(id)
      case b: Byte if b == SBox.typeCode => TaggedBox(id)
    }, consumed)
  }

  override def serializeBody(v: TaggedVariable[_ <: SType]) = {
    Array(v.typeCode, v.id)
  }
}
