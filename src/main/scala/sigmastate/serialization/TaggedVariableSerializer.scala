package sigmastate.serialization

import sigmastate._

object TaggedVariableSerializer extends SigmaSerializer[TaggedVariable[_ <: SType]] {

  override val opCode = SigmaSerializer.TaggedVariableCode

  override def parseBody = {case (bytes, pos) =>
    val tc = bytes(pos)
    val id = bytes(pos + 1)

    val consumed = 2

    (tc match {
      case b: Byte if b == SInt.typeCode => TaggedInt(id)
      case b: Byte if b == SBigInt.typeCode => TaggedBigInt(id)
      case b: Byte if b == SBoolean.typeCode => TaggedBoolean(id)
      case b: Byte if b == SByteArray.typeCode => TaggedByteArray(id)
      case b: Byte if b == SAvlTree.typeCode => TaggedAvlTree(id)
      case b: Byte if b == SGroupElement.typeCode => TaggedGroupElement(id)
      case b: Byte if b == SBox.typeCode => TaggedBox(id)
      case b: Byte if b == SBoxWithMetadata.typeCode => TaggedBoxWithMetadata(id)
    }, consumed, tc)
  }

  override def serializeBody = {v =>
    Array(v.typeCode, v.id)
  }
}
