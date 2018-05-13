package sigmastate.serialization

import sigmastate._
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer.Position
import OpCodes._

object ContextVariableSerializer extends ValueSerializer[ContextVariable[_ <: SType]] {

  override val opCode: OpCode = ContextVariableCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val varId = bytes(pos)
    val (tpe, consumed) = STypeSerializer.deserialize(bytes, pos + 1)

    val node = tpe match {
      case SInt => TaggedInt(varId)
      case SBigInt => TaggedBigInt(varId)
      case SBoolean => TaggedBoolean(varId)
      case SCollection(_) => TaggedVariable(varId, tpe)
      case SAvlTree => TaggedAvlTree(varId)
      case SGroupElement => TaggedGroupElement(varId)
      case SBox => TaggedBox(varId)
    }
    (node, consumed + 1)
  }

  override def serializeBody(v: ContextVariable[_ <: SType]) = {
    val tpeBytes = STypeSerializer.serialize(v.tpe)
    Array(v.id, tpeBytes: _*)
  }
}
