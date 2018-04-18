package sigmastate.serialization

import sigmastate.Values.GroupElementConstant
import ValueSerializer._
import sigmastate.interpreter.GroupSettings
import sigmastate.interpreter.GroupSettings.EcPointType

object GroupElementSerializer extends ValueSerializer[GroupElementConstant] {

  override val opCode: OpCode = ValueSerializer.GroupElementConstantCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {

    // TODO: Yet remains to be done

    GroupElementConstant(null) -> 0
  }

  override def serializeBody(obj: GroupElementConstant): Array[Byte] = {

    // TODO: Yet remains to be done

    val value: EcPointType = obj.value

    Array[Byte]()
  }
}
