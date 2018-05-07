package sigmastate.serialization

import org.bouncycastle.math.ec.custom.sec.SecP384R1Point
import sigmastate.Values.GroupElementConstant
import sigmastate.interpreter.GroupSettings
import sigmastate.serialization.OpCodes._


object GroupElementSerializer extends ValueSerializer[GroupElementConstant] {

  type ElemType = GroupSettings.EcPointType

  private val curve = GroupSettings.dlogGroup

  override val opCode: OpCode = GroupElementConstantCode

  override def serializeBody(gec: GroupElementConstant): Array[Byte] = gec.value.getEncoded(true)

  override def parseBody(bytesIn: Array[Byte], pos: Int): (GroupElementConstant, Int) = {
    val consumed = 1 + (curve.curve.getFieldSize + 7) / 8
    val encoded = bytesIn.slice(pos, pos + consumed)
    val point: SecP384R1Point = curve.curve.decodePoint(encoded).asInstanceOf[SecP384R1Point]
    (GroupElementConstant(point), consumed)
  }
}
