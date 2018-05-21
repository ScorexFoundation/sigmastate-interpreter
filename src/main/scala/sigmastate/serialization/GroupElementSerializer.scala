package sigmastate.serialization

import org.bouncycastle.math.ec.custom.sec.SecP384R1Point
import sigmastate.Values.GroupElementConstant
import sigmastate.interpreter.CryptoConstants
import sigmastate.serialization.OpCodes._


object GroupElementSerializer extends ValueSerializer[GroupElementConstant] {

  type ElemType = CryptoConstants.EcPointType

  private val curve = CryptoConstants.dlogGroup

  override val opCode: OpCode = GroupElementConstantCode

  /**
    * Encode a point on this curve to a compressed ASN.1 encoding (X9.62 s 4.2.1 - 4.2.2).
    *
    * @return the point encoding
    */
  override def serializeBody(gec: GroupElementConstant): Array[Byte] = gec.value.getEncoded(true)

  /**
    * Decode a point on this curve from its ASN.1 encoding.
    * Only compressed points are supported (X9.62 s 4.2.1 pg 17).
    *
    * @return The decoded point.
    */
  override def parseBody(bytesIn: Array[Byte], pos: Int): (GroupElementConstant, Int) = bytesIn(pos) match {
    case 0 =>
      // infinity point is always compressed as 1 byte (X9.62 s 4.3.6)
      val point = curve.curve.decodePoint(Array(bytesIn(pos))).asInstanceOf[SecP384R1Point]
      (GroupElementConstant(point), 1)
    case m if m == 2 || m == 3 =>
      val consumed = 1 + (curve.curve.getFieldSize + 7) / 8
      val encoded = bytesIn.slice(pos, pos + consumed)
      val point = curve.curve.decodePoint(encoded).asInstanceOf[SecP384R1Point]
      (GroupElementConstant(point), consumed)
    case m =>
      throw new Error(s"Only compressed encoding is supported, $m given")
  }
}
