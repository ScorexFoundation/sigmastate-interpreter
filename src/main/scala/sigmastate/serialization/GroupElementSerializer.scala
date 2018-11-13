package sigmastate.serialization

import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object GroupElementSerializer extends Serializer[EcPointType, EcPointType] {

  private val curve = CryptoConstants.dlogGroup

  override def serializeBody(obj: EcPointType, w: SigmaByteWriter): Unit = {
    val bytes = obj.getEncoded(true)
    w.putBytes(bytes)
  }

  override def parseBody(r: SigmaByteReader): EcPointType = r.getByte() match {
    case 0 =>
      // infinity point is always compressed as 1 byte (X9.62 s 4.3.6)
      val point = curve.curve.decodePoint(Array(0)).asInstanceOf[EcPointType]
      point
    case m if m == 2 || m == 3 =>
      val consumed = 1 + (curve.curve.getFieldSize + 7) / 8
      r.position = r.position - 1
      val encoded = r.getBytes(consumed)
      val point = curve.curve.decodePoint(encoded).asInstanceOf[EcPointType]
      point
    case m =>
      throw new Error(s"Only compressed encoding is supported, $m given")
  }

}
