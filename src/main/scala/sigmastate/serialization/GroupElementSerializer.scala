package sigmastate.serialization

import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object GroupElementSerializer extends Serializer[EcPointType, EcPointType] {

  private val curve = CryptoConstants.dlogGroup
  private val encodingSize = 1 + (curve.curve.getFieldSize + 7) / 8
  private lazy val identityPointEncoding = Array.fill(encodingSize)(0: Byte)

  override def serializeBody(point: EcPointType, w: SigmaByteWriter): Unit = {
    val bytes = if(point.isInfinity) {
      identityPointEncoding
    } else {
      val normed = point.normalize()
      val ySign = normed.getAffineYCoord.testBitZero()
      val X = normed.getXCoord.getEncoded
      val PO = new Array[Byte](X.length + 1)
      PO(0) = (if (ySign) 0x03 else 0x02).toByte
      System.arraycopy(X, 0, PO, 1, X.length)
      PO
    }
    w.putBytes(bytes)
  }

  override def parseBody(r: SigmaByteReader): EcPointType = {
    val encoded = r.getBytes(encodingSize)
    if(encoded.head != 0) {
      curve.curve.decodePoint(encoded).asInstanceOf[EcPointType]
    } else {
      curve.identity
    }
  }

}
