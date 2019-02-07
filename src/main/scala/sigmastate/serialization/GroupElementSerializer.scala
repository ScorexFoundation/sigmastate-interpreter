package sigmastate.serialization

import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

/**
  * A serializer which encodes group elements, so elliptic curve points in our case, to bytes, and decodes points
  * from bytes.
  * Every point is encoded in compressed form (so only X coordinate and sign of Y are stored).
  * Thus for secp256k1 point, 33 bytes are needed. The first bytes is whether equals 2 or 3 depending on the sign of
  * Y coordinate(==2 is Y is positive, ==3, if Y is negative). Other 32 bytes are containing the X coordinate.
  * Special case is infinity point, which is encoded by 33 zeroes.
  * Thus elliptic curve point is always encoded with 33 bytes.
  */
object GroupElementSerializer extends Serializer[EcPointType, EcPointType] {

  private val curve = CryptoConstants.dlogGroup
  private val encodingSize = 1 + CryptoConstants.groupSize
  private lazy val identityPointEncoding = Array.fill(encodingSize)(0: Byte)

  override def serializeBody(point: EcPointType, w: SigmaByteWriter): Unit = {
    val bytes = if (point.isInfinity) {
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
    if (encoded.head != 0) {
      curve.curve.decodePoint(encoded).asInstanceOf[EcPointType]
    } else {
      curve.identity
    }
  }

}
