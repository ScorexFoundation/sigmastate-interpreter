package sigmastate

import java.util.{Arrays, Objects}

import scorex.crypto.authds.ADDigest
import sigmastate.serialization.Serializer
import sigmastate.utils.{ByteReader, ByteWriter}

case class AvlTreeData( startingDigest: ADDigest,
                        keyLength: Int,
                        valueLengthOpt: Option[Int] = None,
                        maxNumOperations: Option[Int] = None,
                        maxDeletes: Option[Int] = None ) {
  override def equals(arg: Any) = arg match {
    case x: AvlTreeData =>
      Arrays.equals(startingDigest, x.startingDigest) &&
      keyLength == x.keyLength &&
      valueLengthOpt == x.valueLengthOpt &&
      maxNumOperations == x.maxNumOperations &&
      maxDeletes == x.maxDeletes
    case _ => false
  }

  override def hashCode() =
    (Arrays.hashCode(startingDigest) * 31 +
        keyLength.hashCode()) * 31 + Objects.hash(valueLengthOpt, maxNumOperations, maxDeletes)
}

object AvlTreeData {
  val dummy = new AvlTreeData(ADDigest @@ Array.fill(32)(0:Byte), keyLength = 32)

  object serializer extends Serializer[AvlTreeData, AvlTreeData] {

    override def serializeBody(data: AvlTreeData, w: ByteWriter): Unit = {
      w.putUByte(data.startingDigest.length)
        .putBytes(data.startingDigest)
        .putUInt(data.keyLength)
        .putOption(data.valueLengthOpt)(_.putUInt(_))
        .putOption(data.maxNumOperations)(_.putUInt(_))
        .putOption(data.maxDeletes)(_.putUInt(_))
    }

    override def parseBody(r: ByteReader): AvlTreeData = {
      val startingDigestLen = r.getUByte()
      val startingDigest = r.getBytes(startingDigestLen)
      val keyLength = r.getUInt().toInt
      val valueLengthOpt = r.getOption(r.getUInt().toInt)
      val maxNumOperations = r.getOption(r.getUInt().toInt)
      val maxDeletes = r.getOption(r.getUInt().toInt)
      AvlTreeData(ADDigest @@ startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)
    }
  }
}
