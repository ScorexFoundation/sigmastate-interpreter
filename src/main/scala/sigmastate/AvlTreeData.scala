package sigmastate

import java.util.{Arrays, Objects}

import scorex.crypto.authds.ADDigest
import sigmastate.serialization.Serializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class AvlTreeFlags(insertAllowed: Boolean, updateAllowed: Boolean, removeAllowed: Boolean)

object AvlTreeFlags {
  def apply(serializedFlags: Byte): AvlTreeFlags = {
    val insertAllowed = (serializedFlags & 0x01) != 0
    val updateAllowed = (serializedFlags & 0x02) != 0
    val removeAllowed = (serializedFlags & 0x04) != 0
    AvlTreeFlags(insertAllowed, updateAllowed, removeAllowed)
  }

  def serializeFlags(avlTreeFlags: AvlTreeFlags): Byte = {
    val readOnly = 0
    val i = if(avlTreeFlags.insertAllowed) readOnly | 0x01 else readOnly
    val u = if(avlTreeFlags.updateAllowed) i | 0x02 else i
    val r = if(avlTreeFlags.removeAllowed) u | 0x04 else u
    r.toByte
  }
}

/**
  * Type of data which efficiently authenticates
  *
  * @param startingDigest
  * @param treeFlags
  * @param keyLength
  * @param valueLengthOpt
  * @param maxNumOperations
  * @param maxDeletes
  */

case class AvlTreeData( startingDigest: ADDigest,
                        treeFlags: AvlTreeFlags,
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
  val dummy = new AvlTreeData(ADDigest @@ Array.fill(33)(0:Byte), keyLength = 32)

  object serializer extends Serializer[AvlTreeData, AvlTreeData] {

    override def serializeBody(data: AvlTreeData, w: SigmaByteWriter): Unit = {
      w.putUByte(data.startingDigest.length)
        .putBytes(data.startingDigest)
        .putUInt(data.keyLength)
        .putOption(data.valueLengthOpt)(_.putUInt(_))
        .putOption(data.maxNumOperations)(_.putUInt(_))
        .putOption(data.maxDeletes)(_.putUInt(_))
    }

    override def parseBody(r: SigmaByteReader): AvlTreeData = {
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
