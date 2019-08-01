package sigmastate

import java.util
import java.util.{Arrays, Objects}

import io.circe._
import io.circe.syntax._
import org.ergoplatform.JsonCodecs
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.CryptoConstants
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}


case class AvlTreeFlags(insertAllowed: Boolean, updateAllowed: Boolean, removeAllowed: Boolean) {
  def serializeToByte: Byte = AvlTreeFlags.serializeFlags(this)
}

object AvlTreeFlags {

  lazy val ReadOnly = AvlTreeFlags(insertAllowed = false, updateAllowed = false, removeAllowed = false)

  lazy val AllOperationsAllowed = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)

  lazy val InsertOnly = AvlTreeFlags(insertAllowed = true, updateAllowed = false, removeAllowed = false)

  lazy val RemoveOnly = AvlTreeFlags(insertAllowed = false, updateAllowed = false, removeAllowed = true)

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
  * Type of data which efficiently authenticates potentially huge dataset having key-value dictionary interface.
  * Only root hash of dynamic AVL+ tree, tree height, key length, optional value length, and access flags are stored
  * in an instance of the datatype.
  *
  * Please note that standard hash function from CryptoConstants is used, and height is stored along with root hash of
  * the tree, thus `digest` size is always CryptoConstants.hashLength + 1 bytes.
  *
  * @param digest authenticated tree digest: root hash along with tree height
  * @param treeFlags - allowed modifications. See AvlTreeFlags description for details
  * @param keyLength  - all the elements under the tree have the same length
  * @param valueLengthOpt - if non-empty, all the values under the tree are of the same length
  */

case class AvlTreeData(digest: ADDigest,
                       treeFlags: AvlTreeFlags,
                       keyLength: Int,
                       valueLengthOpt: Option[Int] = None) {
  override def equals(arg: Any): Boolean = arg match {
    case x: AvlTreeData =>
      Arrays.equals(digest, x.digest) &&
      keyLength == x.keyLength &&
      valueLengthOpt == x.valueLengthOpt &&
      treeFlags == x.treeFlags
    case _ => false
  }

  override def hashCode(): Int =
    (util.Arrays.hashCode(digest) * 31 +
        keyLength.hashCode()) * 31 + Objects.hash(valueLengthOpt, treeFlags)
}

object AvlTreeData extends JsonCodecs {
  val DigestSize: Int = CryptoConstants.hashLength + 1 //please read class comments above for details
  val TreeDataSize = DigestSize + 3 + 4 + 4

  val dummy = new AvlTreeData(
    ADDigest @@ Array.fill(DigestSize)(0:Byte),
    AvlTreeFlags.AllOperationsAllowed,
    keyLength = 32)

  object serializer extends SigmaSerializer[AvlTreeData, AvlTreeData] {

    override def serialize(data: AvlTreeData, w: SigmaByteWriter): Unit = {
      val tf = AvlTreeFlags.serializeFlags(data.treeFlags)
      w.putBytes(data.digest)
        .putUByte(tf)
        .putUInt(data.keyLength)
        .putOption(data.valueLengthOpt)(_.putUInt(_))
    }

    override def parse(r: SigmaByteReader): AvlTreeData = {
      val digest = r.getBytes(DigestSize)
      val tf = AvlTreeFlags(r.getByte())
      val keyLength = r.getUInt().toInt
      val valueLengthOpt = r.getOption(r.getUInt().toInt)
      AvlTreeData(ADDigest @@ digest, tf, keyLength, valueLengthOpt)
    }
  }

  implicit val jsonEncoder: Encoder[AvlTreeData] = { v =>
    Json.obj(
      "digest" -> v.digest.asJson,
      "treeFlags" -> v.treeFlags.serializeToByte.asJson,
      "keyLength" -> v.keyLength.asJson,
      "valueLength" -> v.valueLengthOpt.asJson
    )
  }

  implicit val jsonDecoder: Decoder[AvlTreeData] = { cursor =>
    for {
      digest <- cursor.downField("digest").as[ADDigest]
      treeFlagsByte <- cursor.downField("treeFlags").as[Byte]
      keyLength <- cursor.downField("keyLength").as[Int]
      valueLength <- cursor.downField("valueLength").as[Option[Int]]
    } yield new AvlTreeData(digest, AvlTreeFlags(treeFlagsByte), keyLength, valueLength)
  }
}
