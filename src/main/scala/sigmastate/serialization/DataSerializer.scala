package sigmastate.serialization

import java.math.BigInteger
import java.nio.ByteBuffer

import org.bouncycastle.math.ec.custom.sec.SecP384R1Point
import scorex.crypto.authds.ADDigest
import sigmastate.utils.ByteArrayBuilder
import sigmastate._
import sigmastate.utils.Extensions._
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.utxo.ErgoBox

object DataSerializer {
  private val curve = CryptoConstants.dlogGroup
  private val LengthSize: Int = 2

  def serialize[T <: SType](v: T#WrappedType, tpe: T, buf: ByteArrayBuilder): Unit = tpe match {
    case SByte => buf.append(v.asInstanceOf[Byte])
    case SBoolean => buf.append(v.asInstanceOf[Boolean])
    case SInt => buf.append(v.asInstanceOf[Long])
    case SBigInt =>
      val data = v.asInstanceOf[BigInteger].toByteArray
      val length = data.length
      require(length <= Short.MaxValue, "max collection size is Short.MaxValue")
      buf.append(length.toShort)
      buf.append(data)
    case SGroupElement =>
      val bytes = v.asInstanceOf[EcPointType].getEncoded(true)
      buf.append(bytes)
    case SBox =>
      val bytes = ErgoBox.serializer.toBytes(v.asInstanceOf[ErgoBox])
      buf.append(bytes.length).append(bytes)
    case SAvlTree =>
      val data = v.asInstanceOf[AvlTreeData]
      buf.append(data.startingDigest.length)
      buf.append(data.startingDigest)
      buf.append(data.keyLength)
      buf.appendOption(data.valueLengthOpt)(buf.append(_))
      buf.appendOption(data.maxNumOperations)(buf.append(_))
      buf.appendOption(data.maxDeletes)(buf.append(_))

    case tCol: SCollection[a] =>
      val arr = v.asInstanceOf[tCol.WrappedType]
      buf.append(arr.length)
      for (x <- arr)
        DataSerializer.serialize(x, tCol.elemType, buf)
    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, buf: ByteBuffer): (T#WrappedType) = (tpe match {
    case SByte => buf.get()
    case SBoolean => (buf.get() != 0.toByte)
    case SInt => buf.getLong()
    case SBigInt =>
      val size: Short = buf.getShort()
      val valueBytes = buf.getBytes(size)
      new BigInteger(valueBytes)
    case SGroupElement =>
      buf.get() match {
        case 0 =>
          // infinity point is always compressed as 1 byte (X9.62 s 4.3.6)
          val point = curve.curve.decodePoint(Array(0)).asInstanceOf[SecP384R1Point]
          point
        case m if m == 2 || m == 3 =>
          val consumed = 1 + (curve.curve.getFieldSize + 7) / 8
          val encoded = new Array[Byte](consumed)
          buf.position(buf.position() - 1)
          buf.get(encoded)
          val point = curve.curve.decodePoint(encoded).asInstanceOf[SecP384R1Point]
          point
        case m =>
          throw new Error(s"Only compressed encoding is supported, $m given")
      }
    case SBox =>
      val len = buf.getInt()
      val bytes = buf.getBytes(len)
      val box = ErgoBox.serializer.parseBytes(bytes).get
      box
    case SAvlTree =>
      val digestLength = buf.getInt()
      val digestBytes = buf.getBytes(digestLength)
      val keyLength = buf.getInt()
      val vlOpt = buf.getOption(buf.getInt())
      val mnoOpt = buf.getOption(buf.getInt())
      val mdOpt = buf.getOption(buf.getInt())
      val data = AvlTreeData(ADDigest @@ digestBytes, keyLength, vlOpt, mnoOpt, mdOpt)
      data
    case tCol: SCollection[a] =>
      val len = buf.getInt()
      val arr = new Array(len).asInstanceOf[SCollection[a]#WrappedType]
      for (i <- 0 until len) {
        arr(i) = deserialize(tCol.elemType, buf)
      }
      arr
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }).asInstanceOf[T#WrappedType]
}