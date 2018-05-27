package sigmastate.serialization

import java.math.BigInteger

import org.bouncycastle.math.ec.custom.sec.SecP384R1Point
import org.ergoplatform.ErgoBox
import scorex.crypto.authds.ADDigest
import sigmastate.utils.{ByteWriter, ByteReader}
import sigmastate._
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.collection.mutable

object DataSerializer {
  private val curve = CryptoConstants.dlogGroup
  private val LengthSize: Int = 2

  def serialize[T <: SType](v: T#WrappedType, tpe: T, w: ByteWriter): Unit = tpe match {
    case SByte => w.put(v.asInstanceOf[Byte])
    case SBoolean => w.putBoolean(v.asInstanceOf[Boolean])
    case SInt => w.putLong(v.asInstanceOf[Long])
    case SBigInt =>
      val data = v.asInstanceOf[BigInteger].toByteArray
      val length = data.length
      require(length <= Short.MaxValue, "max collection size is Short.MaxValue")
      w.putShort(length.toShort)
      w.putBytes(data)
    case SGroupElement =>
      val bytes = v.asInstanceOf[EcPointType].getEncoded(true)
      w.putBytes(bytes)
    case SBox =>
      val bytes = ErgoBox.serializer.toBytes(v.asInstanceOf[ErgoBox])
      w.putInt(bytes.length).putBytes(bytes)
    case SAvlTree =>
      val data = v.asInstanceOf[AvlTreeData]
      w.putInt(data.startingDigest.length)
      w.putBytes(data.startingDigest)
      w.putInt(data.keyLength)
      w.putOption(data.valueLengthOpt)(_.putInt(_))
      w.putOption(data.maxNumOperations)(_.putInt(_))
      w.putOption(data.maxDeletes)(_.putInt(_))

    case tCol: SCollection[a] =>
      val arr = v.asInstanceOf[tCol.WrappedType]
      w.putInt(arr.length)
      for (x <- arr)
        DataSerializer.serialize(x, tCol.elemType, w)
    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, r: ByteReader): (T#WrappedType) = (tpe match {
    case SByte => r.get()
    case SBoolean => (r.get() != 0.toByte)
    case SInt => r.getLong()
    case SBigInt =>
      val size: Short = r.getShort()
      val valueBytes = r.getBytes(size)
      new BigInteger(valueBytes)
    case SGroupElement =>
      r.get() match {
        case 0 =>
          // infinity point is always compressed as 1 byte (X9.62 s 4.3.6)
          val point = curve.curve.decodePoint(Array(0)).asInstanceOf[SecP384R1Point]
          point
        case m if m == 2 || m == 3 =>
          val consumed = 1 + (curve.curve.getFieldSize + 7) / 8
          r.position = r.position - 1
          val encoded = r.getBytes(consumed)
          val point = curve.curve.decodePoint(encoded).asInstanceOf[SecP384R1Point]
          point
        case m =>
          throw new Error(s"Only compressed encoding is supported, $m given")
      }
    case SBox =>
      val len = r.getInt()
      val bytes = r.getBytes(len)
      val box = ErgoBox.serializer.parseBytes(bytes).get
      box
    case SAvlTree =>
      val digestLength = r.getInt()
      val digestBytes = r.getBytes(digestLength)
      val keyLength = r.getInt()
      val vlOpt = r.getOption(r.getInt())
      val mnoOpt = r.getOption(r.getInt())
      val mdOpt = r.getOption(r.getInt())
      val data = AvlTreeData(ADDigest @@ digestBytes, keyLength, vlOpt, mnoOpt, mdOpt)
      data
    case tCol: SCollection[a] =>
      val len = r.getInt()
      val arr = deserializeArray(len, tCol.elemType, r)
      arr
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }).asInstanceOf[T#WrappedType]

  def deserializeArray[T <: SType](len: Int, tpe: T, r: ByteReader): Array[T#WrappedType] = {
    val b = mutable.ArrayBuilder.make[T#WrappedType]()(tpe.classTag)
    for (i <- 0 until len) {
      b += deserialize(tpe, r)
    }
    b.result()
  }
}