package sigmastate.serialization

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import scorex.crypto.authds.ADDigest
import sigmastate.SCollection.SByteArray
import sigmastate.Values.EvaluatedValue
import sigmastate.utils.{ByteWriter, ByteReader}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import scala.collection.mutable

/** This works in tandem with ConstantSerializer, if you change one make sure to check the other.*/
object DataSerializer {
  type ElemType = CryptoConstants.EcPointType

  private val curve = CryptoConstants.dlogGroup
  private val LengthSize: Int = 2

  def serialize[T <: SType](v: T#WrappedType, tpe: T, w: ByteWriter): Unit = tpe match {
    case SUnit => // don't need to save anything
    case SBoolean => w.putBoolean(v.asInstanceOf[Boolean])
    case SByte => w.put(v.asInstanceOf[Byte])
    case SShort => w.putShort(v.asInstanceOf[Short])
    case SInt => w.putInt(v.asInstanceOf[Int])
    case SLong => w.putLong(v.asInstanceOf[Long])
    case SBigInt =>
      val data = v.asInstanceOf[BigInteger].toByteArray
      val length = data.length
      require(length <= Short.MaxValue, "max collection size is Short.MaxValue")
      w.putUShort(length.toShort)
      w.putBytes(data)
    case SGroupElement =>
      val bytes = v.asInstanceOf[EcPointType].getEncoded(true)
      w.putBytes(bytes)
    case SBox =>
      ErgoBox.serializer.serializeBody(v.asInstanceOf[ErgoBox], w)

    case SAvlTree =>
      val data = v.asInstanceOf[AvlTreeData]
      serialize[SByteArray](data.startingDigest, SByteArray, w)
      w.putUInt(data.keyLength)
      w.putOption(data.valueLengthOpt)(_.putUInt(_))
      w.putOption(data.maxNumOperations)(_.putUInt(_))
      w.putOption(data.maxDeletes)(_.putUInt(_))

    case tCol: SCollectionType[a] =>
      val arr = v.asInstanceOf[tCol.WrappedType]
      val len = arr.length
      if (len > 0xFFFF)
        sys.error(s"Length of array $arr exceeds ${0xFFFF} limit.")
      w.putUShort(len.toShort)
      if (tCol.elemType == SBoolean)
        w.putBits(arr.asInstanceOf[Array[Boolean]])
      else
        arr.foreach(x => serialize(x, tCol.elemType, w))

    case t: STuple =>
      val arr = v.asInstanceOf[t.WrappedType]
      val len = arr.length
      assert(arr.length == t.items.length, s"Type $t doesn't correspond to value $arr")
      if (len > 0xFFFF)
        sys.error(s"Length of tuple $arr exceeds ${0xFF} limit.")
      var i = 0
      while (i < arr.length) {
        serialize[SType](arr(i), t.items(i), w)
        i += 1
      }

    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, r: ByteReader): (T#WrappedType) = (tpe match {
    case SUnit => ()
    case SBoolean => r.getUByte() != 0
    case SByte => r.getByte()
    case SShort => r.getShort()
    case SInt => r.getInt()
    case SLong => r.getLong()
    case SBigInt =>
      val size: Short = r.getUShort().toShort
      val valueBytes = r.getBytes(size)
      new BigInteger(valueBytes)
    case SGroupElement =>
      r.getByte() match {
        case 0 =>
          // infinity point is always compressed as 1 byte (X9.62 s 4.3.6)
          val point = curve.curve.decodePoint(Array(0)).asInstanceOf[ElemType]
          point
        case m if m == 2 || m == 3 =>
          val consumed = 1 + (curve.curve.getFieldSize + 7) / 8
          r.position = r.position - 1
          val encoded = r.getBytes(consumed)
          val point = curve.curve.decodePoint(encoded).asInstanceOf[ElemType]
          point
        case m =>
          throw new Error(s"Only compressed encoding is supported, $m given")
      }
    case SBox =>
      ErgoBox.serializer.parseBody(r)
    case SAvlTree =>
      val startingDigest = deserialize[SByteArray](SByteArray, r)
      val keyLength = r.getUInt().toInt
      val valueLengthOpt = r.getOption(r.getUInt().toInt)
      val maxNumOperations = r.getOption(r.getUInt().toInt)
      val maxDeletes = r.getOption(r.getUInt().toInt)
      val data = AvlTreeData(ADDigest @@ startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)
      data
    case tCol: SCollectionType[a] =>
      val len = r.getUShort()
      val arr = deserializeArray(len, tCol.elemType, r)
      arr
    case tuple: STuple =>
      val arr =  tuple.items.map { t =>
        deserialize(t, r)
      }.toArray[Any]
      arr
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }).asInstanceOf[T#WrappedType]

  def deserializeArray[T <: SType](len: Int, tpe: T, r: ByteReader): Array[T#WrappedType] =
    tpe match {
      case SBoolean =>
        r.getBits(len).asInstanceOf[Array[T#WrappedType]]
      case _ =>
        val b = mutable.ArrayBuilder.make[T#WrappedType]()(tpe.classTag)
        for (i <- 0 until len) {
          b += deserialize(tpe, r)
        }
        b.result()
    }
}
