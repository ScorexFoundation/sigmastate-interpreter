package sigmastate.serialization

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import org.ergoplatform.ErgoBox
import sigmastate.Values.SigmaBoolean
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigma.util.Extensions._
import sigmastate._
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.collection.mutable

/** This works in tandem with ConstantSerializer, if you change one make sure to check the other.*/
object DataSerializer {

  def serialize[T <: SType](v: T#WrappedType, tpe: T, w: SigmaByteWriter): Unit = tpe match {
    case SUnit => // don't need to save anything
    case SBoolean => w.putBoolean(v.asInstanceOf[Boolean])
    case SByte => w.put(v.asInstanceOf[Byte])
    case SShort => w.putShort(v.asInstanceOf[Short])
    case SInt => w.putInt(v.asInstanceOf[Int])
    case SLong => w.putLong(v.asInstanceOf[Long])
    case SString =>
      SerializeLog.logPrintf(true, true, false, "SString");

      val bytes = v.asInstanceOf[String].getBytes(StandardCharsets.UTF_8)

      SerializeLog.logPrintf(true, true, false, "bytes.length");
      w.putUInt(bytes.length)
      SerializeLog.logPrintf(false, true, false, "bytes.length");

      SerializeLog.logPrintf(true, true, false, "bytes");
      w.putBytes(bytes)
      SerializeLog.logPrintf(false, true, false, "bytes");

      SerializeLog.logPrintf(false, true, false, "SString");
    case SBigInt =>
      SerializeLog.logPrintf(true, true, false, "SBigInt");

      val data = v.asInstanceOf[BigInteger].toByteArray

      SerializeLog.logPrintf(true, true, false, "data.length");
      w.putUShort(data.length)
      SerializeLog.logPrintf(false, true, false, "data.length");

      SerializeLog.logPrintf(true, true, false, "data");
      w.putBytes(data)
      SerializeLog.logPrintf(false, true, false, "data");

      SerializeLog.logPrintf(false, true, false, "SBigInt");
    case SGroupElement =>
      SerializeLog.logPrintf(true, true, false, "SGroupElement");

      val p = v.asInstanceOf[EcPointType]
      SerializeLog.logPrintf(true, true, false, "toEcPointType");
      GroupElementSerializer.serializeBody(p, w)
      SerializeLog.logPrintf(false, true, false, "toEcPointType");

      SerializeLog.logPrintf(false, true, false, "SGroupElement");
    case SSigmaProp =>
      SerializeLog.logPrintf(true, true, false, "SSigmaProp");

      val p = v.asInstanceOf[SigmaBoolean]
      SerializeLog.logPrintf(true, true, false, "toSigmaBoolean");
      w.putValue(p)
      SerializeLog.logPrintf(false, true, false, "toSigmaBoolean");

      SerializeLog.logPrintf(false, true, false, "SSigmaProp");
    case SBox =>
      SerializeLog.logPrintf(true, true, false, "SBox");

      val p = v.asInstanceOf[ErgoBox]
      SerializeLog.logPrintf(true, true, false, "toErgoBox");
      ErgoBox.serializer.serializeBody(p, w)
      SerializeLog.logPrintf(false, true, false, "toErgoBox");

      SerializeLog.logPrintf(false, true, false, "SBox");
    case SAvlTree =>
      SerializeLog.logPrintf(true, true, false, "SAvlTree");

      val p = v.asInstanceOf[AvlTreeData]
      SerializeLog.logPrintf(true, true, false, "toAvlTreeData");
      AvlTreeData.serializer.serializeBody(p, w)
      SerializeLog.logPrintf(false, true, false, "toAvlTreeData");

      SerializeLog.logPrintf(false, true, false, "SAvlTree");
    //andruiman: what is the difference between SCollection and SCollectionType?
    case tColl: SCollectionType[a] =>
      SerializeLog.logPrintf(true, true, false, "SCollectionType");

      val arr = v.asInstanceOf[tColl.WrappedType]
      SerializeLog.logPrintf(true, true, false, "arr.length");
      w.putUShort(arr.length)
      SerializeLog.logPrintf(false, true, false, "arr.length");

      tColl.elemType match {
        case SBoolean =>
          SerializeLog.logPrintf(true, true, false, "[SBoolean]");
          w.putBits(arr.asInstanceOf[Array[Boolean]])
          SerializeLog.logPrintf(false, true, false, "[SBoolean]");
        case SByte =>
          SerializeLog.logPrintf(true, true, false, "[SByte]");
          w.putBytes(arr.asInstanceOf[Array[Byte]])
          SerializeLog.logPrintf(false, true, false, "[SByte]");
        case _ =>
          SerializeLog.logPrintf(true, true, false, "[_]");
          arr.foreach(serialize(_, tColl.elemType, w))
          SerializeLog.logPrintf(false, true, false, "[_]");
      }

      SerializeLog.logPrintf(false, true, false, "SCollectionType");
    //andruiman: why not serialize array length?
    //maybe because the tuple type is stored before?
    case t: STuple =>
      SerializeLog.logPrintf(true, true, false, "STuple");

      val arr = v.asInstanceOf[t.WrappedType]
      val len = arr.length
      assert(arr.length == t.items.length, s"Type $t doesn't correspond to value $arr")
      if (len > 0xFFFF)
        sys.error(s"Length of tuple $arr exceeds ${0xFFFF} limit.")
      var i = 0

      SerializeLog.logPrintf(true, true, false, "arr");
      while (i < arr.length) {
        serialize[SType](arr(i), t.items(i), w)
        i += 1
      }
      SerializeLog.logPrintf(false, true, false, "arr");

      SerializeLog.logPrintf(false, true, false, "STuple");

    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, r: SigmaByteReader): (T#WrappedType) = (tpe match {
    case SUnit => ()
    case SBoolean => r.getUByte() != 0
    case SByte => r.getByte()
    case SShort => r.getShort()
    case SInt => r.getInt()
    case SLong => r.getLong()
    case SString =>
      val size = r.getUInt().toInt
      val bytes = r.getBytes(size)
      new String(bytes, StandardCharsets.UTF_8)
    case SBigInt =>
      val size: Short = r.getUShort().toShort
      val valueBytes = r.getBytes(size)
      new BigInteger(valueBytes)
    case SGroupElement =>
      GroupElementSerializer.parseBody(r)
    case SSigmaProp =>
      val p = r.getValue().asInstanceOf[SigmaBoolean]
      p
    case SBox =>
      ErgoBox.serializer.parseBody(r)
    case SAvlTree =>
      AvlTreeData.serializer.parseBody(r)
    case tColl: SCollectionType[a] =>
      val len = r.getUShort()
      if (tColl.elemType == SByte)
        r.getBytes(len)
      else
        deserializeArray(len, tColl.elemType, r)
    case tuple: STuple =>
      val arr =  tuple.items.map { t =>
        deserialize(t, r)
      }.toArray[Any]
      arr
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }).asInstanceOf[T#WrappedType]

  def deserializeArray[T <: SType](len: Int, tpe: T, r: SigmaByteReader): Array[T#WrappedType] =
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
