package sigmastate.serialization

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import org.ergoplatform.ErgoBox
import sigmastate.Values.SigmaBoolean
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate._
import sigmastate.eval.Evaluation
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
      val bytes = v.asInstanceOf[String].getBytes(StandardCharsets.UTF_8)
      w.putUInt(bytes.length)
      w.putBytes(bytes)
    case SBigInt =>
      val data = v.asInstanceOf[BigInteger].toByteArray
      w.putUShort(data.length)
      w.putBytes(data)
    case SGroupElement =>
      GroupElementSerializer.serialize(v.asInstanceOf[EcPointType], w)
    case SSigmaProp =>
      val p = v.asInstanceOf[SigmaBoolean]
      SigmaBoolean.serializer.serialize(p, w)
    case SBox =>
      ErgoBox.sigmaSerializer.serialize(v.asInstanceOf[ErgoBox], w)
    case SAvlTree =>
      AvlTreeData.serializer.serialize(v.asInstanceOf[AvlTreeData], w)
    case tColl: SCollectionType[a] =>
      val arr = v.asInstanceOf[tColl.WrappedType]
      w.putUShort(arr.length)
      tColl.elemType match {
        case SBoolean =>
          w.putBits(arr.asInstanceOf[Array[Boolean]])
        case SByte =>
          w.putBytes(arr.asInstanceOf[Array[Byte]])
        case _ =>
          arr.foreach(x => serialize(x, tColl.elemType, w))
      }

    case t: STuple =>
      val arr = Evaluation.fromDslTuple(v, t).asInstanceOf[t.WrappedType]
      val len = arr.length
      assert(arr.length == t.items.length, s"Type $t doesn't correspond to value $arr")
      if (len > 0xFFFF)
        sys.error(s"Length of tuple $arr exceeds ${0xFFFF} limit.")
      var i = 0
      while (i < arr.length) {
        serialize[SType](arr(i), t.items(i), w)
        i += 1
      }

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
      GroupElementSerializer.parse(r)
    case SSigmaProp =>
      SigmaBoolean.serializer.parse(r)
    case SBox =>
      ErgoBox.sigmaSerializer.parse(r)
    case SAvlTree =>
      AvlTreeData.serializer.parse(r)
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
        for (_ <- 0 until len) {
          b += deserialize(tpe, r)
        }
        b.result()
    }
}
