package sigmastate.serialization

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import org.ergoplatform.ErgoBox
import scalan.RType
import sigmastate.Values.SigmaBoolean
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate._
import sigmastate.eval.{Evaluation, _}
import special.collection._
import special.sigma._

import scala.collection.mutable

/** This works in tandem with ConstantSerializer, if you change one make sure to check the other.*/
object DataSerializer {

  /** Use type descriptor `tpe` to deconstruct type structure and recursively serialize subcomponents.
    * Primitive types are leaves of the type tree, and they are served as basis of recursion.
    * The data value `v` is expected to conform to the type described by `tpe`.
    */
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
      val data = SigmaDsl.toBigInteger(v.asInstanceOf[BigInt]).toByteArray
      w.putUShort(data.length)
      w.putBytes(data)
    case SGroupElement =>
      GroupElementSerializer.serialize(groupElementToECPoint(v.asInstanceOf[GroupElement]), w)
    case SSigmaProp =>
      val p = v.asInstanceOf[SigmaProp]
      SigmaBoolean.serializer.serialize(sigmaPropToSigmaBoolean(p), w)
    case SBox =>
      val b = v.asInstanceOf[Box]
      ErgoBox.sigmaSerializer.serialize(boxToErgoBox(b), w)
    case SAvlTree =>
      AvlTreeData.serializer.serialize(avlTreeToAvlTreeData(v.asInstanceOf[AvlTree]), w)
    case tColl: SCollectionType[a] =>
      val arr = v.asInstanceOf[tColl.WrappedType]
      w.putUShort(arr.length)
      tColl.elemType match {
        case SBoolean =>
          w.putBits(arr.asInstanceOf[Coll[Boolean]].toArray)
        case SByte =>
          w.putBytes(arr.asInstanceOf[Coll[Byte]].toArray)
        case _ =>
          arr.toArray.foreach(x => serialize(x, tColl.elemType, w))
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

  /** Reads a data value from Reader. The data value bytes is expected to confirm
    * to the type descriptor `tpe`. */
  def deserialize[T <: SType](tpe: T, r: SigmaByteReader): (T#WrappedType) = {
    val depth = r.level
    r.level = depth + 1
    val res = (tpe match {
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
        SigmaDsl.BigInt(new BigInteger(valueBytes))
      case SGroupElement =>
        SigmaDsl.GroupElement(GroupElementSerializer.parse(r))
      case SSigmaProp =>
        SigmaDsl.SigmaProp(SigmaBoolean.serializer.parse(r))
      case SBox =>
        SigmaDsl.Box(ErgoBox.sigmaSerializer.parse(r))
      case SAvlTree =>
        SigmaDsl.avlTree(AvlTreeData.serializer.parse(r))
      case tColl: SCollectionType[a] =>
        val len = r.getUShort()
        if (tColl.elemType == SByte)
          Colls.fromArray(r.getBytes(len))
        else
          deserializeColl(len, tColl.elemType, r)
      case tuple: STuple =>
        val arr = tuple.items.map { t =>
          deserialize(t, r)
        }.toArray[Any]
        val coll = Colls.fromArray(arr)(RType.AnyType)
        Evaluation.toDslTuple(coll, tuple)
      case _ => sys.error(s"Don't know how to deserialize $tpe")
    }).asInstanceOf[T#WrappedType]
    r.level = r.level - 1
    res
  }

  def deserializeColl[T <: SType](len: Int, tpeElem: T, r: SigmaByteReader): Coll[T#WrappedType] =
    tpeElem match {
      case SBoolean =>
        Colls.fromArray(r.getBits(len)).asInstanceOf[Coll[T#WrappedType]]
      case SByte =>
        Colls.fromArray(r.getBytes(len)).asInstanceOf[Coll[T#WrappedType]]
      case _ =>
        implicit val tItem = (tpeElem match {
          case tTup: STuple if tTup.items.length == 2 =>
            Evaluation.stypeToRType(tpeElem)
          case tTup: STuple =>
            collRType(RType.AnyType)
          case _ =>
            Evaluation.stypeToRType(tpeElem)
        }).asInstanceOf[RType[T#WrappedType]]
        val b = mutable.ArrayBuilder.make[T#WrappedType]()(tItem.classTag)
        for (_ <- 0 until len) {
          b += deserialize(tpeElem, r)
        }
        Colls.fromArray(b.result())
    }
}
