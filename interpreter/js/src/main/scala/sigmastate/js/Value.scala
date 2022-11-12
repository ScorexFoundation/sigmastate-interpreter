package sigmastate.js

import scalan.RType
import scorex.util.Extensions.{IntOps, LongOps}
import scorex.util.encode.Base16
import sigmastate.eval.{Colls, Evaluation}
import sigmastate.serialization.{DataSerializer, SigmaSerializer}
import sigmastate.SType
import sigmastate.js.Value.toRuntimeValue
import special.collection.{Coll, CollType}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Value", moduleID = "core")
class Value(val data: Any, val tpe: Type) extends js.Object {

  final private[js] def runtimeValue: Any = toRuntimeValue(data, tpe.rtype)

  /**
    * Encode this value as Base16 hex string.
    * 1) it transforms this value into {@link sigmastate.Values.ConstantNode} of sigma.
    * 2) it serializes the constant into byte array using {@link sigmastate.serialization.ConstantSerializer}
    * 3) the bytes are encoded using Base16 encoder into string
    *
    * @return hex string of serialized bytes
    */
  def toHex(): String = {
    // this can be implemented using ConstantSerializer and isoValueToConstant, but this
    // will add dependence on Constant and Values, which we want to avoid facilitate
    // module splitting
    // TODO simplify if module splitting fails
    val stype = Evaluation.rtypeToSType(tpe.rtype)
    val value = runtimeValue.asInstanceOf[SType#WrappedType]
    val w = SigmaSerializer.startWriter()
    w.putType(stype)
    DataSerializer.serialize(value, stype, w)
    Base16.encode(w.toBytes)
  }
}

@JSExportTopLevel("Values", moduleID = "core")
object Value extends js.Object {
  val MaxLong = js.BigInt("0x7fffffffffffffff")
  val MinLong = -js.BigInt("0x8000000000000000")

  final private[js] def toRuntimeValue(data: Any, rtype: RType[_]): Any = rtype match {
    case RType.ByteType | RType.ShortType | RType.IntType => data
    case RType.LongType => java.lang.Long.parseLong(data.asInstanceOf[js.BigInt].toString(10))
    case ct: CollType[a] =>
      val xs = data.asInstanceOf[js.Array[Any]]
      implicit val cT = ct.tItem.classTag
      val items = xs.map(x => toRuntimeValue(x, ct.tItem).asInstanceOf[a]).toArray[a]
      Colls.fromItems(items:_*)(ct.tItem)
    case _ =>
      throw new IllegalArgumentException(s"Unsupported type $rtype")
  }

  final private[js] def fromRuntimeValue(value: Any, rtype: RType[_]): Any = rtype match {
    case RType.ByteType | RType.ShortType | RType.IntType => value
    case RType.LongType => js.BigInt(value.asInstanceOf[Long].toString)
    case ct: CollType[a] =>
      val arr = value.asInstanceOf[Coll[a]].toArray
      js.Array(arr.map(x => fromRuntimeValue(x, ct.tItem)):_*)
    case _ =>
      throw new IllegalArgumentException(s"Unsupported type $rtype")
  }

  def ofByte(n: Int): Value = {
    new Value(n.toByteExact, Type.Byte)
  }
  def ofShort(n: Int): Value = {
    new Value(n.toShortExact, Type.Short)
  }
  def ofInt(n: Int): Value = {
    new Value(n.toLong.toIntExact, Type.Int)
  }
  def ofLong(n: js.BigInt): Value = {
    if (n < MinLong || n > MaxLong)
      throw new ArithmeticException(s"value $n is out of long range")
    new Value(n, Type.Long)
  }
  def pairOf(l: Value, r: Value): Value = {
    new Value(js.Array(l.data, r.data), Type.pairType(l.tpe, r.tpe))
  }
  def collOf(items: js.Array[Any], elemType: Type): Value = {
    new Value(items, Type.collType(elemType))
  }

  /**
    * Creates Value from hex encoded serialized bytes of Constant values.
    * <p>
    * In order to create Value you need to provide both value instance and
    * Type descriptor. This is similar to how values are represented in sigma
    * ConstantNode. Each ConstantNode also have value instance and `tpe: SType`
    * descriptor.
    * @param hex the string is obtained as hex encoding of serialized ConstantNode.
    *            (The bytes obtained by ConstantSerializer in sigma)
    * @return new deserialized ErgoValue instance
    */
  def fromHex(hex: String): Value = {
    // this can be implemented using ConstantSerializer and isoValueToConstant, but this
    // will add dependence on Constant and Values, which we want to avoid facilitate
    // module splitting
    // TODO simplify if module splitting fails
    val bytes = Base16.decode(hex).fold(t => throw t, identity)
    val r = SigmaSerializer.startReader(bytes)
    val stype = r.getType()
    val value = DataSerializer.deserialize(stype, r)
    val rtype = Evaluation.stypeToRType(stype)
    val jsvalue = fromRuntimeValue(value, rtype)
    new Value(jsvalue, new Type(rtype))
  }
}
