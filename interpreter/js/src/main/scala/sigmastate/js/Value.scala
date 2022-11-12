package sigmastate.js

import scalan.RType
import scorex.util.Extensions.{IntOps, LongOps}
import scorex.util.encode.Base16
import sigmastate.eval.Evaluation
import sigmastate.serialization.{DataSerializer, SigmaSerializer}
import sigmastate.SType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Value", moduleID = "core")
class Value(val data: Any, val tpe: Type) extends js.Object {

  final private[js] def runtimeValue: Any = tpe.rtype match {
    case RType.ByteType | RType.ShortType | RType.IntType => data
    case RType.LongType => java.lang.Long.parseLong(data.asInstanceOf[js.BigInt].toString(10))
    case _ =>
      throw new IllegalArgumentException(s"Unsupported type $tpe")
  }

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

  final private[js] def fromRuntimeValue(value: Any, tpe: Type): Value = {
    val jsValue = tpe.rtype match {
      case RType.ByteType | RType.ShortType | RType.IntType => value
      case RType.LongType => js.BigInt(value.asInstanceOf[Long].toString)
      case _ =>
        throw new IllegalArgumentException(s"Unsupported type $tpe")
    }
    new Value(jsValue, tpe)
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
    fromRuntimeValue(value, new Type(Evaluation.stypeToRType(stype)))
  }
}
