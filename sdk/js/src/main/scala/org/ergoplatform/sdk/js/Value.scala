package org.ergoplatform.sdk.js

import org.ergoplatform.sdk.js.Value.toRuntimeData
import scalan.RType
import scalan.RType.PairType
import scorex.util.Extensions.{IntOps, LongOps}
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.crypto.Platform
import sigmastate.eval.{CAvlTree, CGroupElement, CSigmaProp, Colls, CostingBox, Evaluation, SigmaDsl}
import sigmastate.fleetSdkCommon.distEsmTypesBoxesMod.Box
import sigmastate.fleetSdkCommon.distEsmTypesCommonMod
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.{ConstantSerializer, DataSerializer, SigmaSerializer}
import special.collection.{Coll, CollType}

import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * This class is used to represent any valid value of ErgoScript language.
  * Any such value comes equipped with {@link Type} descriptor.
  * Note, there is a distinction between JS types and ErgoScript types.
  * Each Value instance represents the concrete ErgoScript type given by `tpe`.
  * The implementation is based on the pre-defined mapping between JS and ES types.
  * This mapping is applied recursively and is given by the following:
  *
  * JS type            |  ErgoScript Type
  * --------------------------------------
  * Number             |  Byte
  * Number             |  Short
  * Number             |  Int
  * BigInt             |  Long
  * BigInt             |  BigInt
  * array [A, B]       |  (A, B) - pair
  * array [a1, a2 ..]  |  Coll[A] - collection
  *
  * @param data JS value wrapped in this value
  * @param tpe  type descriptor of the ErgoScript type
  */
@JSExportTopLevel("Value")
class Value(val data: Any, val tpe: Type) extends js.Object {

  /** Get Sigma runtime value which can be passed to interpreter, saved in register and
    * [[sigmastate.Values.Constant]] nodes.
    */
  final private[js] def runtimeData: Any = toRuntimeData(data, tpe.rtype)

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
    val value = runtimeData.asInstanceOf[SType#WrappedType]
    val w = SigmaSerializer.startWriter()
    w.putType(stype)
    DataSerializer.serialize(value, stype, w)
    Base16.encode(w.toBytes)
  }
}

@JSExportTopLevel("ValueObj")
object Value extends js.Object {
  /** Maximal positive value of ES type Long */
  val MaxLong = js.BigInt("0x7fffffffffffffff")

  /** Minimal negative value of ES type Long */
  val MinLong = -js.BigInt("0x8000000000000000")

  /** Helper method to get Sigma runtime value which can be passed to interpreter, saved
    * in register and [[sigmastate.Values.Constant]] nodes.
    */
  final private[js] def toRuntimeData(data: Any, rtype: RType[_]): Any = rtype match {
    case RType.BooleanType => data
    case RType.ByteType | RType.ShortType | RType.IntType => data
    case RType.LongType => java.lang.Long.parseLong(data.asInstanceOf[js.BigInt].toString(10))
    case special.sigma.BigIntRType =>
      val v = data.asInstanceOf[js.BigInt]
      SigmaDsl.BigInt(new BigInteger(v.toString(16), 16))
    case special.sigma.GroupElementRType =>
      val point = data.asInstanceOf[Platform.Point]
      SigmaDsl.GroupElement(new Platform.Ecp(point))
    case special.sigma.SigmaPropRType =>
      val p = data.asInstanceOf[SigmaProp]
      SigmaDsl.SigmaProp(p.sigmaBoolean)
    case special.sigma.AvlTreeRType =>
      val t = data.asInstanceOf[AvlTree]
      Isos.isoAvlTree.to(t)
    case special.sigma.BoxRType =>
      val t = data.asInstanceOf[Box[distEsmTypesCommonMod.Amount, NonMandatoryRegisters]]
      SigmaDsl.Box(Isos.isoBox.to(t))
    case ct: CollType[a] =>
      val xs = data.asInstanceOf[js.Array[Any]]
      implicit val cT = ct.tItem.classTag
      val items = xs.map(x => toRuntimeData(x, ct.tItem).asInstanceOf[a]).toArray[a]
      Colls.fromItems(items:_*)(ct.tItem)
    case pt: PairType[a, b] =>
      val p = data.asInstanceOf[js.Array[Any]]
      val x = toRuntimeData(p(0), pt.tFst).asInstanceOf[a]
      val y = toRuntimeData(p(1), pt.tSnd).asInstanceOf[b]
      (x, y)
    case RType.UnitType => data
    case _ =>
      throw new IllegalArgumentException(s"Unsupported type $rtype")
  }

  /** Helper method to extract JS data value from Sigma runtime value.
    * This should be inverse to `toRuntimeData`.
    *
    * @param value runtime value of type given by `rtype`
    * @param rtype type descriptor of Sigma runtime value
    */
  final private[js] def fromRuntimeData(value: Any, rtype: RType[_]): Any = rtype match {
    case RType.BooleanType => value
    case RType.ByteType | RType.ShortType | RType.IntType => value
    case RType.LongType => js.BigInt(value.asInstanceOf[Long].toString)
    case special.sigma.BigIntRType =>
      val hex = SigmaDsl.toBigInteger(value.asInstanceOf[special.sigma.BigInt]).toString(10)
      js.BigInt(hex)
    case special.sigma.GroupElementRType =>
      val point: Platform.Point = value.asInstanceOf[CGroupElement].wrappedValue.asInstanceOf[Platform.Ecp].point
      point
    case special.sigma.SigmaPropRType =>
      new SigmaProp(value.asInstanceOf[CSigmaProp].wrappedValue)
    case special.sigma.AvlTreeRType =>
      Isos.isoAvlTree.from(value.asInstanceOf[CAvlTree])
    case special.sigma.BoxRType =>
      Isos.isoBox.from(value.asInstanceOf[CostingBox].wrappedValue)
    case ct: CollType[a] =>
      val arr = value.asInstanceOf[Coll[a]].toArray
      js.Array(arr.map(x => fromRuntimeData(x, ct.tItem)):_*)
    case pt: PairType[a, b] =>
      val p = value.asInstanceOf[(a, b)]
      js.Array(fromRuntimeData(p._1, pt.tFst), fromRuntimeData(p._2, pt.tSnd))
    case RType.UnitType => value
    case _ =>
      throw new IllegalArgumentException(s"Unsupported type $rtype")
  }

  /** Helper method to check validity of JS data value against the given runtime type.
    *
    * @param data  js value
    * @param rtype type descriptor of Sigma runtime value
    */
  final private def checkJsData[T](data: T, rtype: RType[_]): Any = rtype match {
    case RType.ByteType => data.asInstanceOf[Int].toByteExact
    case RType.ShortType => data.asInstanceOf[Int].toShortExact
    case RType.IntType => data.asInstanceOf[Int].toLong.toIntExact
    case RType.LongType =>
      val n = data.asInstanceOf[js.BigInt]
      if (n < MinLong || n > MaxLong)
        throw new ArithmeticException(s"value $n is out of long range")
      n
    case special.sigma.BigIntRType =>
      data.asInstanceOf[js.BigInt]
    case PairType(l, r) => data match {
      case arr: js.Array[Any @unchecked] =>
        checkJsData(arr(0), l)
        checkJsData(arr(1), r)
        data
      case _ =>
        throw new ArithmeticException(s"$data cannot represent pair value")
    }
    case CollType(elemType) => data match {
      case arr: js.Array[Any @unchecked] =>
        arr.foreach(x => checkJsData(x, elemType))
        data
      case _ =>
        throw new ArithmeticException(s"$data cannot represent Coll value")
    }
    case _ =>
      throw new IllegalArgumentException(s"Unsupported type $rtype")
  }

  /** Create Byte value from JS number. */
  def ofByte(n: Int): Value = {
    checkJsData(n, Type.Byte.rtype)
    new Value(n, Type.Byte)
  }

  /** Create Short value from JS number. */
  def ofShort(n: Int): Value = {
    checkJsData(n, Type.Short.rtype)
    new Value(n, Type.Short)
  }

  /** Create Int value from JS number. */
  def ofInt(n: Int): Value = {
    checkJsData(n, Type.Int.rtype)
    new Value(n, Type.Int)
  }

  /** Create Long value from JS BigInt. */
  def ofLong(n: js.BigInt): Value = {
    checkJsData(n, Type.Long.rtype)
    new Value(n, Type.Long)
  }

  /** Create BigInt value from JS BigInt. */
  def ofBigInt(n: js.BigInt): Value = {
    checkJsData(n, Type.BigInt.rtype)
    new Value(n, Type.BigInt)
  }

  /** Create Pair value from two values. */
  def pairOf(l: Value, r: Value): Value = {
    val data = js.Array(l.data, r.data) // the l and r data have been validated
    new Value(data, Type.pairType(l.tpe, r.tpe))
  }

  /** Create Coll value from array and element type descriptor.
    * @param items collection elements which should be valid JS representation of `elemType`
    * @param elemType descriptor of types for collection elements
    */
  def collOf(items: js.Array[Any], elemType: Type): Value = {
    val t = Type.collType(elemType)
    checkJsData(items, t.rtype)
    new Value(items, t)
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
    val bytes = Base16.decode(hex).fold(t => throw t, identity)
    val S = ConstantSerializer(DeserializationSigmaBuilder)
    val c = S.deserialize(SigmaSerializer.startReader(bytes))
    Isos.isoValueToConstant.from(c)
  }
}
