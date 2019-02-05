package special.sigma

import special.collection.{Coll}
import java.math.BigInteger

class CBigInt(private[sigma] val value: BigInteger) extends BigInt {
  val dsl: SigmaDslBuilder = new TestSigmaDslBuilder

  override def toByte : Byte  = value.byteValueExact()
  override def toShort: Short = value.shortValueExact()
  override def toInt  : Int   = value.intValueExact()
  override def toLong : Long  = value.longValueExact()

  override def toBytes: Coll[Byte] = dsl.Colls.fromArray(value.toByteArray)

  override def toBits: Coll[Boolean] = ???

  override def toAbs: BigInt = new CBigInt(value.abs())

  override def compareTo(that: BigInt): Int = value.compareTo(that.value)

  override def modQ: BigInt = ???

  override def plusModQ(other: BigInt): BigInt = ???

  override def minusModQ(other: BigInt): BigInt = ???

  override def multModQ(other: BigInt): BigInt = ???

  override def inverseModQ: BigInt = ???

  override def signum: Int = value.signum()

  override def add(that: BigInt): BigInt = new CBigInt(value.add(that.value))

  override def subtract(that: BigInt): BigInt = new CBigInt(value.subtract(that.value))

  override def multiply(that: BigInt): BigInt = new CBigInt(value.multiply(that.value))

  override def divide(that: BigInt): BigInt = new CBigInt(value.divide(that.value))

  override def mod(m: BigInt): BigInt = new CBigInt(value.mod(m.value))

  override def remainder(that: BigInt): BigInt = new CBigInt(value.remainder(that.value))

  override def min(that: BigInt): BigInt = new CBigInt(value.min(that.value))

  override def max(that: BigInt): BigInt = new CBigInt(value.max(that.value))

  override def negate(): BigInt = new CBigInt(value.negate())
}
