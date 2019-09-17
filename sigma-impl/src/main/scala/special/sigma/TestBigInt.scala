package special.sigma

import special.collection.{Coll}
import java.math.BigInteger
import scalan.util.Extensions.BigIntegerOps

abstract class TestBigInt(private[sigma] val value: BigInteger) extends BigInt {
  val dsl: TestSigmaDslBuilder = new TestSigmaDslBuilder

  override def toByte : Byte  = value.byteValueExact()
  override def toShort: Short = value.shortValueExact()
  override def toInt  : Int   = value.intValueExact()
  override def toLong : Long  = value.longValueExact()

  override def toBytes: Coll[Byte] = dsl.Colls.fromArray(value.toByteArray)

  override def toBits: Coll[Boolean] = ???

  override def toAbs: BigInt = dsl.BigInt(value.abs())

  override def compareTo(that: BigInt): Int = value.compareTo(that.value)

  override def modQ: BigInt = ???

  override def plusModQ(other: BigInt): BigInt = ???

  override def minusModQ(other: BigInt): BigInt = ???

  override def multModQ(other: BigInt): BigInt = ???

  override def inverseModQ: BigInt = ???

  override def signum: Int = value.signum()

  override def add(that: BigInt): BigInt = dsl.BigInt(value.add(that.value).to256BitValueExact)

  override def subtract(that: BigInt): BigInt = dsl.BigInt(value.subtract(that.value).to256BitValueExact)

  override def multiply(that: BigInt): BigInt = dsl.BigInt(value.multiply(that.value).to256BitValueExact)

  override def divide(that: BigInt): BigInt = dsl.BigInt(value.divide(that.value))

  override def mod(m: BigInt): BigInt = dsl.BigInt(value.mod(m.value))

  override def remainder(that: BigInt): BigInt = dsl.BigInt(value.remainder(that.value))

  override def min(that: BigInt): BigInt = dsl.BigInt(value.min(that.value))

  override def max(that: BigInt): BigInt = dsl.BigInt(value.max(that.value))

  override def negate(): BigInt = dsl.BigInt(value.negate().to256BitValueExact)
}
