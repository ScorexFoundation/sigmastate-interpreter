package special.sigma.wrappers

import java.math.BigInteger

import scalan.WrapSpec
import special.wrappers.{WrapSpecBase}
import org.bouncycastle.math.ec.ECPoint
import special.sigma.SigmaPredef

import scala.reflect.ClassTag

trait ECPointWrapSpec extends WrapSpecBase {
  def getEncoded[A](g: ECPoint, compressed: Boolean): Array[Byte] = g.getEncoded(compressed)
  def multiply(l: ECPoint, r: BigInteger) = l.multiply(r)
  def add(l: ECPoint, r: ECPoint) = l.add(r)
}

trait BigIntegerWrapSpec extends WrapSpecBase {
  def fromString(s: String) = new BigInteger(s)
  def fromArray(sig: Int, arr: Array[Byte]) = new BigInteger(sig, arr)
  def ZERO = BigInteger.ZERO
  def ONE = BigInteger.ONE
  def valueOf(l: Long) = BigInteger.valueOf(l)
  def toString(l: BigInteger, radix: Int) = l.toString(radix)
  def toByteArray(l: BigInteger): Array[Byte] = l.toByteArray()
  def add(l: BigInteger, r: BigInteger) = l.add(r)
  def subtract(l: BigInteger, r: BigInteger) = l.subtract(r)
  def multiply(l: BigInteger, r: BigInteger) = l.multiply(r)
  def mod(l: BigInteger, r: BigInteger) = l.mod(r)
  def modInverse(l: BigInteger, r: BigInteger) = l.modInverse(r)
  def modPow(l: BigInteger, exponent: BigInteger, m: BigInteger) = l.modPow(exponent, m)
  def remainder(l: BigInteger, r: BigInteger) = l.remainder(r)
  def divide(l: BigInteger, r: BigInteger) = l.divide(r)
  def compareTo(l: BigInteger, r: BigInteger) = l.compareTo(r)
  def min(l: BigInteger, r: BigInteger) = l.min(r)
  def max(l: BigInteger, r: BigInteger) = l.max(r)
  def gcd(l: BigInteger, r: BigInteger) = l.gcd(r)
  def and(l: BigInteger, r: BigInteger) = l.and(r)
  def or(l: BigInteger, r: BigInteger) = l.or(r)
  def xor(l: BigInteger, r: BigInteger) = l.xor(r)
  def not(l: BigInteger) = l.not()
  def andNot(l: BigInteger, r: BigInteger) = l.andNot(r)
  def pow(l: BigInteger, r: Int) = l.pow(r)
  def testBit(l: BigInteger, r: Int) = l.testBit(r)
  def setBit(l: BigInteger, r: Int) = l.setBit(r)
  def clearBit(l: BigInteger, r: Int) = l.clearBit(r)
  def flipBit(l: BigInteger, r: Int) = l.flipBit(r)
  def getLowestSetBit(l: BigInteger) = l.getLowestSetBit()
  def bitCount(l: BigInteger) = l.bitCount()
  def bitLength(l: BigInteger) = l.bitLength()
  def isProbablePrime(l: BigInteger, r: Int) = l.isProbablePrime(r)
  def shiftLeft(l: BigInteger, r: Int) = l.shiftLeft(r)
  def shiftRight(l: BigInteger, r: Int) = l.shiftRight(r)
  def abs(l: BigInteger) = l.abs()
  def negate(l: BigInteger) = l.negate()
  def signum(l: BigInteger) = l.signum()
  def byteValue(l: BigInteger) = l.byteValue()
  def shortValue(l: BigInteger) = l.shortValue()
  def intValue(l: BigInteger) = l.intValue()
  def longValue(l: BigInteger) = l.longValue()
  def byteValueExact(l: BigInteger) = l.byteValueExact()
  def shortValueExact(l: BigInteger) = l.shortValueExact()
  def intValueExact(l: BigInteger) = l.intValueExact()
  def longValueExact(l: BigInteger) = l.longValueExact()

}

trait SigmaPredefWrapSpec extends WrapSpecBase {
  def dataSize(v: Any): Long = SigmaPredef.dataSize(v)
}
