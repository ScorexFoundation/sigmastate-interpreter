package sigmastate

import java.math.BigInteger

import scalan.RType
import sigmastate.basics.DLogProtocol
import sigmastate.interpreter.CryptoConstants
import special.sigma.SigmaTestingData
import sigmastate.SType.AnyOps
import sigmastate.eval.SigmaDsl
import special.collection.Coll
import special.sigma.BigInt

class TypesSpecificatio extends SigmaTestingData {

  def Coll[T](items: T*)(implicit cT: RType[T]) = SigmaDsl.Colls.fromItems(items:_*)

  property("SType.dataSize") {
    def check(tpe: SType, v: Any, exp: Long) =
      tpe.dataSize(v.asWrappedType) shouldBe exp

    def checkColl[T](elemTpe: SType, arr: Coll[T], exp: Long) =
      check(sigmastate.SCollection(elemTpe), arr, exp)

    check(SBoolean, true, 1)
    check(SByte, 1.toByte, 1)
    check(SShort, 1.toShort, 2)
    check(SInt, 1, 4)
    check(SLong, 1, 8)
    check(SString, "abc", 3)
    check(SBigInt, BigInteger.ZERO, 32L)
    check(SBigInt, BigInteger.ONE, 32L)
    check(SBigInt, BigInteger.valueOf(Long.MaxValue), 32L)
    check(SBigInt, { val i = BigInteger.valueOf(Long.MaxValue); i.multiply(i) }, 32L)
    forAll { n: BigInt =>
      check(SBigInt, n, 32L)

    }
    val g = CryptoConstants.dlogGroup.generator
    check(SGroupElement, g, 33L)
    check(SSigmaProp, DLogProtocol.ProveDlog(g), 1024L)
    check(sigmastate.SOption(SInt), Some(10), 1L + 4)
    checkColl(SInt, Coll[Int](10,20), 2L * 4)
    checkColl(SInt, Coll[Int](), 0L)
    checkColl(SBigInt, Coll[BigInt](SigmaDsl.BigInt(BigInteger.ZERO), SigmaDsl.BigInt(BigInteger.valueOf(Long.MaxValue))), 2 * 32L)
    check(STuple(SInt, STuple(SInt, SInt)), Array(10, Array[Any](20, 30)), 2 + 4 + (2 + 4 + 4))
  }

}
