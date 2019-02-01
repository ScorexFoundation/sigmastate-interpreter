package special.sigma

import java.math.BigInteger

import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utils.Extensions._
import special.collection.Coll
import special.collections.CollGens

/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslTest extends PropSpec with PropertyChecks with Matchers with SigmaTestingCommons with CollGens {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
  }

  def checkEq[A,B](f: A => B)(g: A => B): A => Unit = { x: A =>
    val b1 = f(x); val b2 = g(x)
    assert(b1.getClass == b2.getClass)
    assert(b1 == b2)
  }

  property("Int methods equivalence") {
    val toByte = checkEq(func[Int,Byte]("{ (n: Int) => n.toByte }"))(n => n.toByte)
    val toShort = checkEq(func[Int,Short]("{ (n: Int) => n.toShort }"))(n => n.toShort)
    val toInt = checkEq(func[Int,Int]("{ (n: Int) => n.toInt }"))(n => n.toInt)
    val toLong = checkEq(func[Int,Long]("{ (n: Int) => n.toLong }"))(n => n.toLong)
    val toBigInt = checkEq(func[Int,BigInteger]("{ (n: Int) => n.toBigInt }"))(n => n.toBigInt)
//    val toBytes = checkEq(func[Int,Coll[Byte]]("{ (n: Int) => n.toBytes }"))(n => n.toBytes)

    forAll(valGen) { (n: Int) =>
      whenever(Byte.MinValue <= n && n <= Byte.MaxValue) {
        toByte(n)
      }
      whenever(Short.MinValue <= n && n <= Short.MaxValue) {
        toShort(n)
      }
      toInt(n)
      toLong(n)
//      toBytes(n)
    }
  }
}
