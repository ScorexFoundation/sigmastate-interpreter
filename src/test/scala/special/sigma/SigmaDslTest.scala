package special.sigma

import java.math.BigInteger

import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import sigmastate.helpers.SigmaTestingCommons
import sigma.util.Extensions._
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

  property("Boolean methods equivalence") {
    lazy val toByte = checkEq(func[Boolean,Byte]("{ (x: Boolean) => x.toByte }"))(x => x.toByte)
    forAll { (x: Boolean) =>
//       toByte(x)
    }
  }

  property("Byte methods equivalence") {
    val toByte = checkEq(func[Byte,Byte]("{ (x: Byte) => x.toByte }"))(x => x.toByte)
    val toShort = checkEq(func[Byte,Short]("{ (x: Byte) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Byte,Int]("{ (x: Byte) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Byte,Long]("{ (x: Byte) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Byte,BigInteger]("{ (x: Byte) => x.toBigInt }"))(x => x.toBigInt)
    lazy val toBytes = checkEq(func[Byte,Coll[Byte]]("{ (x: Byte) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Byte,Coll[Boolean]]("{ (x: Byte) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[Byte,Byte]("{ (x: Byte) => x.toAbs }"))(x => x.toAbs)
    lazy val compareTo = checkEq(func[(Byte, Byte), Int]("{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }"))({ (x: (Byte, Byte)) => x._1.compareTo(x._2) })

    forAll { x: Byte =>
      Seq(toInt, toLong, toBigInt/*, toBytes, toBits, toAbs*/).foreach(_(x))
    }
    forAll { x: (Byte, Byte) =>
//      compareTo(x)
    }
  }

  property("Int methods equivalence") {
    val toByte = checkEq(func[Int,Byte]("{ (x: Int) => x.toByte }"))(x => x.toByte)
    val toShort = checkEq(func[Int,Short]("{ (x: Int) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Int,Int]("{ (x: Int) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Int,Long]("{ (x: Int) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Int,BigInteger]("{ (x: Int) => x.toBigInt }"))(x => x.toBigInt)
    lazy val toBytes = checkEq(func[Int,Coll[Byte]]("{ (x: Int) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Int,Coll[Boolean]]("{ (x: Int) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[Int,Int]("{ (x: Int) => x.toAbs }"))(x => x.toAbs)
    lazy val compareTo = checkEq(func[(Int, Int), Int]("{ (x: (Int, Int)) => x._1.compareTo(x._2) }"))(x => x._1.compareTo(x._2))

    forAll(valGen) { x: Int =>
      whenever(Byte.MinValue <= x && x <= Byte.MaxValue) {
        toByte(x)
      }
      whenever(Short.MinValue <= x && x <= Short.MaxValue) {
        toShort(x)
      }
      Seq(toInt, toLong, toBigInt/*, toBytes, toBits, toAbs*/).foreach(_(x))
    }
    forAll { x: (Int, Int) =>
      //      compareTo(x)
    }
  }

  // TODO add tests for Short, Long, BigInt operations

}
