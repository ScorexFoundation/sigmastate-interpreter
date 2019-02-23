package special.sigma

import java.math.BigInteger

import com.google.common.primitives.Longs
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalacheck.{Arbitrary, Gen}
import sigmastate.helpers.SigmaTestingCommons
import sigma.util.Extensions._
import sigmastate.eval.CBigInt
import sigmastate.serialization.generators.ValueGenerators
import special.collection.{Builder, Coll}
import special.collections.CollGens

trait SigmaTypeGens extends ValueGenerators {
  import Gen._; import Arbitrary._
  import sigma.types._
  val genBoolean = Arbitrary.arbBool.arbitrary.map(CBoolean(_): Boolean)
  implicit val arbBoolean = Arbitrary(genBoolean)
  
  val genByte = Arbitrary.arbByte.arbitrary.map(CByte(_): Byte)
  implicit val arbByte = Arbitrary(genByte)

  val genInt = Arbitrary.arbInt.arbitrary.map(CInt(_): Int)
  implicit val arbInt = Arbitrary(genInt)

  val genBigInt = arbBigInteger.arbitrary.map(CBigInt(_): BigInt)
  implicit val arbBigInt = Arbitrary(genBigInt)
}

/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslTest extends PropSpec with PropertyChecks with Matchers with SigmaTestingCommons with CollGens with SigmaTypeGens {
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
    forAll { x: Boolean =>
      x.toByte
    }
  }

  property("Byte methods equivalence") {
    val toShort = checkEq(func[Byte,Short]("{ (x: Byte) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Byte,Int]("{ (x: Byte) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Byte,Long]("{ (x: Byte) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Byte,BigInteger]("{ (x: Byte) => x.toBigInt }"))(x => x.toBigInt)
    lazy val toBytes = checkEq(func[Byte,Coll[Byte]]("{ (x: Byte) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Byte,Coll[Boolean]]("{ (x: Byte) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[Byte,Byte]("{ (x: Byte) => x.toAbs }"))(x => x.toAbs)
    lazy val compareTo = checkEq(func[(Byte, Byte), Int]("{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }"))({ (x: (Byte, Byte)) => x._1.compareTo(x._2) })

    forAll { x: Byte =>
      Seq(toInt, toLong, toBigInt).foreach(_(x))
//TODO toBytes, toBits, toAbs
    }
    forAll { x: (Byte, Byte) =>
//TODO  compareTo(x)
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
      whenever(Byte.MinValue <= x && x <= scala.Byte.MaxValue) {
        toByte(x)
      }
      whenever(Short.MinValue <= x && x <= Short.MaxValue) {
        toShort(x)
      }
      Seq(toInt, toLong, toBigInt).foreach(_(x))
      //TODO toBytes, toBits, toAbs
    }
    forAll { x: (Int, Int) =>
      //TODO  compareTo(x)
    }
  }
  // TODO add tests for Short, Long, BigInt operations

  property("sigma.types.Byte methods equivalence") {
    import sigma.types._
    val toInt = checkEq(func[Byte,Int]("{ (x: Byte) => x.toInt }"))(x => x.toInt)
    forAll { x: Byte =>
      Seq(toInt).foreach(_(x))
    }
  }

  property("sigma.types.Int methods equivalence") {
    import sigma.types._
    val toByte = checkEq(func[Int,Byte]("{ (x: Int) => x.toByte }"))(x => x.toByte)
    lazy val compareTo = checkEq(func[(Int, Int), Int]("{ (x: (Int, Int)) => x._1.compareTo(x._2) }"))(x => x._1.compareTo(x._2))
    forAll(valGen) { in: scala.Int =>
      whenever(scala.Byte.MinValue <= in && in <= scala.Byte.MaxValue) {
        val x = CInt(in)
        toByte(x)
      }
    }
  }

  property("byteArrayToLong equivalence") {
    val eq = checkEq(func[Coll[Byte],Long]("{ (x: Coll[Byte]) => byteArrayToLong(x) }")){ x =>
      Longs.fromByteArray(x.toArray)
    }
    forAll { x: Array[Byte] =>
      whenever(x.size >= 8) {
        eq(Builder.DefaultCollBuilder.fromArray(x))
      }
    }
  }

  property("xorOf equivalence") {
    val eq = checkEq(func[Coll[Boolean], Boolean]("{ (x: Coll[Boolean]) => xorOf(x) }")) { x =>
      x.toArray.distinct.length == 2
    }
    forAll { x: Array[Boolean] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  property("LogicalNot equivalence") {
    val eq = checkEq(func[Boolean, Boolean]("{ (x: Boolean) => !x }")) { x => !x }
    forAll { x: Boolean => eq(x) }
  }

  property("Negation equivalence") {
    val negByte = checkEq(func[Byte, Byte]("{ (x: Byte) => -x }")) { x => (-x).toByte }
    forAll { x: Byte => negByte(x) }
    val negShort = checkEq(func[Short, Short]("{ (x: Short) => -x }")) { x => (-x).toShort }
    forAll { x: Short => negShort(x) }
    val negInt = checkEq(func[Int, Int]("{ (x: Int) => -x }")) { x => -x }
    forAll { x: Int => negInt(x) }
    val negLong = checkEq(func[Long, Long]("{ (x: Long) => -x }")) { x => -x }
    forAll { x: Long => negLong(x) }
    // TODO add scala.BigInt test
  }

  ignore("special.sigma.BigInt Negation equivalence") {
    // TODO fix return type (java.math.BigInteger)
    // TODO make negate() into a prefix method
    val negBigInteger = checkEq(func[BigInt, BigInt]("{ (x: BigInt) => -x }")) { x => x.negate() }
    forAll { x: BigInt => negBigInteger(x) }
  }

  ignore("BinXor(logical XOR) equivalence") {
    // TODO implement
    val eq = checkEq(func[(Boolean, Boolean), Boolean]("{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }")) {
      x => x._1 ^ x._2
    }
    forAll { x: (Boolean, Boolean) => eq(x) }
  }
}
