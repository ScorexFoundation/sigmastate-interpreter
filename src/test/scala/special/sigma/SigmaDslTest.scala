package special.sigma

import java.math.BigInteger

import org.scalacheck.Gen.containerOfN
import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{Lookup, BatchAVLProver}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.helpers.SigmaTestingCommons
import sigma.util.Extensions._
import sigmastate.eval.Extensions._
import sigmastate.eval.CostingSigmaDslBuilder
import sigmastate.AvlTreeFlags
import special.collection.Coll
import special.collections.CollGens

trait SigmaTypeGens {
  import sigma.types._
  val genBoolean = Arbitrary.arbBool.arbitrary.map(CBoolean(_): Boolean)
  implicit val arbBoolean = Arbitrary(genBoolean)
  
  val genByte = Arbitrary.arbByte.arbitrary.map(CByte(_): Byte)
  implicit val arbByte = Arbitrary(genByte)

  val genInt = Arbitrary.arbInt.arbitrary.map(CInt(_): Int)
  implicit val arbInt = Arbitrary(genInt)
}

/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslTest extends PropSpec with PropertyChecks with Matchers with SigmaTestingCommons with CollGens with SigmaTypeGens {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
  }

  def checkEq[A,B](f: A => B)(g: A => B): A => Unit = { x: A =>
    val b1 = f(x); val b2 = g(x)
//    assert(b1.getClass == b2.getClass)
    assert(b1 == b2)
  }

  def checkEq2[A,B,R](f: (A, B) => R)(g: (A, B) => R): (A,B) => Unit = { (x: A, y: B) =>
    val r1 = f(x, y); val r2 = g(x, y)
    assert(r1.getClass == r2.getClass)
    assert(r1 == r2)
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
    val toBigInt = checkEq(func[Byte,BigInt]("{ (x: Byte) => x.toBigInt }"))(x => x.toBigInt)
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
    val toBigInt = checkEq(func[Int,BigInt]("{ (x: Int) => x.toBigInt }"))(x => x.toBigInt)
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

  val bytesGen: Gen[Array[Byte]] = containerOfN[Array, Byte](100, Arbitrary.arbByte.arbitrary)
  val bytesCollGen = bytesGen.map(builder.fromArray(_))
  implicit val arbBytes = Arbitrary(bytesCollGen)
  val keyCollGen = bytesCollGen.map(_.slice(0, 32))
  import org.ergoplatform.dsl.AvlTreeHelpers._

  private def sampleAvlTree = {
    val key = keyCollGen.sample.get
    val value = bytesCollGen.sample.get
    val (_, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, ADKey @@ key.toArray -> ADValue @@ value.toArray)
    (key, value, avlProver)
  }

  property("AvlTree properties equivalence") {
    val doDigest = checkEq(func[AvlTree, Coll[Byte]]("{ (t: AvlTree) => t.digest }")) { (t: AvlTree) => t.digest }
    val doEnabledOps = checkEq(func[AvlTree, Byte](
      "{ (t: AvlTree) => t.enabledOperations }")) { (t: AvlTree) => t.enabledOperations }
    val doKeyLength = checkEq(func[AvlTree, Int]("{ (t: AvlTree) => t.keyLength }")) { (t: AvlTree) => t.keyLength }
    val doValueLength = checkEq(func[AvlTree, Option[Int]]("{ (t: AvlTree) => t.valueLengthOpt }")) { (t: AvlTree) => t.valueLengthOpt }
    val okInsert = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isInsertAllowed }")) { (t: AvlTree) => t.isInsertAllowed }
    val okUpdate = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isUpdateAllowed }")) { (t: AvlTree) => t.isUpdateAllowed }
    val okRemove = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isRemoveAllowed }")) { (t: AvlTree) => t.isRemoveAllowed }

    val (key, _, avlProver) = sampleAvlTree
    val digest = avlProver.digest.toColl
    val tree = CostingSigmaDslBuilder.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    doDigest(tree)
    doEnabledOps(tree)
    doKeyLength(tree)
    doValueLength(tree)
    okInsert(tree)
    okUpdate(tree)
    okRemove(tree)
  }

  property("AvlTree.contains equivalence") {
    val doCheck = checkEq(
      func[(AvlTree, (Coll[Byte], Coll[Byte])), Boolean](
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }

    val (key, _, avlProver) = sampleAvlTree
    avlProver.performOneOperation(Lookup(ADKey @@ key.toArray))
    val digest = avlProver.digest.toColl
    val proof = avlProver.generateProof().toColl
    val tree = CostingSigmaDslBuilder.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    doCheck((tree, (key, proof)))
  }

}
