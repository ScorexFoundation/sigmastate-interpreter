package special.sigma

import java.math.BigInteger

import org.ergoplatform.ErgoLikeContext.dummyPubkey
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec}
import org.ergoplatform._
import org.scalacheck.Gen.containerOfN
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scalan.RType
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.util.Extensions._
import sigmastate.Values.{BooleanConstant, EvaluatedValue, IntConstant}
import sigmastate._
import sigmastate.Values._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptEnv, ScriptNameProp}
import special.collection.{Builder, Coll}


/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslTest extends PropSpec
  with PropertyChecks
  with Matchers
  with SigmaTestingData with SigmaContractSyntax
  with SigmaTypeGens { suite =>

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  override def contractEnv: ScriptEnv = Map()

  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
  }

  def checkEq[A,B](f: A => B)(g: A => B): A => Unit = { x: A =>
    val b1 = f(x); val b2 = g(x)
    assert(b1 == b2)
  }

  def checkEq2[A,B,R](f: (A, B) => R)(g: (A, B) => R): (A,B) => Unit = { (x: A, y: B) =>
    val r1 = f(x, y); val r2 = g(x, y)
    assert(r1.getClass == r2.getClass)
    assert(r1 == r2)
  }

  def getRandomIndex(size: Int): Int = {
    val r = scala.util.Random
    if (size > 1) r.nextInt(size) else 0
  }

  def makeSlicePair(size: Int): (Int, Int) = {
    val r = scala.util.Random
    val rBorder = getRandomIndex(size)
    val lBorder = getRandomIndex(rBorder)
    (lBorder, rBorder)
  }

  // TODO: make more effective
  def generateIndexColl(maxSize: Int): Coll[Int] = {
    var ret: Coll[Int] = Colls.emptyColl
    var index = getRandomIndex(maxSize)
    while (index > 0) {
      ret = ret.append(Colls.fromArray(Array(index)))
      index = getRandomIndex(index)
    }
    ret
  }

  case class EqualityChecker[T: RType](obj: T) {
    def apply[R: RType](dslFunc: T => R)(script: String) =
      checkEq(func[T, R](script))(dslFunc)(obj)
  }

  ignore("Boolean methods equivalence") {
    lazy val toByte = checkEq(func[Boolean,Byte]("{ (x: Boolean) => x.toByte }"))((x: Boolean) => x.toByte)
    forAll { x: Boolean =>
      //TODO soft-fork: for new operation below
      Seq(toByte).foreach(_(x))
    }
  }

  property("Byte methods equivalence") {
    val toByte = checkEq(func[Byte, Byte]("{ (x: Byte) => x.toByte }"))(x => x.toByte)
    val toShort = checkEq(func[Byte,Short]("{ (x: Byte) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Byte,Int]("{ (x: Byte) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Byte,Long]("{ (x: Byte) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Byte,BigInt]("{ (x: Byte) => x.toBigInt }"))(x => x.toBigInt)

    //TODO soft-fork: for new 4 operations below
    lazy val toBytes = checkEq(func[Byte,Coll[Byte]]("{ (x: Byte) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Byte,Coll[Boolean]]("{ (x: Byte) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[Byte,Byte]("{ (x: Byte) => x.toAbs }"))(x => x.toAbs)
    lazy val compareTo = checkEq(func[(Byte, Byte), Int]("{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }"))({ (x: (Byte, Byte)) => x._1.compareTo(x._2) })

    forAll { x: Byte =>
      Seq(toByte, toShort, toInt, toLong, toBigInt).foreach(_(x))
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

    forAll { x: Int =>
      whenever(Byte.MinValue <= x && x <= scala.Byte.MaxValue) {
        toByte(x)
      }
      whenever(Short.MinValue <= x && x <= Short.MaxValue) {
        toShort(x)
      }
      Seq(toInt, toLong, toBigInt).foreach(_(x))
      //TODO soft-fork: toBytes, toBits, toAbs
    }
    forAll { x: (Int, Int) =>
      //TODO soft-fork: compareTo(x)
    }
  }

  property("Short methods equivalence") {
    val toByte = checkEq(func[Short,Byte]("{ (x: Short) => x.toByte }"))(x => x.toByte)
    val toShort = checkEq(func[Short,Short]("{ (x: Short) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Short,Int]("{ (x: Short) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Short,Long]("{ (x: Short) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Short,BigInt]("{ (x: Short) => x.toBigInt }"))(x => x.toBigInt)
    lazy val toBytes = checkEq(func[Short,Coll[Byte]]("{ (x: Short) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Short,Coll[Boolean]]("{ (x: Short) => x.toBits }"))(x => x.toBits)
    // TODO: Implement Short.toAbs
    lazy val toAbs = checkEq(func[Short,Short]("{ (x: Short) => x.toAbs }"))((x: Short) => if (x >= 0.toShort) x else (-x).toShort)
    lazy val compareTo = checkEq(func[(Short, Short), Int]("{ (x: (Short, Short)) => x._1.compareTo(x._2) }"))(x => x._1.compareTo(x._2))

    forAll { x: Short =>
      whenever(Byte.MinValue <= x && x <= scala.Byte.MaxValue) {
        toByte(x)
      }
      Seq(toShort, toInt, toLong, toBigInt).foreach(_(x))
      //TODO soft-fork: toBytes, toBits, toAbs
    }
    forAll { x: (Short, Short) =>
      //TODO soft-fork: compareTo(x)
    }
  }

  property("Long methods equivalence") {
    val toByte = checkEq(func[Long,Byte]("{ (x: Long) => x.toByte }"))(x => x.toByte)
    val toShort = checkEq(func[Long,Short]("{ (x: Long) => x.toShort }"))(x => x.toShort)
    val toInt = checkEq(func[Long,Int]("{ (x: Long) => x.toInt }"))(x => x.toInt)
    val toLong = checkEq(func[Long,Long]("{ (x: Long) => x.toLong }"))(x => x.toLong)
    val toBigInt = checkEq(func[Long,BigInt]("{ (x: Long) => x.toBigInt }"))(x => BigInt(x).bigInteger)
    /*
    lazy val toBytes = checkEq(func[Long,Coll[Byte]]("{ (x: Long) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[Long,Coll[Boolean]]("{ (x: Long) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[Long,Long]("{ (x: Long) => x.toAbs }"))(x => x.toAbs)
    */
    lazy val compareTo = checkEq(func[(Long, Long), Int]("{ (x: (Long, Long)) => x._1.compareTo(x._2) }"))(x => x._1.compareTo(x._2))

    forAll { x: Long =>
      whenever(Byte.MinValue <= x && x <= scala.Byte.MaxValue) {
        toByte(x)
      }
      whenever(Short.MinValue <= x && x <= Short.MaxValue) {
        toShort(x)
      }
      whenever(Int.MinValue <= x && x <= Int.MaxValue) {
        toInt(x)
      }
      Seq(toLong, toBigInt).foreach(_(x))
      //TODO soft-fork: toBytes, toBits, toAbs
    }
    forAll { x: (Long, Long) =>
      //TODO soft-fork: compareTo(x)
    }
  }
  property("BigInt methods equivalence") {
    val toByte = checkEq(func[(Byte, BigInt),Boolean]("{ (x: (Byte, BigInt)) => x._2.toByte == x._1 }")) { x =>
      x._2.toByte == x._1
    }
    val toShort = checkEq(func[(Short, BigInt),Boolean]("{ (x: (Short, BigInt)) => x._2.toShort == x._1 }")) { x =>
      x._2.toShort == x._1
    }
    val toInt = checkEq(func[(Int, BigInt),Boolean]("{ (x: (Int, BigInt)) => x._2.toInt == x._1 }")) { x =>
      x._2.toInt == x._1
    }
    val toLong = checkEq(func[(Long, BigInt),Boolean]("{ (x: (Long, BigInt)) => x._2.toLong == x._1 }")) { x =>
      x._2.toLong == x._1
    }
    val toBigInt = checkEq(func[(BigInt, BigInt),Boolean]("{ (x: (BigInt, BigInt)) => x._2.toBigInt == x._1 }")) { x =>
      x._2 == x._1
    }

    lazy val toBytes = checkEq(func[BigInt,Coll[Byte]]("{ (x: BigInt) => x.toBytes }"))(x => x.toBytes)
    lazy val toBits = checkEq(func[BigInt,Coll[Boolean]]("{ (x: BigInt) => x.toBits }"))(x => x.toBits)
    lazy val toAbs = checkEq(func[BigInt,BigInt]("{ (x: BigInt) => x.toAbs }"))(x => x.toAbs)
    lazy val compareTo = checkEq(func[(BigInt, BigInt), Int]("{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }"))(x => x._1.compareTo(x._2))

    /*
    forAll { x: Byte =>
      toByte((x, x.toBigInt))
    }
    forAll { x: Short =>
      toShort((x, x.toBigInt))
    }
    forAll { x: Int =>
      toInt((x, x.toBigInt))
    }
    forAll { x: Long =>
      toLong((x, BigInt(x).bigInteger))
    }
    */
    forAll { x: BigInt =>
      Seq(toBigInt).foreach(_((x, x)))
      //TODO soft-fork: toBytes, toBits, toAbs
    }
    forAll { x: (BigInt, BigInt) =>
      //TODO soft-fork: compareTo(x)
    }
  }

  property("GroupElement operations equivalence") {
    val ge = SigmaDsl.groupGenerator
    val n = SigmaDsl.BigInt(BigInteger.TEN)
    val g2 = ge.exp(n)

    {
      val eq = EqualityChecker(ge)
      eq({ (x: GroupElement) => x.getEncoded })("{ (x: GroupElement) => x.getEncoded }")
      eq({ (x: GroupElement) => decodePoint(x.getEncoded) == x })("{ (x: GroupElement) => decodePoint(x.getEncoded) == x }")
      eq({ (x: GroupElement) => x.negate })("{ (x: GroupElement) => x.negate }")

      //TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
      // eq({ (x: GroupElement) => x.isIdentity })("{ (x: GroupElement) => x.isIdentity }")
    }

    {
      val eq = EqualityChecker((ge, n))
      eq({ (x: (GroupElement, BigInt)) => x._1.exp(x._2) })("{ (x: (GroupElement, BigInt)) => x._1.exp(x._2) }")
    }

    {
      val eq = EqualityChecker((ge, g2))
      eq({ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) })("{ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) }")
    }
  }

  property("AvlTree properties equivalence") {
    val doDigest = checkEq(func[AvlTree, Coll[Byte]]("{ (t: AvlTree) => t.digest }")) { (t: AvlTree) => t.digest }
    val doEnabledOps = checkEq(func[AvlTree, Byte](
      "{ (t: AvlTree) => t.enabledOperations }")) { (t: AvlTree) => t.enabledOperations }
    val doKeyLength = checkEq(func[AvlTree, Int]("{ (t: AvlTree) => t.keyLength }")) { (t: AvlTree) => t.keyLength }
    val doValueLength = checkEq(func[AvlTree, Option[Int]]("{ (t: AvlTree) => t.valueLengthOpt }")) { (t: AvlTree) => t.valueLengthOpt }
    val insertAllowed = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isInsertAllowed }")) { (t: AvlTree) => t.isInsertAllowed }
    val updateAllowed = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isUpdateAllowed }")) { (t: AvlTree) => t.isUpdateAllowed }
    val removeAllowed = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isRemoveAllowed }")) { (t: AvlTree) => t.isRemoveAllowed }

    val newTree = sampleAvlTree.updateOperations(1.toByte)
    val trees = Array(sampleAvlTree, newTree)

    for (tree <- trees) {
      doDigest(tree)
      doEnabledOps(tree)
      doKeyLength(tree)
      doValueLength(tree)
      insertAllowed(tree)
      updateAllowed(tree)
      removeAllowed(tree)
    }
  }

  property("AvlTree.{contains, get, getMany} equivalence") {
    val doContains = checkEq(
      func[(AvlTree, (Coll[Byte], Coll[Byte])), Boolean](
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }
    val doGet = checkEq(
      func[(AvlTree, (Coll[Byte], Coll[Byte])), Option[Coll[Byte]]](
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2) }
    val doGetMany = checkEq(
      func[(AvlTree, (Coll[Coll[Byte]], Coll[Byte])), Coll[Option[Coll[Byte]]]](
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2) }

    val (key, _, avlProver) = sampleAvlProver
    avlProver.performOneOperation(Lookup(ADKey @@ key.toArray))
    val digest = avlProver.digest.toColl
    val proof = avlProver.generateProof().toColl
    val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    doContains((tree, (key, proof)))
    doGet((tree, (key, proof)))
    val keys = Colls.fromItems(key)
    doGetMany((tree, (keys, proof)))
  }

  property("AvlTree.{insert, update, remove} equivalence") {
    type KV = (Coll[Byte], Coll[Byte])
    val doInsert = checkEq(
      func[(AvlTree, (Coll[KV], Coll[Byte])), Option[AvlTree]](
      "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2) }
    val doUpdate = checkEq(
      func[(AvlTree, (Coll[KV], Coll[Byte])), Option[AvlTree]](
        "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.update(t._2._1, t._2._2) }"))
        { (t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.update(t._2._1, t._2._2) }
    val doRemove = checkEq(
      func[(AvlTree, (Coll[Coll[Byte]], Coll[Byte])), Option[AvlTree]](
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2) }

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val key = Array.fill(32)(1.toByte).toColl

    {
      val preInsertDigest = avlProver.digest.toColl
      val value = bytesCollGen.sample.get
      avlProver.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
      val insertProof = avlProver.generateProof().toColl
      val preInsertTree = SigmaDsl.avlTree(AvlTreeFlags(true, false, false).serializeToByte, preInsertDigest, 32, None)
      val insertKvs = Colls.fromItems((key -> value))
      doInsert((preInsertTree, (insertKvs, insertProof)))
    }

    {
      val preUpdateDigest = avlProver.digest.toColl
      val newValue = bytesCollGen.sample.get
      avlProver.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ newValue.toArray))
      val updateProof = avlProver.generateProof().toColl
      val preUpdateTree = SigmaDsl.avlTree(AvlTreeFlags(false, true, false).serializeToByte, preUpdateDigest, 32, None)
      val updateKvs = Colls.fromItems((key -> newValue))
      doUpdate((preUpdateTree, (updateKvs, updateProof)))
    }

    {
      val preRemoveDigest = avlProver.digest.toColl
      avlProver.performOneOperation(Remove(ADKey @@ key.toArray))
      val removeProof = avlProver.generateProof().toColl
      val preRemoveTree = SigmaDsl.avlTree(AvlTreeFlags(false, false, true).serializeToByte, preRemoveDigest, 32, None)
      val removeKeys = Colls.fromItems(key)
      doRemove((preRemoveTree, (removeKeys, removeProof)))
    }
  }

  property("longToByteArray equivalence") {
    val eq = checkEq(func[Long, Coll[Byte]]("{ (x: Long) => longToByteArray(x) }")){ x =>
      longToByteArray(x)
    }
    forAll { x: Long => eq(x) }
  }

  property("byteArrayToBigInt equivalence") {
    val eq = checkEq(func[Coll[Byte], BigInt]("{ (x: Coll[Byte]) => byteArrayToBigInt(x) }")){ x =>
      byteArrayToBigInt(x)
    }
    forAll { x: Array[Byte] =>
      whenever(x.length <= ErgoConstants.MaxBigIntSizeInBytes.get) {
        eq(Builder.DefaultCollBuilder.fromArray(x))
      }
    }
  }

  property("byteArrayToLong equivalence") {
    val eq = checkEq(func[Coll[Byte],Long]("{ (x: Coll[Byte]) => byteArrayToLong(x) }")){ x =>
      byteArrayToLong(x)
    }
    forAll { x: Array[Byte] =>
      whenever(x.length >= 8) {
        eq(Builder.DefaultCollBuilder.fromArray(x))
      }
    }
  }

  // TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/427
  // TODO costing: expression t._1(t._2) cannot be costed because t is lambda argument
  //  ignore("Func context variable") {
  //    val doApply = checkEq(func[(Int => Int, Int), Int]("{ (t: (Int => Int, Int)) => t._1(t._2) }")) { (t: (Int => Int, Int)) => t._1(t._2) }
  //    val code = compileWithCosting(emptyEnv, s"{ (x: Int) => x + 1 }")
  //    val ctx = ErgoLikeContext.dummy(fakeSelf)
  //    doApply((CFunc[Int, Int](ctx, code), 10))
  //  }

  lazy val ctx = ergoCtx.toSigmaContext(IR, false)

  property("Box properties equivalence") {
    val box = ctx.dataInputs(0)
    val eq = EqualityChecker(box)
    eq({ (x: Box) => x.id })("{ (x: Box) => x.id }")
    eq({ (x: Box) => x.value })("{ (x: Box) => x.value }")
    eq({ (x: Box) => x.propositionBytes })("{ (x: Box) => x.propositionBytes }")
    eq({ (x: Box) => x.bytes })("{ (x: Box) => x.bytes }")
    eq({ (x: Box) => x.bytesWithoutRef })("{ (x: Box) => x.bytesWithoutRef }")
    eq({ (x: Box) => x.creationInfo })("{ (x: Box) => x.creationInfo }")
    eq({ (x: Box) => x.tokens })("{ (x: Box) => x.tokens }")
  }

  property("Advanced Box test") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    avlProver.generateProof()

    val digest = avlProver.digest
    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))
    val box = ctx.dataInputs(0)

    val s = ErgoBox(20, TrueProp, 0, Seq(),Map(
      ErgoBox.nonMandatoryRegisters(0) -> ByteConstant(1.toByte),
      ErgoBox.nonMandatoryRegisters(1) -> ShortConstant(1024.toShort),
      ErgoBox.nonMandatoryRegisters(2) -> IntConstant(1024 * 1024),
      ErgoBox.nonMandatoryRegisters(3) -> LongConstant(1024.toLong),
      ErgoBox.nonMandatoryRegisters(4) -> BigIntConstant(222L),
      ErgoBox.nonMandatoryRegisters(5) -> AvlTreeConstant(treeData)
    ))
    lazy val byteCheck = checkEq(func[Box,Byte]("{ (x: Box) => x.R4[Byte].get }"))((x: Box) => x.R4[Byte].get)
    lazy val shortCheck = checkEq(func[Box,Short]("{ (x: Box) => x.R5[Short].get }"))((x: Box) => x.R5[Short].get)
    lazy val intCheck = checkEq(func[Box,Int]("{ (x: Box) => x.R6[Int].get }"))((x: Box) => x.R6[Int].get)
    lazy val longCheck = checkEq(func[Box,Long]("{ (x: Box) => x.R7[Long].get }"))((x: Box) => x.R7[Long].get)
    lazy val BigIntCheck = checkEq(func[Box,BigInt]("{ (x: Box) => x.R8[BigInt].get }"))((x: Box) => x.R8[BigInt].get)
    byteCheck(s)
    shortCheck(s)
    intCheck(s)
    longCheck(s)
    BigIntCheck(s)
  }

  property("PreHeader properties equivalence") {
    val h = ctx.preHeader
    val eq = EqualityChecker(h)
    eq({ (x: PreHeader) => x.version })("{ (x: PreHeader) => x.version }")
    eq({ (x: PreHeader) => x.parentId })("{ (x: PreHeader) => x.parentId }")
    eq({ (x: PreHeader) => x.timestamp })("{ (x: PreHeader) => x.timestamp }")
    eq({ (x: PreHeader) => x.nBits })("{ (x: PreHeader) => x.nBits }")
    eq({ (x: PreHeader) => x.height })("{ (x: PreHeader) => x.height }")
    eq({ (x: PreHeader) => x.minerPk })("{ (x: PreHeader) => x.minerPk }")
    eq({ (x: PreHeader) => x.votes })("{ (x: PreHeader) => x.votes }")
  }

  property("Header properties equivalence") {
    val h = ctx.headers(0)
    val eq = EqualityChecker(h)
    eq({ (x: Header) => x.id })("{ (x: Header) => x.id }")
    eq({ (x: Header) => x.version })("{ (x: Header) => x.version }")
    eq({ (x: Header) => x.parentId })("{ (x: Header) => x.parentId }")
    eq({ (x: Header) => x.ADProofsRoot})("{ (x: Header) => x.ADProofsRoot}")
    eq({ (x: Header) => x.stateRoot })("{ (x: Header) => x.stateRoot }")
    eq({ (x: Header) => x.transactionsRoot })("{ (x: Header) => x.transactionsRoot }")
    eq({ (x: Header) => x.timestamp })("{ (x: Header) => x.timestamp }")
    eq({ (x: Header) => x.nBits })("{ (x: Header) => x.nBits }")
    eq({ (x: Header) => x.height })("{ (x: Header) => x.height }")
    eq({ (x: Header) => x.extensionRoot })("{ (x: Header) => x.extensionRoot }")
    eq({ (x: Header) => x.minerPk })("{ (x: Header) => x.minerPk }")
    eq({ (x: Header) => x.powOnetimePk })("{ (x: Header) => x.powOnetimePk }")
    eq({ (x: Header) => x.powNonce })("{ (x: Header) => x.powNonce }")
    eq({ (x: Header) => x.powDistance })("{ (x: Header) => x.powDistance }")
    eq({ (x: Header) => x.votes })("{ (x: Header) => x.votes }")
  }

  property("Context properties equivalence") {
    val eq = EqualityChecker(ctx)
    eq({ (x: Context) => x.dataInputs })("{ (x: Context) => x.dataInputs }")
    eq({ (x: Context) => x.dataInputs(0) })("{ (x: Context) => x.dataInputs(0) }")
    eq({ (x: Context) => x.dataInputs(0).id })("{ (x: Context) => x.dataInputs(0).id }")
    eq({ (x: Context) => x.preHeader })("{ (x: Context) => x.preHeader }")
    eq({ (x: Context) => x.headers })("{ (x: Context) => x.headers }")
    eq({ (x: Context) => x.OUTPUTS })("{ (x: Context) => x.OUTPUTS }")
    eq({ (x: Context) => x.INPUTS })("{ (x: Context) => x.INPUTS }")
    eq({ (x: Context) => x.HEIGHT })("{ (x: Context) => x.HEIGHT }")
    eq({ (x: Context) => x.SELF })("{ (x: Context) => x.SELF }")
    eq({ (x: Context) => x.INPUTS.map { (b: Box) => b.value } })("{ (x: Context) => x.INPUTS.map { (b: Box) => b.value } }")
    eq({ (x: Context) => x.selfBoxIndex })("{ (x: Context) => x.selfBoxIndex }")
    eq({ (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed })("{ (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed }")
    eq({ (x: Context) => x.minerPubKey })("{ (x: Context) => x.minerPubKey }")
    eq({ (x: Context) =>
      x.INPUTS.map { (b: Box) => (b.value, b.value) }
    })(
      """{ (x: Context) =>
       |  x.INPUTS.map { (b: Box) => (b.value, b.value) }
       |}""".stripMargin
    )

    eq({ (x: Context) =>
      x.INPUTS.map { (b: Box) =>
        val pk = b.R4[Int].get
        val value = longToByteArray(b.value)
        (pk, value)
      }
    })(
    """{ (x: Context) =>
     |  x.INPUTS.map { (b: Box) =>
     |    val pk = b.R4[Int].get
     |    val value = longToByteArray(b.value)
     |    (pk, value)
     |  }
     |}""".stripMargin)
  }

  property("getVar equivalence") {
    val eq = checkEq(func[Int,Int]("{ (x: Int) => getVar[Int](2).get }", 2.toByte -> IntConstant(10))) { x =>
      10
    }
    eq(1)
  }

  property("xorOf equivalence") {
    val eq = checkEq(func[Coll[Boolean], Boolean]("{ (x: Coll[Boolean]) => xorOf(x) }")) { x =>
      xorOf(x)
    }
    forAll { x: Array[Boolean] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  property("LogicalNot equivalence") {
    // TODO make a prefix method
    val eq = checkEq(func[Boolean, Boolean]("{ (x: Boolean) => !x }")) { x => !x }
    forAll { x: Boolean => eq(x) }
  }

  property("Negation equivalence") {
    // TODO make a prefix method
    val negByte = checkEq(func[Byte, Byte]("{ (x: Byte) => -x }")) { (x: Byte) => (-x).toByte }
    forAll { x: Byte => negByte(x) }
    val negShort = checkEq(func[Short, Short]("{ (x: Short) => -x }")) { (x: Short) => (-x).toShort }
    forAll { x: Short => negShort(x) }
    val negInt = checkEq(func[Int, Int]("{ (x: Int) => -x }")) { (x: Int) => -x }
    forAll { x: Int => negInt(x) }
    val negLong = checkEq(func[Long, Long]("{ (x: Long) => -x }")) { (x: Long) => -x }
    forAll { x: Long => negLong(x) }
  }

  property("special.sigma.BigInt Negation equivalence") {
    // TODO make negate() into a prefix method
    val negBigInteger = checkEq(func[BigInt, BigInt]("{ (x: BigInt) => -x }")) { (x: BigInt) => x.negate() }
    forAll { x: BigInt => negBigInteger(x) }
  }

  property("BinXor(logical XOR) equivalence") {
    val eq = checkEq(func[(Boolean, Boolean), Boolean]("{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }")) {
      x => x._1 ^ x._2
    }
    forAll { x: (Boolean, Boolean) => eq(x) }
  }

  property("BinXor(logical XOR) test") {
    val eq = checkEq(func[(Int, Boolean), Boolean]("{ (x: (Int, Boolean)) => (x._1 == 0) ^ x._2 }")) {
      x => (x._1 == 0) ^ x._2
    }
    forAll { x: (Int, Boolean) => eq(x) }
  }

  // TODO: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
  ignore("Box.getReg equivalence") {
//    val eq = checkEq(func[Box, Int]("{ (x: Box) => x.getReg[Int](1).get }")) { x => x.getReg(1).get }
//    forAll { x: Box => eq(x) }
  }

  property("global functions equivalence") {
    val n = SigmaDsl.BigInt(BigInteger.TEN)
    val Global = SigmaDsl

    {
      val eq = EqualityChecker(1)
      eq({ (x: Int) => groupGenerator })("{ (x: Int) => groupGenerator }")
      eq({ (x: Int) => Global.groupGenerator })("{ (x: Int) => Global.groupGenerator }")
    }

    {
      val eq = EqualityChecker(n)
      eq({ (n: BigInt) => groupGenerator.exp(n) })("{ (n: BigInt) => groupGenerator.exp(n) }")
    }

    {
      val eq = checkEq(func[(Coll[Byte], Coll[Byte]), Coll[Byte]](
        "{ (x: (Coll[Byte], Coll[Byte])) => xor(x._1, x._2) }"))
        { x => Global.xor(x._1, x._2) }
      forAll(bytesGen, bytesGen) { (l, r) =>
        eq(Builder.DefaultCollBuilder.fromArray(l), Builder.DefaultCollBuilder.fromArray(r))
      }
    }
  }

  property("Coll methods equivalence") {
    val coll = ctx.OUTPUTS
    val eq = EqualityChecker(coll)
    eq({ (x: Coll[Box]) => x.filter({ (b: Box) => b.value > 1 }) })("{ (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }")
    eq({ (x: Coll[Box]) => x.flatMap({ (b: Box) => b.propositionBytes }) })("{ (x: Coll[Box]) => x.flatMap({(b: Box) => b.propositionBytes }) }")
    eq({ (x: Coll[Box]) => x.zip(x) })("{ (x: Coll[Box]) => x.zip(x) }")
    eq({ (x: Coll[Box]) => x.size })("{ (x: Coll[Box]) => x.size }")
    eq({ (x: Coll[Box]) => x.indices })("{ (x: Coll[Box]) => x.indices }")
    eq({ (x: Coll[Box]) => x.forall({ (b: Box) => b.value > 1 }) })("{ (x: Coll[Box]) => x.forall({(b: Box) => b.value > 1 }) }")
    eq({ (x: Coll[Box]) => x.exists({ (b: Box) => b.value > 1 }) })("{ (x: Coll[Box]) => x.exists({(b: Box) => b.value > 1 }) }")
  }

  property("Coll size method equivalnce") {
    val eq = checkEq(func[Coll[Int],Int]("{ (x: Coll[Int]) => x.size }")){ x =>
      x.size
    }
    forAll { x: Array[Int] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  property("Coll patch method equivalnce") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]]("{ (x: (Coll[Int], (Int, Int))) => x._1.patch(x._2._1, x._1, x._2._2) }")){ x =>
      x._1.patch(x._2._1, x._1, x._2._2)
    }
    forAll { x: Array[Int] =>
      whenever (x.size > 1) {
        eq(Builder.DefaultCollBuilder.fromArray(x), makeSlicePair(x.size))
      }
    }
  }

  property("Coll updated method equivalnce") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]]("{ (x: (Coll[Int], (Int, Int))) => x._1.updated(x._2._1, x._2._2) }")){ x =>
      x._1.updated(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], Int) =>
      val size = x._1.size
      whenever (size > 1) {
        val index = getRandomIndex(size)
        eq(Builder.DefaultCollBuilder.fromArray(x._1), (index, x._2))
      }
    }
  }

  property("Coll updateMany method equivalnce") {
    val eq = checkEq(func[(Coll[Int], (Coll[Int], Coll[Int])),Coll[Int]]("{ (x: (Coll[Int], (Coll[Int], Coll[Int]))) => x._1.updateMany(x._2._1, x._2._2) }")){ x =>
      x._1.updateMany(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], Int) =>
      val size = x._1.size
      whenever (size > 1) {
        val fromColl = Builder.DefaultCollBuilder.fromArray(x._1)
        val indexColl = generateIndexColl(size)
        eq(fromColl, (indexColl, fromColl.reverse.slice(0, indexColl.size)))
      }
    }
  }

  // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ignore("Coll find method equivalnce") {
    val eq = checkEq(func[Coll[Int],Option[Int]]("{ (x: Coll[Int]) => x.find({(v: Int) => v > 0})}")){ x =>
      x.find(v => v > 0)
    }
    forAll { x: Array[Int] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  // https://github.com/ScorexFoundation/sigmastate-interpreter/issues/418
  ignore("Coll bitwise methods equivalnce") {
    val eq = checkEq(func[Coll[Boolean],Coll[Boolean]]("{ (x: Coll[Boolean]) => x >> 2 }")){ x =>
      if (x.size > 2) x.slice(0, x.size - 2) else Colls.emptyColl
    }
    forAll { x: Array[Boolean] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ignore("Coll diff methods equivalnce") {
    val eq = checkEq(func[Coll[Int],Coll[Int]]("{ (x: Coll[Int]) => x.diff(x) }")){ x =>
      x.diff(x)
    }
    forAll { x: Array[Int] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  property("Coll fold method equivalnce") {
    val monoid = Builder.DefaultCollBuilder.Monoids.intPlusMonoid
    val eq = checkEq(func[(Coll[Int], Int),Int]("{ (x: (Coll[Int], Int)) => x._1.fold(x._2, { (i1: Int, i2: Int) => i1 + i2 }) }"))
    { x =>
      x._1.sum(monoid) + x._2
    }
    val eqIndexOf = checkEq(func[(Coll[Int], Int),Int]("{ (x: (Coll[Int], Int)) => x._1.indexOf(x._2, 0) }"))
    { x =>
      x._1.indexOf(x._2, 0)
    }
    forAll { x: (Array[Int], Int) =>
      eq(Builder.DefaultCollBuilder.fromArray(x._1), x._2)
      eqIndexOf(Builder.DefaultCollBuilder.fromArray(x._1), x._2)
    }
  }

  property("Coll indexOf method equivalnce") {
    val eqIndexOf = checkEq(func[(Coll[Int], (Int, Int)),Int]("{ (x: (Coll[Int], (Int, Int))) => x._1.indexOf(x._2._1, x._2._2) }"))
    { x =>
      x._1.indexOf(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], Int) =>
      eqIndexOf(Builder.DefaultCollBuilder.fromArray(x._1), (getRandomIndex(x._1.size), x._2))
    }
  }

  property("Coll apply method equivalnce") {
    val eqApply = checkEq(func[(Coll[Int], Int),Int]("{ (x: (Coll[Int], Int)) => x._1(x._2) }"))
    { x =>
      x._1(x._2)
    }
    forAll { x: Array[Int] =>
      whenever (0 < x.size) {
        eqApply(Builder.DefaultCollBuilder.fromArray(x), getRandomIndex(x.size))
      }
    }
  }

  property("Coll getOrElse method equivalnce") {
    val eqGetOrElse = checkEq(func[(Coll[Int], (Int, Int)),Int]("{ (x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2) }"))
    { x =>
      x._1.getOrElse(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], (Int, Int)) =>
      eqGetOrElse(Builder.DefaultCollBuilder.fromArray(x._1), x._2)
    }
  }

  property("Tuple filter method equivalnce") {
    val eq = checkEq(func[(Int, Int),Int]("{ (x: (Int, Int)) => x.size }")) { x => 2 }
    eq((-1, 1))
  }

  property("Coll map method equivalnce") {
    val eq = checkEq(func[Coll[Int],Coll[Int]]("{ (x: Coll[Int]) => x.map({ (v: Int) => v + 1 }) }"))
    { x =>
      x.map(v => v + 1)
    }
    forAll { x: Array[Int] =>
      eq(Builder.DefaultCollBuilder.fromArray(x))
    }
  }

  property("Coll slice method equivalnce") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]]("{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }"))
    { x =>
      x._1.slice(x._2._1, x._2._2)
    }
    forAll { x: Array[Int] =>
      val size = x.size
      whenever (size > 0) {
        eq(Builder.DefaultCollBuilder.fromArray(x), makeSlicePair(size))
      }
    }
    val arr = Array[Int](1, 2, 3, 4, 5)
    eq(Builder.DefaultCollBuilder.fromArray(arr), (0, 2))
  }

  /*
  line 5: sliced.append(toAppend)
               ^
  Don't know how to evalNode(Select(Ident(sliced,Coll[SInt$]),append,Some((Coll[SInt$]) => Coll[SInt$])))
  sigmastate.lang.exceptions.CosterException:
  line 5: sliced.append(toAppend)
   */
  ignore("Coll append method equivalnce") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]](
      """{ (x: (Coll[Int], (Int, Int))) =>
        |val sliced: Coll[Int] = x._1.slice(x._2._1, x._2._2)
        |val toAppend: Coll[Int] = x._1
        |sliced.append(toAppend)
        |}""".stripMargin))
    { x =>
      val sliced: Coll[Int] = x._1.slice(x._2._1, x._2._2)
      val toAppend: Coll[Int] = x._1
      sliced.append(toAppend)
    }
    forAll { x: Array[Int] =>
      val size = x.size
      whenever (size > 0) {
        eq(Builder.DefaultCollBuilder.fromArray(x), makeSlicePair(size))
      }
    }
  }

  property("Option methods equivalence") {
    val opt: Option[Long] = ctx.dataInputs(0).R0[Long]
    val eq = EqualityChecker(opt)
    eq({ (x: Option[Long]) => x.get })("{ (x: Option[Long]) => x.get }")
    // TODO implement Option.isEmpty
    //  eq({ (x: Option[Long]) => x.isEmpty })("{ (x: Option[Long]) => x.isEmpty }")
    eq({ (x: Option[Long]) => x.isDefined })("{ (x: Option[Long]) => x.isDefined }")
    eq({ (x: Option[Long]) => x.getOrElse(1L) })("{ (x: Option[Long]) => x.getOrElse(1L) }")
    eq({ (x: Option[Long]) => x.filter({ (v: Long) => v == 1} ) })("{ (x: Option[Long]) => x.filter({ (v: Long) => v == 1 }) }")
    eq({ (x: Option[Long]) => x.map( (v: Long) => v + 1 ) })("{ (x: Option[Long]) => x.map({ (v: Long) => v + 1 }) }")
  }

  // TODO implement Option.fold
  ignore("Option fold method") {
    val opt: Option[Long] = ctx.dataInputs(0).R0[Long]
    val eq = EqualityChecker(opt)
    eq({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => v + 1 ) })("{ (x: Option[Long]) => x.fold(5, { (v: Long) => v + 1 }) }")
  }

  property("Option fold workaround method") {
    val opt: Option[Long] = ctx.dataInputs(0).R0[Long]
    val eq = EqualityChecker(opt)
    eq({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => v + 1 ) })(
      """{(x: Option[Long]) =>
        |  def f(opt: Long): Long = opt + 1
        |  if (x.isDefined) f(x.get) else 5L
        |}""".stripMargin)
  }
}
