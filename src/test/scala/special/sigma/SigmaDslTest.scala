package special.sigma

import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec}
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import org.scalacheck.Gen.containerOfN
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scalan.RType
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.util.Extensions._
import sigmastate.Values.{BooleanConstant, IntConstant}
import sigmastate._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.ScriptEnv
import special.collection.{Builder, Coll}


/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslTest extends PropSpec
  with PropertyChecks
  with Matchers
  with SigmaTestingCommons with SigmaContractSyntax
  with SigmaTypeGens { suite =>

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  override def contractEnv: ScriptEnv = Map()

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

  case class EqualityChecker[T: RType](obj: T) {
    def apply[R: RType](dslFunc: T => R)(script: String) =
      checkEq(func[T, R](script))(dslFunc)(obj)
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

    forAll { x: Int =>
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

//  property("sigma.types.Byte methods equivalence") {
//    import sigma.types._
//    val toInt = checkEq(func[Byte,Int]("{ (x: Byte) => x.toInt }"))(x => x.toInt)
//    forAll { x: Byte =>
//      Seq(toInt).foreach(_(x))
//    }
//  }
//
//  property("sigma.types.Int methods equivalence") {
//    import sigma.types._
//    val toByte = checkEq(func[Int,Byte]("{ (x: Int) => x.toByte }"))(x => x.toByte)
//    lazy val compareTo = checkEq(func[(Int, Int), Int]("{ (x: (Int, Int)) => x._1.compareTo(x._2) }"))(x => x._1.compareTo(x._2))
//    forAll { in: scala.Int =>
//      whenever(scala.Byte.MinValue <= in && in <= scala.Byte.MaxValue) {
//        val x = CInt(in)
//        toByte(x)
//      }
//    }
//  }

  val bytesGen: Gen[Array[Byte]] = containerOfN[Array, Byte](100, Arbitrary.arbByte.arbitrary)
  val bytesCollGen = bytesGen.map(Colls.fromArray(_))
  implicit val arbBytes = Arbitrary(bytesCollGen)
  val keyCollGen = bytesCollGen.map(_.slice(0, 32))
  import org.ergoplatform.dsl.AvlTreeHelpers._

  private def sampleAvlProver = {
    val key = keyCollGen.sample.get
    val value = bytesCollGen.sample.get
    val (_, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, ADKey @@ key.toArray -> ADValue @@ value.toArray)
    (key, value, avlProver)
  }

  private def sampleAvlTree:CAvlTree = {
    val (key, _, avlProver) = sampleAvlProver
    val digest = avlProver.digest.toColl
    val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    tree
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

    val tree = sampleAvlTree

    doDigest(tree)
    doEnabledOps(tree)
    doKeyLength(tree)
    doValueLength(tree)
    insertAllowed(tree)
    updateAllowed(tree)
    removeAllowed(tree)
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

  // TODO: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/427
  // TODO costing: expression t._1(t._2) cannot be costed because t is lambda argument
  ignore("Func context variable") {
//    val doApply = checkEq(func[(Int => Int, Int), Int]("{ (t: (Int => Int, Int)) => t._1(t._2) }")) { (t: (Int => Int, Int)) => t._1(t._2) }
//    val code = compileWithCosting(emptyEnv, s"{ (x: Int) => x + 1 }")
//    val ctx = ErgoLikeContext.dummy(fakeSelf)
//    doApply((CFunc[Int, Int](ctx, code), 10))
  }

  val tokenId1: Digest32 = Blake2b256("id1")
  val tokenId2: Digest32 = Blake2b256("id2")
  val inBox = createBox(10, TrivialProp.TrueProp,
    Seq(tokenId1 -> 10L, tokenId2 -> 20L),
    Map(ErgoBox.R4 -> IntConstant(100), ErgoBox.R5 -> BooleanConstant(true)))

  val dataBox = createBox(1000, TrivialProp.TrueProp,
    Seq(tokenId1 -> 10L, tokenId2 -> 20L),
    Map(ErgoBox.R4 -> IntConstant(100), ErgoBox.R5 -> BooleanConstant(true)))

  val outBox = createBox(10, TrivialProp.TrueProp,
    Seq(tokenId1 -> 10L, tokenId2 -> 20L),
    Map(ErgoBox.R4 -> IntConstant(100), ErgoBox.R5 -> BooleanConstant(true)))

  val header1: Header = CHeader(Blake2b256("Header.id").toColl,
    0,
    Blake2b256("Header.parentId").toColl,
    Blake2b256("ADProofsRoot").toColl,
    sampleAvlTree,
    Blake2b256("transactionsRoot").toColl,
    timestamp = 0,
    nBits = 0,
    height = 0,
    extensionRoot = Blake2b256("transactionsRoot").toColl,
    minerPk = SigmaDsl.groupGenerator,
    powOnetimePk = SigmaDsl.groupGenerator,
    powNonce = Colls.fromArray(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)),
    powDistance = SigmaDsl.BigInt(BigInt("1405498250268750867257727119510201256371618473728619086008183115260323").bigInteger),
    votes = Colls.fromArray(Array[Byte](0, 1, 2))
  )
  val header2: Header = CHeader(Blake2b256("Header2.id").toColl,
    0,
    header1.id,
    Blake2b256("ADProofsRoot2").toColl,
    sampleAvlTree,
    Blake2b256("transactionsRoot2").toColl,
    timestamp = 2,
    nBits = 0,
    height = 1,
    extensionRoot = Blake2b256("transactionsRoot2").toColl,
    minerPk = SigmaDsl.groupGenerator,
    powOnetimePk = SigmaDsl.groupGenerator,
    powNonce = Colls.fromArray(Array.fill(0.toByte)(8)),
    powDistance = SigmaDsl.BigInt(BigInt("19306206489815517413186395405558417825367537880571815686937307203793939").bigInteger),
    votes =  Colls.fromArray(Array[Byte](0, 1, 0))
    )
  val headers = Colls.fromItems(header2, header1)
  val preHeader: PreHeader = CPreHeader(0,
    header2.id,
    timestamp = 3,
    nBits = 0,
    height = 2,
    minerPk = SigmaDsl.groupGenerator,
    votes = Colls.emptyColl[Byte]
  )
  val ergoCtx = new ErgoLikeContext(
    currentHeight = preHeader.height,
    lastBlockUtxoRoot = header2.stateRoot.asInstanceOf[CAvlTree].treeData,
    preHeader.minerPk.getEncoded.toArray,
    boxesToSpend = IndexedSeq(inBox),
    spendingTransaction = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(outBox)),
    self = inBox, headers = headers, preHeader = preHeader, dataBoxes = IndexedSeq(dataBox),
    extension = ContextExtension(Map()))
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
// TODO
//    checkEq(func[Box, Coll[(Coll[Byte], Long)]]("{ (x: Box) => x.registers }"))({ (x: Box) => x.registers })(box)
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
// TODO costing for  eq({ (x: Header) => x.id })("{ (x: Header) => x.id }")
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

// TODO
//    checkEq(func[Context, Coll[Box]]("{ (x: Context) => INPUTS }"))({ (x: Context) => x.INPUTS })(ctx)
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
    val negByte = checkEq(func[Byte, Byte]("{ (x: Byte) => -x }")) { x => (-x).toByte }
    forAll { x: Byte => negByte(x) }
    val negShort = checkEq(func[Short, Short]("{ (x: Short) => -x }")) { x => (-x).toShort }
    forAll { x: Short => negShort(x) }
    val negInt = checkEq(func[Int, Int]("{ (x: Int) => -x }")) { x => -x }
    forAll { x: Int => negInt(x) }
    val negLong = checkEq(func[Long, Long]("{ (x: Long) => -x }")) { x => -x }
    forAll { x: Long => negLong(x) }
  }

  property("special.sigma.BigInt Negation equivalence") {
    // TODO make negate() into a prefix method
    val negBigInteger = checkEq(func[BigInt, BigInt]("{ (x: BigInt) => -x }")) { x => x.negate() }
    forAll { x: BigInt => negBigInteger(x) }
  }

  //TODO: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/424
  ignore("BinXor(logical XOR) equivalence") {
    val eq = checkEq(func[(Boolean, Boolean), Boolean]("{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }")) {
      x => x._1 ^ x._2
    }
    forAll { x: (Boolean, Boolean) => eq(x) }
  }

  // TODO: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
  ignore("Box.getReg equivalence") {
    // TODO implement in SigmaDsl (interpreter test passes in BasicOpsSpec.Box.getReg test)
//    val eq = checkEq(func[Box, Int]("{ (x: Box) => x.getReg[Int](1).get }")) { x => x.getReg(1).get }
//    forAll { x: Box => eq(x) }
  }
}
