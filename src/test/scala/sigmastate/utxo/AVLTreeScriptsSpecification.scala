package sigmastate.utxo

import com.google.common.primitives.Longs
import org.ergoplatform._
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.lang.Terms._
import sigmastate.serialization.OperationSerializer

class AVLTreeScriptsSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)

  def genKey(str: String): ADKey = ADKey @@ Blake2b256("key: " + str)

  def genValue(str: String): ADValue = ADValue @@ Blake2b256("val: " + str)

  property("avl tree modification") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val inKey = genKey("init key")
    avlProver.performOneOperation(Insert(inKey, genValue("init value")))
    avlProver.generateProof()
    val digest = avlProver.digest
    val treeData = new AvlTreeData(digest, 32, None)

    val operations: Seq[Operation] = (0 to 10).map(i => Insert(genKey(i.toString), genValue(i.toString))) :+
      Update(inKey, genValue("updated value"))
    val serializer = new OperationSerializer(avlProver.keyLength, avlProver.valueLengthOpt)
    val opsBytes: Array[Byte] = serializer.serializeSeq(operations)
    operations.foreach(o => avlProver.performOneOperation(o))
    val proof = avlProver.generateProof()
    val endDigest = avlProver.digest

    val prop = EQ(TreeModifications(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ByteArrayConstant(opsBytes),
      ByteArrayConstant(proof)).get, ByteArrayConstant(endDigest))
    val env = Map("ops" -> opsBytes, "proof" -> proof, "endDigest" -> endDigest)
    val propCompiled = compileWithCosting(env, """treeModifications(SELF.R4[AvlTree].get, ops, proof).get == endDigest""").asBoolValue
    prop shouldBe propCompiled

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("avl tree lookup") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val key = genKey("key")
    val value = genValue("value")
    avlProver.performOneOperation(Insert(key, value))
    avlProver.performOneOperation(Insert(genKey("key2"), genValue("value2")))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(genKey("key")))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)


    val prop = EQ(TreeLookup(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ByteArrayConstant(key),
      ByteArrayConstant(proof)).get, ByteArrayConstant(value))

    val env = Map("key" -> key, "proof" -> proof, "value" -> value)
    val propCompiled = compileWithCosting(env, """treeLookup(SELF.R4[AvlTree].get, key, proof).get == value""").asBoolValue
    prop shouldBe propCompiled

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("avl tree - simplest case") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val key = genKey("hello world")
    avlProver.performOneOperation(Insert(key, genValue("val")))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)

    val env = Map("key" -> key, "proof" -> proof)
    val prop = compileWithCosting(env, """isMember(SELF.R4[AvlTree].get, key, proof)""").asBoolValue

    val propTree = OptionIsDefined(TreeLookup(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ByteArrayConstant(key),
      ByteArrayConstant(proof)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("avl tree - leaf satisfying condition exists") {
    val elements = Seq(123, 22)
    val treeElements = elements.map(i => Longs.toByteArray(i)).map(s => (ADKey @@ Blake2b256(s), ADValue @@ s))
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    treeElements.foreach(s => avlProver.performOneOperation(Insert(s._1, s._2)))
    avlProver.generateProof()
    val treeData = new AvlTreeData(avlProver.digest, 32, None)
    val proofId = 0: Byte
    val elementId = 1: Byte

    val prop: Value[SBoolean.type] = AND(
      GE(GetVarLong(elementId).get, LongConstant(120)),
      OptionIsDefined(TreeLookup(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
        CalcBlake2b256(LongToByteArray(GetVarLong(elementId).get)),
        GetVarByteArray(proofId).get))
    )
    val env = Map("proofId" -> proofId.toLong, "elementId" -> elementId.toLong)
    val propCompiled = compileWithCosting(env,
      """{
        |  val tree = SELF.R3[AvlTree].get
        |  val proof = getVar[Coll[Byte]](proofId).get
        |  val element = getVar[Long](elementId).get
        |  val elementKey = blake2b256(longToByteArray(element))
        |  element >= 120 && isMember(tree, elementKey, proof)
        |}""".stripMargin).asBoolValue

    // TODO propCompiled shouldBe prop

    val recipientProposition = new ErgoLikeTestProvingInterpreter().dlogSecrets.head.publicImage
    val selfBox = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition, 0))),
      self = selfBox)

    avlProver.performOneOperation(Lookup(treeElements.head._1))
    val bigLeafProof = avlProver.generateProof()
    val prover = new ErgoLikeTestProvingInterpreter()
      .withContextExtender(proofId, ByteArrayConstant(bigLeafProof))
      .withContextExtender(elementId, LongConstant(elements.head))
    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true

    avlProver.performOneOperation(Lookup(treeElements.last._1))
    val smallLeafTreeProof = avlProver.generateProof()
    val smallProver = new ErgoLikeTestProvingInterpreter()
      .withContextExtender(proofId, ByteArrayConstant(smallLeafTreeProof))
      .withContextExtender(elementId, LongConstant(elements.head))
    smallProver.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
    // TODO check that verifier return false for incorrect proofs?
  }

  property("avl tree - prover provides proof") {

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val key = genKey("hello world")
    avlProver.performOneOperation(Insert(key, genValue("val")))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)

    val proofId = 31: Byte

    val prover = new ErgoLikeTestProvingInterpreter().withContextExtender(proofId, ByteArrayConstant(proof))
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("proofId" -> proofId.toLong)
    val prop = compileWithCosting(env,
      """{
        |  val tree = SELF.R4[AvlTree].get
        |  val key = SELF.R5[Coll[Byte]].get
        |  val proof = getVar[Coll[Byte]](proofId).get
        |  isMember(tree, key, proof)
        |}""".stripMargin).asBoolValue

    val propTree = OptionIsDefined(TreeLookup(
      ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ExtractRegisterAs[SByteArray](Self, reg2).get,
      GetVarByteArray(proofId).get))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData), reg2 -> ByteArrayConstant(key)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction, self = s)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)
    verifier.verify(prop, ctxv, pr, fakeMessage).get._1 shouldBe true
  }
}