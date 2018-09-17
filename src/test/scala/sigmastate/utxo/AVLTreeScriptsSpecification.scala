package sigmastate.utxo

import com.google.common.primitives.Longs
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate._
import sigmastate.Values._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.lang.Terms._
import sigmastate.utxo.GetVar._

class AVLTreeScriptsSpecification extends SigmaTestingCommons {

  private val reg1 = ErgoBox.nonMandatoryRegisters.head
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)

  property("avl tree - simplest case") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val key = Blake2b256("hello world")
    avlProver.performOneOperation(Insert(ADKey @@ key, ADValue @@ key))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(ADKey @@ key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)

    val env = Map("key" -> key, "proof" -> proof)
    val prop = compile(env, """isMember(SELF.R4[AvlTree].get, key, proof)""").asBoolValue

    val propTree = IsMember(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ByteArrayConstant(key),
      ByteArrayConstant(proof))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
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
      IsMember(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
        CalcBlake2b256(LongToByteArray(GetVarLong(elementId).get)),
        GetVarByteArray(proofId).get)
    )
    val env = Map("proofId" -> proofId.toLong, "elementId" -> elementId.toLong)
    val propCompiled = compile(env,
      """{
        |  val tree = SELF.R3[AvlTree].get
        |  val proof = getVar[Array[Byte]](proofId).get
        |  val element = getVar[Long](elementId).get
        |  val elementKey = blake2b256(longToByteArray(element))
        |  element >= 120 && isMember(tree, elementKey, proof)
        |}""".stripMargin).asBoolValue

    // TODO propCompiled shouldBe prop

    val recipientProposition = new ErgoLikeProvingInterpreter().dlogSecrets.head.publicImage
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition))),
      self = ErgoBox(20, TrueLeaf, Seq(), Map(reg1 -> AvlTreeConstant(treeData))))

    avlProver.performOneOperation(Lookup(treeElements.head._1))
    val bigLeafProof = avlProver.generateProof()
    val prover = new ErgoLikeProvingInterpreter()
      .withContextExtender(proofId, ByteArrayConstant(bigLeafProof))
      .withContextExtender(elementId, LongConstant(elements.head))
    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true

    avlProver.performOneOperation(Lookup(treeElements.last._1))
    val smallLeafTreeProof = avlProver.generateProof()
    val smallProver = new ErgoLikeProvingInterpreter()
      .withContextExtender(proofId, ByteArrayConstant(smallLeafTreeProof))
      .withContextExtender(elementId, LongConstant(elements.head))
    smallProver.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
    // TODO check that verifier return false for incorrect proofs?
  }

  property("avl tree - prover provides proof") {

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val key = Blake2b256("hello world")
    avlProver.performOneOperation(Insert(ADKey @@ key, ADValue @@ key))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(ADKey @@ key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)

    val proofId = 31: Byte

    val prover = new ErgoLikeProvingInterpreter().withContextExtender(proofId, ByteArrayConstant(proof))
    val verifier = new ErgoLikeInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("proofId" -> proofId.toLong)
    val prop = compile(env,
      """{
        |  val tree = SELF.R4[AvlTree].get
        |  val key = SELF.R5[Array[Byte]].get
        |  val proof = getVar[Array[Byte]](proofId).get
        |  isMember(tree, key, proof)
        |}""".stripMargin).asBoolValue

    val propTree = IsMember(
      ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ExtractRegisterAs[SByteArray](Self, reg2).get,
      GetVarByteArray(proofId).get)
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Seq(), Map(reg1 -> AvlTreeConstant(treeData), reg2 -> ByteArrayConstant(key)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction, self = s)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)
    verifier.verify(prop, ctxv, pr, fakeMessage).get._1 shouldBe true
  }
}