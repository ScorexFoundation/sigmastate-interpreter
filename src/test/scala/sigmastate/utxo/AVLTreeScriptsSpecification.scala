package sigmastate.utxo

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform._
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, TestContractSpec}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.{IRContext, CSigmaProp}
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._
import special.collection.Coll
import special.sigma.{Context, AvlTree}


class AVLTreeScriptsSpecification extends SigmaTestingCommons { suite =>
  import org.ergoplatform.dsl.AvlTreeHelpers._
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  lazy val prover = spec.ProvingParty("Alice")
  private implicit lazy val IR: IRContext = spec.IR

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)

  def genKey(str: String): ADKey = ADKey @@ Blake2b256("key: " + str)
  def genValue(str: String): ADValue = ADValue @@ Blake2b256("val: " + str)

  val inKey = genKey("init key")
  val inValue = genValue("init value")

  property("avl tree - removals") {
    case class AvlTreeContract[Spec <: ContractSpec]
      (ops: Coll[Coll[Byte]], proof: Coll[Byte], prover: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax
    {
      def pkProver = prover.pubKey
      import syntax._
      lazy val contractEnv = Env("pkProver" -> pkProver, "ops" -> ops, "proof" -> proof)

      lazy val treeProp = proposition("treeProp", { ctx: Context => import ctx._
        sigmaProp(SELF.R4[AvlTree].get.remove(ops, proof).get == SELF.R5[AvlTree].get)
      },
      """{
       |  sigmaProp(SELF.R4[AvlTree].get.remove(ops, proof).get == SELF.R5[AvlTree].get)
       |}
      """.stripMargin)

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val entries = (0 to 10).map { i => (genKey(i.toString) -> genValue(i.toString)) }
    val (tree, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, entries:_*)

    val removalKeys = (0 to 10).map(i => genKey(i.toString)).toArray
    val removals: Seq[Operation] = removalKeys.map(k => Remove(k))
    removals.foreach(o => avlProver.performOneOperation(o))

    val proof = avlProver.generateProof().toColl
    val endDigest = avlProver.digest.toColl
    val endTree = tree.updateDigest(endDigest)

    val contract = AvlTreeContract[spec.type](removalKeys.toColl, proof, prover)(spec)
    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> tree, reg2 -> endTree)

    val spendingTx = candidateBlock(50).newTransaction().spending(s)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val pr = prover.prove(in1).get
    contract.verifier.verify(in1, pr) shouldBe true
  }

  property("avl tree - inserts") {
    case class AvlTreeContract[Spec <: ContractSpec]
      (ops: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte], prover: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax
    {
      def pkProver = prover.pubKey
      import syntax._
      lazy val contractEnv = Env("pkProver" -> pkProver, "ops" -> ops, "proof" -> proof)

      lazy val treeProp = proposition("treeProp", { ctx: Context => import ctx._
        val tree = SELF.R4[AvlTree].get
        val endTree = SELF.R5[AvlTree].get
        sigmaProp(tree.insert(ops, proof).get == endTree)
      },
      """{
       |  val tree = SELF.R4[AvlTree].get
       |  val endTree = SELF.R5[AvlTree].get
       |  sigmaProp(tree.insert(ops, proof).get == endTree)
       |}""".stripMargin)

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val (tree, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed)
    val insertPairs = (0 to 10).map { i => (genKey(i.toString), genValue(i.toString)) }.toArray
    insertPairs.foreach { case (k, v) => avlProver.performOneOperation(Insert(k, v)) }

    val proof = avlProver.generateProof().toColl
    val endDigest = avlProver.digest.toColl
    val endTree = tree.updateDigest(endDigest)

    val contract = AvlTreeContract[spec.type](insertPairs.toColl, proof, prover)(spec)
    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> tree, reg2 -> endTree)

    val spendingTx = candidateBlock(50).newTransaction().spending(s)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val pr = prover.prove(in1).get
    contract.verifier.verify(in1, pr) shouldBe true
  }

  property("avl tree lookup") {
    case class AvlTreeContract[Spec <: ContractSpec]
      (key: Coll[Byte], proof: Coll[Byte], value: Coll[Byte], prover: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax
    {
      def pkProver = prover.pubKey
      import syntax._
      lazy val contractEnv = Env("pkProver" -> pkProver, "key" -> key, "proof" -> proof, "value" -> value)

      lazy val treeProp = proposition("treeProp", { ctx: Context => import ctx._
        val tree = SELF.R4[AvlTree].get
        sigmaProp(tree.get(key, proof).get == value)
      },
      """{
       |  val tree = SELF.R4[AvlTree].get
       |  sigmaProp(tree.get(key, proof).get == value)
       |}""".stripMargin)

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val key = genKey("key")
    val value = genValue("value")
    val (_, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, key -> value, genKey("key2") -> genValue("value2"))
    avlProver.performOneOperation(Lookup(genKey("key")))

    val digest = avlProver.digest
    val proof = avlProver.generateProof().toColl
    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))

    val contract = AvlTreeContract[spec.type](key.toColl, proof, value.toColl, prover)(spec)
    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> treeData)

    val spendingTx = candidateBlock(50).newTransaction().spending(s)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val pr = prover.prove(in1).get
    contract.verifier.verify(in1, pr) shouldBe true
  }

  property("avl tree - simplest case") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val key = genKey("hello world")
    avlProver.performOneOperation(Insert(key, genValue("val")))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None)

    val env = Map("key" -> key, "proof" -> proof)
    val prop = compile(env, """SELF.R4[AvlTree].get.contains(key, proof)""").asBoolValue.toSigmaProp

    val propTree = IR.builder.mkMethodCall(
      ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      SAvlTree.containsMethod,
      IndexedSeq(ByteArrayConstant(key), ByteArrayConstant(proof))
    ).asBoolValue.toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(SigmaDsl.avlTree(treeData))))

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

  property("avl tree - contains key satisfying condition") {
    val elements = Seq(123, 22)
    val treeElements = elements.map(i => Longs.toByteArray(i)).map(s => (ADKey @@ Blake2b256(s), ADValue @@ s))
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    treeElements.foreach(s => avlProver.performOneOperation(Insert(s._1, s._2)))
    avlProver.generateProof()
    val treeData = new AvlTreeData(avlProver.digest, AvlTreeFlags.ReadOnly, 32, None)
    val proofId = 0: Byte
    val elementId = 1: Byte

    val prop = AND(
      GE(GetVarLong(elementId).get, LongConstant(120)),
      IR.builder.mkMethodCall(
        ExtractRegisterAs[SAvlTree.type](Self, reg1).get, SAvlTree.containsMethod,
        IndexedSeq(CalcBlake2b256(LongToByteArray(GetVarLong(elementId).get)), GetVarByteArray(proofId).get)
      ).asBoolValue
    ).toSigmaProp
    val env = Map("proofId" -> proofId.toLong, "elementId" -> elementId.toLong)
    val propCompiled = compile(env,
      """{
        |  val tree = SELF.R3[AvlTree].get
        |  val proof = getVar[Coll[Byte]](proofId).get
        |  val element = getVar[Long](elementId).get
        |  val elementKey = blake2b256(longToByteArray(element))
        |  element >= 120 && tree.contains(elementKey, proof)
        |}""".stripMargin).asBoolValue.toSigmaProp

    //TODO: propCompiled shouldBe prop

    val recipientProposition = new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    val selfBox = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(SigmaDsl.avlTree(treeData))))
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      createTransaction(ErgoBox(1, recipientProposition, 0)),
      self = selfBox)

    avlProver.performOneOperation(Lookup(treeElements.head._1))
    val bigLeafProof = avlProver.generateProof()
    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(proofId, ByteArrayConstant(bigLeafProof))
      .withContextExtender(elementId, LongConstant(elements.head))
    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true

    avlProver.performOneOperation(Lookup(treeElements.last._1))
    val smallLeafTreeProof = avlProver.generateProof()
    val smallProver = new ContextEnrichingTestProvingInterpreter()
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

    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))

    val proofId = 31: Byte

    val prover = new ContextEnrichingTestProvingInterpreter().withContextExtender(proofId, ByteArrayConstant(proof))
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("proofId" -> proofId.toLong)
    val prop = compile(env,
      """{
        |  val tree = SELF.R4[AvlTree].get
        |  val key = SELF.R5[Coll[Byte]].get
        |  val proof = getVar[Coll[Byte]](proofId).get
        |  tree.contains(key, proof)
        |}""".stripMargin).asBoolValue.toSigmaProp

    val propTree = IR.builder.mkMethodCall(
      ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      SAvlTree.containsMethod,
      IndexedSeq(ExtractRegisterAs[SByteArray](Self, reg2).get, GetVarByteArray(proofId).get)
    ).asBoolValue.toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData), reg2 -> ByteArrayConstant(key)))

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

  property("avl tree - getMany") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    (1 to 10).foreach {i =>
      val key = genKey(s"$i")
      avlProver.performOneOperation(Insert(key, genValue(s"${i + 100}")))
    }
    avlProver.generateProof()

    (3 to 5).foreach { i =>
      val key = genKey(s"$i")
      avlProver.performOneOperation(Lookup(key))
    }
    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val proofId = 31: Byte

    val prover = new ContextEnrichingTestProvingInterpreter().withContextExtender(proofId, ByteArrayConstant(proof))
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))

    val env = Map("proofId" -> proofId.toLong,
                  "keys" -> ConcreteCollection(genKey("3"), genKey("4"), genKey("5")))
    val prop = compile(env,
      """{
        |  val tree = SELF.R4[AvlTree].get
        |  val proof = getVar[Coll[Byte]](proofId).get
        |  sigmaProp(tree.getMany(keys, proof).forall( { (o: Option[Coll[Byte]]) => o.isDefined }))
        |}""".stripMargin).asBoolValue.toSigmaProp

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction, self = s)
    val pr = prover.prove(env + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)
    verifier.verify(env + (ScriptNameProp -> "verify"), prop, ctxv, pr, fakeMessage).get._1 shouldBe true
  }

}
