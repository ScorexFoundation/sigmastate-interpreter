package sigmastate.utxo

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform._
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, TestContractSpec, StdContracts}
import scalan.RType
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.{IRContext, CAvlTree, CostingSigmaDslBuilder, CSigmaProp}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.lang.Terms._
import sigmastate.serialization.OperationSerializer
import special.collection.Coll
import special.sigma.{Context, Box, AvlTree}



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

  property("avl tree - simple modification (ErgoDsl)") {
    case class AvlTreeContract[Spec <: ContractSpec]
        (ops: Coll[Byte], proof: Coll[Byte], prover: Spec#ProvingParty)
        (implicit val spec: Spec) extends SigmaContractSyntax
    {
      def pkProver = prover.pubKey
      import syntax._
      lazy val contractEnv = Env("pkProver" -> pkProver, "ops" -> ops, "proof" -> proof)

      lazy val treeProp = proposition("treeProp", { ctx: Context => import ctx._
        sigmaProp(treeModifications(SELF.R4[AvlTree].get, ops, proof).get == SELF.R5[AvlTree].get)
      },
      """{
       |  sigmaProp(treeModifications(SELF.R4[AvlTree].get, ops, proof).get == SELF.R5[AvlTree].get)
       |}
      """.stripMargin)

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val (tree, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, (inKey -> inValue))

    val operations: Seq[Operation] =
      (0 to 10).map(i => Insert(genKey(i.toString), genValue(i.toString))) :+
         Update(inKey, genValue("updated value"))

    operations.foreach(o => avlProver.performOneOperation(o))
    val opsBytes = serializeOperations(avlProver, operations)
    val proof = avlProver.generateProof().toColl
    val endDigest = avlProver.digest.toColl
    val endTree = tree.updateDigest(endDigest)

    val contract = AvlTreeContract[spec.type](opsBytes, proof, prover)(spec)
    import contract.spec._

    val mockTx = block(0).newTransaction()
    val s = mockTx
      .outBox(20, contract.treeProp)
      .withRegs(reg1 -> tree, reg2 -> endTree)

    val spendingTx = block(50).newTransaction().spending(s)
    val newBox1 = spendingTx.outBox(10, contract.proverSig)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

//    val pr = prover.prove(in1).get
//    contract.verifier.verify(in1, pr) shouldBe true
  }

  property("avl tree - composite modifications") {
    case class AvlTreeContract[Spec <: ContractSpec]
        (ops0: Coll[Byte], proof0: Coll[Byte], ops1: Coll[Byte], proof1: Coll[Byte],
         prover: Spec#ProvingParty)
        (implicit val spec: Spec) extends SigmaContractSyntax
    {
      def pkProver = prover.pubKey
      import syntax._
      lazy val contractEnv = Env("pkProver" -> pkProver, "ops0" -> ops0, "proof0" -> proof0, "ops1" -> ops1, "proof1" -> proof1)

      lazy val treeProp = proposition("treeProp", { ctx: Context => import ctx._
        val tree0 = SELF.R4[AvlTree].get
        val endTree = SELF.R5[AvlTree].get
        val tree1 = tree0.modify(ops0, proof0).get
        sigmaProp(tree1.modify(ops1, proof1).get == endTree)
      },
      """{
       |  val tree0 = SELF.R4[AvlTree].get
       |  val endTree = SELF.R5[AvlTree].get
       |  val tree1 = treeModifications(tree0, ops0, proof0).get
       |  sigmaProp(treeModifications(tree1, ops1, proof1).get == endTree)
       |}
      """.stripMargin)

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val (tree, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, (inKey -> inValue))

    val operations0: Seq[Operation] = (0 to 10).map(i => Insert(genKey(i.toString), genValue(i.toString))) :+
      Update(inKey, genValue(s"updated value - 0"))
    val operations1: Seq[Operation] = (0 to 10).map(i => Remove(genKey(i.toString))) :+
      Update(inKey, genValue(s"updated value - 1"))

    val opsBytes0 = serializeOperations(avlProver, operations0)
    val opsBytes1 = serializeOperations(avlProver, operations1)

    operations0.foreach(o => avlProver.performOneOperation(o))
    val proof0 = avlProver.generateProof().toColl

    operations1.foreach(o => avlProver.performOneOperation(o))
    val proof1 = avlProver.generateProof().toColl

    val endDigest = avlProver.digest.toColl
    val endTree = tree.updateDigest(endDigest)

    val contract = AvlTreeContract[spec.type](opsBytes0, proof0, opsBytes1, proof1, prover)(spec)
    import contract.spec._

    val mockTx = block(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> tree, reg2 -> endTree)

    val spendingTx = block(50).newTransaction().spending(s)
    val newBox1 = spendingTx.outBox(10, contract.proverSig)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    //    val pr = prover.prove(in1).get
    //    contract.verifier.verify(in1, pr) shouldBe true
  }

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
       |  sigmaProp(treeRemovals(SELF.R4[AvlTree].get, ops, proof).get == SELF.R5[AvlTree].get)
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

    val mockTx = block(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> tree, reg2 -> endTree)

    val spendingTx = block(50).newTransaction().spending(s)
    val newBox1 = spendingTx.outBox(10, contract.proverSig)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    //    val pr = prover.prove(in1).get
    //    contract.verifier.verify(in1, pr) shouldBe true
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
      """{ sigmaProp(treeInserts(SELF.R4[AvlTree].get, ops, proof).get == SELF.R5[AvlTree].get) }""")

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

    val mockTx = block(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> tree, reg2 -> endTree)

    val spendingTx = block(50).newTransaction().spending(s)
    val newBox1 = spendingTx.outBox(10, contract.proverSig)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    //    val pr = prover.prove(in1).get
    //    contract.verifier.verify(in1, pr) shouldBe true
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
      """{ sigmaProp(treeLookup(SELF.R4[AvlTree].get, key, proof).get == value) }""")

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val key = genKey("key")
    val value = genValue("value")
    val (tree, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, key -> value, genKey("key2") -> genValue("value2"))
    avlProver.performOneOperation(Lookup(genKey("key")))

    val digest = avlProver.digest
    val proof = avlProver.generateProof().toColl
    val treeData = new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None)

    val contract = AvlTreeContract[spec.type](key.toColl, proof, value.toColl, prover)(spec)
    import contract.spec._

    val mockTx = block(0).newTransaction()
    val s = mockTx
        .outBox(20, contract.treeProp)
        .withRegs(reg1 -> treeData)

    val spendingTx = block(50).newTransaction().spending(s)
    val newBox1 = spendingTx.outBox(10, contract.proverSig)

    val in1 = spendingTx.inputs(0)
    val res = in1.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)
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

    val treeData = new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None)

    val env = Map("key" -> key, "proof" -> proof)
    val prop = compileWithCosting(env, """isMember(SELF.R4[AvlTree].get, key, proof)""").asBoolValue.toSigmaProp

    val propTree = OptionIsDefined(TreeLookup(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ByteArrayConstant(key),
      ByteArrayConstant(proof))).toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

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
    val treeData = new AvlTreeData(avlProver.digest, AvlTreeFlags.ReadOnly, 32, None)
    val proofId = 0: Byte
    val elementId = 1: Byte

    val prop = AND(
      GE(GetVarLong(elementId).get, LongConstant(120)),
      OptionIsDefined(TreeLookup(ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
        CalcBlake2b256(LongToByteArray(GetVarLong(elementId).get)),
        GetVarByteArray(proofId).get))
    ).toSigmaProp
    val env = Map("proofId" -> proofId.toLong, "elementId" -> elementId.toLong)
    val propCompiled = compileWithCosting(env,
      """{
        |  val tree = SELF.R3[AvlTree].get
        |  val proof = getVar[Coll[Byte]](proofId).get
        |  val element = getVar[Long](elementId).get
        |  val elementKey = blake2b256(longToByteArray(element))
        |  element >= 120 && isMember(tree, elementKey, proof)
        |}""".stripMargin).asBoolValue.toSigmaProp

    //TODO: propCompiled shouldBe prop

    val recipientProposition = new ErgoLikeTestProvingInterpreter().dlogSecrets.head.publicImage
    val selfBox = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))
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

    val treeData = new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None)

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
        |}""".stripMargin).asBoolValue.toSigmaProp

    val propTree = OptionIsDefined(TreeLookup(
      ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
      ExtractRegisterAs[SByteArray](Self, reg2).get,
      GetVarByteArray(proofId).get)).toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

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
}