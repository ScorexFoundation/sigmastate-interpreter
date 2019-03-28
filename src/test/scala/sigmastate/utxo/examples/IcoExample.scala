package sigmastate.utxo.examples

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.dsl.TestContractSpec
import org.ergoplatform._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values.{AvlTreeConstant, ByteArrayConstant, CollectionConstant, IntConstant}
import sigmastate._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._
import sigmastate.serialization.ErgoTreeSerializer
import ErgoTreeSerializer.DefaultSerializer
import sigmastate.eval.{CompiletimeCosting, IRContext}
import sigmastate.interpreter.CryptoConstants

import scala.util.Random

class IcoExample extends SigmaTestingCommons { suite =>

  // Not mixed with TestContext since it is not possible to call commpiler.compile outside tests if mixed
  implicit lazy val IR: IRContext = new IRContext with CompiletimeCosting

  lazy val spec = TestContractSpec(suite)
  lazy val backer = spec.ProvingParty("Alice")
  lazy val project = spec.ProvingParty("Bob")

  private val miningRewardsDelay = 720
  private val feeProp = ErgoScriptPredef.feeProposition(miningRewardsDelay)
  private val feeBytes = feeProp.bytes

  val env = Map(ScriptNameProp -> "withdrawalScriptEnv", "feeBytes" -> feeBytes)

  val withdrawalScript = compiler.compile(env,
    """{
      |  val removeProof = getVar[Coll[Byte]](2).get
      |  val lookupProof = getVar[Coll[Byte]](3).get
      |
      |  val outsCount = OUTPUTS.size
      |  val outs = OUTPUTS.slice(1, outsCount - 1)
      |
      |  val withdrawValues = outs.map({(b: Box) => b.value })
      |
      |  val toRemove = outs.map({(b: Box) => blake2b256(b.propositionBytes)})
      |
      |  val initialTree = SELF.R5[AvlTree].get
      |
      |  val removedValues = initialTree.getMany(toRemove, lookupProof).map({(o: Option[Coll[Byte]]) => byteArrayToLong(o.get)})
      |  val valuesCorrect = removedValues == withdrawValues
      |
      |  val modifiedTree = initialTree.remove(toRemove, removeProof).get
      |
      |  val expectedTree = OUTPUTS(0).R5[AvlTree].get
      |
      |  val properTreeModification = modifiedTree == expectedTree
      |
      |  val selfOutputCorrect = OUTPUTS(0).propositionBytes == SELF.propositionBytes
      |  val lastIndex = getVar[Int](4).get
      |  val feeOut = OUTPUTS(lastIndex)
      |  val feeOutputCorrect = (feeOut.value <= 1) && (feeOut.propositionBytes == feeBytes)
      |
      |  properTreeModification && valuesCorrect && selfOutputCorrect && feeOutputCorrect
      |}""".stripMargin
  ).asBoolValue.toSigmaProp

  val wsHash = Blake2b256(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(withdrawalScript))


  val fixingProp = compile(env.updated("nextStageScriptHash", wsHash),
    """{
      |  val openTree = SELF.R4[AvlTree].get
      |
      |  val closedTree = OUTPUTS(0).R4[AvlTree].get
      |
      |  val digestPreserved = openTree.digest == closedTree.digest
      |  val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
      |  val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
      |  val treeIsClosed = closedTree.enabledOperations == 4
      |
      |  val valuePreserved = SELF.value <= OUTPUTS(0).value
      |  val stateChanged = blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash
      |
      |  digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed && valuePreserved && stateChanged
      |}""".stripMargin
  ).asSigmaProp

  val fundingScript = compile(env,
    """{
      |
      |  val selfIndexIsZero = INPUTS(0).id == SELF.id
      |
      |  val proof = getVar[Coll[Byte]](1).get
      |
      |  val inputsCount = INPUTS.size
      |
      |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      |     val pk = b.R4[Coll[Byte]].get
      |     val value = longToByteArray(b.value)
      |     (pk, value)
      |  })
      |
      |  val modifiedTree = SELF.R5[AvlTree].get.insert(toAdd, proof).get
      |
      |  val expectedTree = OUTPUTS(0).R5[AvlTree].get
      |
      |  val properTreeModification = modifiedTree == expectedTree
      |
      |  val outputsCount = OUTPUTS.size == 2
      |  val selfOutputCorrect = OUTPUTS(0).propositionBytes == SELF.propositionBytes
      |  val feeOutputCorrect = (OUTPUTS(1).value <= 1) && (OUTPUTS(1).propositionBytes == feeBytes)
      |
      |  val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect
      |
      |  selfIndexIsZero && outputsCorrect && properTreeModification
      |}""".stripMargin
  ).asBoolValue.toSigmaProp



  property("simple ico example - fundraising stage only") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val initTreeData = new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val projectBoxBefore = ErgoBox(10, fundingScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(1)(0: Byte)), R5 -> AvlTreeConstant(initTreeData)))

    val funderBoxCount = 2000

    val funderBoxes = (1 to funderBoxCount).map { _ =>
      ErgoBox(10, Values.TrueLeaf.asSigmaProp, 0, Seq(),
        Map(R4 -> ByteArrayConstant(Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte))))
    }

    val inputBoxes = IndexedSeq(projectBoxBefore) ++ funderBoxes

    inputBoxes.tail.foreach { b =>
      val k = b.get(R4).get.asInstanceOf[CollectionConstant[SByte.type]].value
      val v = Longs.toByteArray(b.value)
      avlProver.performOneOperation(Insert(ADKey @@ k, ADValue @@ v))
    }

    val proof = avlProver.generateProof()
    val endTree = new AvlTreeData(avlProver.digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val projectBoxAfter = ErgoBox(funderBoxCount * 10 - 1, fundingScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(1)(0: Byte)), R5 -> AvlTreeConstant(endTree)))
    val feeBox = ErgoBox(1, feeProp, 0, Seq(), Map())

    val fundingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfter, feeBox))

    val fundingContext = ErgoLikeContext(
      currentHeight = 3000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = inputBoxes,
      spendingTransaction = fundingTx,
      self = projectBoxBefore)

    val projectProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    val res = projectProver.prove(env, fundingScript, fundingContext, fakeMessage).get
    println("cost: " + res.cost)
  }

  property("simple ico example - fixing stage") {
    val projectProver = new ErgoLikeTestProvingInterpreter
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val openTreeData = new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val projectBoxBeforeClosing = ErgoBox(10, fixingProp, 0, Seq(),
      Map(R4 -> AvlTreeConstant(openTreeData)))

    val closedTreeData = new AvlTreeData(digest, AvlTreeFlags.RemoveOnly, 32, None)

    val projectBoxAfterClosing = ErgoBox(10, withdrawalScript, 0, Seq(),
      Map(R4 -> AvlTreeConstant(closedTreeData)))

    val fixingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfterClosing))

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBeforeClosing),
      spendingTransaction = fixingTx,
      self = projectBoxBeforeClosing)

    projectProver.prove(env, fixingProp, fundingContext, fakeMessage).get
  }

  property("simple ico example - withdrawal stage") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val funderBoxCount = 2000
    val funderProps = (1 to funderBoxCount).map { _ =>
      val keyPoint = CryptoConstants.dlogGroup.createRandomElement()
      val prop = CreateProveDlog(SGroupElement.mkConstant(keyPoint)).asSigmaProp
      val propBytes = DefaultSerializer.serializeErgoTree(prop)
      propBytes -> Longs.toByteArray(Random.nextInt(Int.MaxValue).toLong)
    }
    val funderKvs = funderProps.map { case (prop, v) =>
      val k = Blake2b256(prop)
      k -> v
    }

    funderKvs.foreach { case (k, v) =>
      avlProver.performOneOperation(Insert(ADKey @@ k, ADValue @@ v))
    }
    val digest = avlProver.digest
    val fundersTree = new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val withdrawalsCount = 9
    val withdrawals = funderKvs.take(withdrawalsCount)

    avlProver.generateProof()

    withdrawals.foreach { case (k, _) =>
      avlProver.performOneOperation(Lookup(ADKey @@ k))
    }
    val lookupProof = avlProver.generateProof()

    withdrawals.foreach { case (k, _) =>
      avlProver.performOneOperation(Remove(ADKey @@ k))
    }
    val removalProof = avlProver.generateProof()

    val finalTree = new AvlTreeData(avlProver.digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val withdrawBoxes = funderProps.take(withdrawalsCount).map { case (prop, v) =>
      ErgoBox(Longs.fromByteArray(v), DefaultSerializer.deserializeErgoTree(prop), 0)
    }

    val projectBoxBefore = ErgoBox(2000, withdrawalScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(1)(0: Byte)), R5 -> AvlTreeConstant(fundersTree)))
    val projectBoxAfter = ErgoBox(1000, withdrawalScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(1)(0: Byte)), R5 -> AvlTreeConstant(finalTree)))
    val feeBox = ErgoBox(1, feeProp, 0, Seq(), Map())

    val outputs = IndexedSeq(projectBoxAfter) ++ withdrawBoxes ++ IndexedSeq(feeBox)
    val fundingTx = ErgoLikeTransaction(IndexedSeq(), outputs)

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBefore),
      spendingTransaction = fundingTx,
      self = projectBoxBefore)

    val projectProver =
      new ContextEnrichingTestProvingInterpreter()
        .withContextExtender(2, ByteArrayConstant(removalProof))
        .withContextExtender(3, ByteArrayConstant(lookupProof))
        .withContextExtender(4, IntConstant(10))

    val res = projectProver.prove(env, withdrawalScript, fundingContext, fakeMessage).get
    println("cost: " + res.cost)
    println("remove proof size: " + removalProof.length)
    println("lookup proof size: " + lookupProof.length)
  }

}
