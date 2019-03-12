package sigmastate.utxo.examples

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.dsl.TestContractSpec
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values.{AvlTreeConstant, ByteArrayConstant, CollectionConstant}
import sigmastate.{AvlTreeData, AvlTreeFlags, SByte}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._

class IcoExample extends SigmaTestingCommons {
  suite =>
  implicit lazy val IR: TestingIRContext = new TestingIRContext()
  lazy val spec = TestContractSpec(suite)
  lazy val backer = spec.ProvingParty("Alice")
  lazy val project = spec.ProvingParty("Bob")

  /**
    * Simplest ICO example
    */
  ignore("simple ico example - fundraising stage only") {
    val fundingEnv = Map(
      ScriptNameProp -> "fundingScriptEnv"
    )

    val fundingScript = compile(fundingEnv,
      """{
        |  val proof = getVar[Coll[Byte]](1).get
        |
        |  // val funders: Coll[Box] = INPUTS.filter({(b: Box) => b.R5[Int].isEmpty})
        |
        |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = INPUTS.map({ (b: Box) =>
        |     val pk = b.R4[Coll[Byte]].get
        |     val value = longToByteArray(b.value)
        |     (pk, value)
        |  })
        |
        |  val modifiedTree = SELF.R5[AvlTree].get.insert(toAdd, proof).get
        |
        |  val expectedTree = OUTPUTS(0).R5[AvlTree].get
        |
        |  modifiedTree == expectedTree
        |
        |}""".stripMargin
    ).asBoolValue.toSigmaProp

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val initTreeData = new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val projectBoxBefore = ErgoBox(10, fundingScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(16)(0: Byte) ++ Array.fill(16)(1: Byte)), R5 -> AvlTreeConstant(initTreeData)))

    val inputBoxes = IndexedSeq(projectBoxBefore)

    inputBoxes.foreach { b =>
      val k = b.get(R4).get.asInstanceOf[CollectionConstant[SByte.type]].value
      val v = Longs.toByteArray(b.value)
      avlProver.performOneOperation(Insert(ADKey @@ k, ADValue @@ v))
    }

    val proof = avlProver.generateProof()
    val endTree = new AvlTreeData(avlProver.digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val projectBoxAfter = ErgoBox(10, fundingScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(32)(0: Byte)), R5 -> AvlTreeConstant(endTree)))

    val fundingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfter))

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = inputBoxes,
      spendingTransaction = fundingTx,
      self = projectBoxBefore)

    val projectProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    projectProver.prove(fundingEnv + (ScriptNameProp -> "fundingScriptEnv"), fundingScript, fundingContext, fakeMessage).get
  }

  property("simple ico example - fixing stage") {

    val fixingEnv = Map(
      ScriptNameProp -> "fixingScriptEnv"
    )

    val fixingProp = compile(fixingEnv,
      """{
        |  val openTree = SELF.R4[AvlTree].get
        |
        |  val closedTree = OUTPUTS(0).R4[AvlTree].get
        |
        |  val digestPreserved = openTree.digest == closedTree.digest
        |  val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
        |  val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
        |  val treeIsClosed = closedTree.enabledOperations == 0
        |
        |  sigmaProp(digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed)
        |}""".stripMargin
    ).asSigmaProp

    val projectProver = new ErgoLikeTestProvingInterpreter
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val openTreeData = new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None)

    val projectBoxBeforeClosing = ErgoBox(10, fixingProp, 0, Seq(),
      Map(R4 -> AvlTreeConstant(openTreeData)))

    val closedTreeData = new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None)

    val projectBoxAfterClosing = ErgoBox(10, fixingProp, 0, Seq(),
      Map(R4 -> AvlTreeConstant(closedTreeData)))

    val fixingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfterClosing))

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBeforeClosing),
      spendingTransaction = fixingTx,
      self = projectBoxBeforeClosing)

    projectProver.prove(fixingEnv, fixingProp, fundingContext, fakeMessage).get
  }
}