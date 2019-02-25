package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.dsl.TestContractSpec
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import scorex.crypto.authds.avltree.batch.BatchAVLProver
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values.{AvlTreeConstant, ByteArrayConstant}
import sigmastate.{AvlTreeData, AvlTreeFlags}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._

class IcoExample extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext()
  lazy val spec = TestContractSpec(suite)
  lazy val backer = spec.ProvingParty("Alice")
  lazy val project = spec.ProvingParty("Bob")

  /**
    * Simplest ICO example
    */
  property("simple ico example") {

    val fundingEnv = Map(
      ScriptNameProp -> "fundingScriptEnv",
      "proof" -> Array.emptyByteArray
    )

    val fundingScript = compileWithCosting(fundingEnv,
      """{
        |
        |  val inputIndexes = getVar[Coll[Int]](1)
        |
        |  val toAddFn = { (b: Box) =>
        |     val pk = b.R4[Coll[Byte]].get
        |     val value = longToByteArray(b.value)
        |     (pk, value)
        |  }
        |
        |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = inputIndexes.map{ (i: Int) =>
        |   val input = INPUTS(i)
        |   toAddFn(input)
        |  }
        |
        |  val modifiedTree = treeInserts(SELF.R4[AvlTree].get, toAdd, proof).get
        |
        |  val expectedTree = OUTPUTS(0).R4[AvlTree].get
        |
        |  modifiedTree == expectedTree
        |
        |}""".stripMargin
    ).asBoolValue.toSigmaProp


    val fixingScript = compileWithCosting(fundingEnv,
      """{
        |}""".stripMargin
    ).asBoolValue.toSigmaProp


    val withdrawTokensScript = compileWithCosting(fundingEnv,
      """{
        |
        |  val totalTokens = 1000000
        |
        |  val tokenPrice = SELF.value / totalTokens
        |
        |  val outputIndexes = getVar[Coll[Int]](1)
        |
        |  val selfOut = OUTPUTS(0)
        |
        |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = outputIndexes.map{ (i: Int) =>
        |   val input = INPUTS(i)
        |   toAddFn(input)
        |  }
        |
        |  val modifiedTree = treeRemovals(SELF.R4[AvlTree].get, toAdd, proof).get
        |
        |  val expectedTree = OUTPUTS(0).R4[AvlTree].get
        |
        |  modifiedTree == expectedTree
        |
        |}""".stripMargin
    ).asBoolValue.toSigmaProp


    val withdrawErgoScript = compileWithCosting(fundingEnv,
      """{
          |
          |
          |}""".stripMargin
    ).asBoolValue.toSigmaProp


    println(fundingScript)

    val projectProver = new ErgoLikeTestProvingInterpreter

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val flags = AvlTreeFlags.AllOperationsAllowed
    val initTreeData = new AvlTreeData(digest, flags, 32, None)

    val projectBoxBefore = ErgoBox(10, fundingScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(32)(0:Byte)), R5 -> AvlTreeConstant(initTreeData)))

    val projectBoxAfter = ErgoBox(10, fundingScript, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(32)(0:Byte)), R5 -> AvlTreeConstant(initTreeData)))

    val fundingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfter))

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBefore),
      spendingTransaction = fundingTx,
      self = projectBoxBefore)

    projectProver.prove(fundingEnv, fundingScript, fundingContext, fakeMessage).get

  }

}