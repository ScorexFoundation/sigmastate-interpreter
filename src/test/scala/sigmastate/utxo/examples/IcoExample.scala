package sigmastate.utxo.examples

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import sigmastate.AvlTreeData
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._

class IcoExample extends SigmaTestingCommons {
  /**
    * Simplest ICO example
    */
  property("simple ico example") {

    implicit val IR: TestingIRContext = new TestingIRContext

    val fundingEnv = Map(
      ScriptNameProp -> "fundingScriptEnv",
      "proof" -> Array.emptyByteArray
    )

    val fundingScript = compileWithCosting(fundingEnv,
      """{
        |  val fundingOut = OUTPUTS(0)
        |
        |  val toAddFn = { (b: Box) =>
        |     val pk = b.R4[Coll[Byte]].get
        |     val value = longToByteArray(b.value)
        |     (pk, value)
        |  }
        |
        |  // val funders: Coll[Box] = INPUTS.filter({(b: Box) => b.R5[Int].isEmpty })
        |
        |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = INPUTS.map(toAddFn)
        |
        |  val modifiedTree = treeInserts(SELF.R4[AvlTree].get, toAdd, proof).get
        |
        |  val addedCorrectly = modifiedTree == fundingOut.R4[AvlTree].get
        |
        |  addedCorrectly
        |
        |}""".stripMargin
    ).asBoolValue

    val projectProver = new ErgoLikeTestProvingInterpreter

    val projectBoxBefore = ErgoBox(10, fundingScript, 0)

    val fundingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxBefore))

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBefore),
      spendingTransaction = fundingTx,
      self = projectBoxBefore)

    println(projectProver.prove(fundingEnv, fundingScript, fundingContext, fakeMessage))



  }
}