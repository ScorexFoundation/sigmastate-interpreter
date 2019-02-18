package sigmastate.utxo.examples

import sigmastate.helpers.SigmaTestingCommons
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
        |     val pk = b.R4[SByteArray].get
        |     val value = b.value
        |     (pk, value)
        |  }
        |
        |  val funders = INPUTS.filter{(b: Box) => b.R5[Int].isEmpty}
        |
        |  val toAdd = funders.map(toAddFn)
        |
        |  val modifiedTree = treeInserts(SELF.R4[AvlTree].get, toAdd, proof).get
        |
        |  val addedCorrectly = modifiedTree == fundingOut.R4[AvlTree].get
        |
        |  addedCorrectly
        |
        |}""".stripMargin
    ).asBoolValue

    println(fundingScript)
  }
}