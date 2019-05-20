
package sigmastate.utxo.examples

import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._

class TrustlessLETS extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  property("Evaluation - LETS Example") {

    val rateTokenID = Blake2b256("rate")
    val letsTokenID = Blake2b256("lets")
    val memberBoxEnv = Map(
      ScriptNameProp -> "memberBoxScriptEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID
    )

    val memberBoxScript = compile(memberBoxEnv,
      """{
        |val validRate = CONTEXT.dataInputs(0).tokens(0)._1 == rateTokenID
        |val rate = CONTEXT.dataInputs(0).R4[Int].get
        |val inBalance = SELF.R4[Long].get
        |val pubKey = SELF.R5[SigmaProp].get
        |val index = getVar[Int](0).get
        |val outBalance = OUTPUTS(index).R4[Long].get
        |val letsInputs = INPUTS.filter({(b:Box) => b.propositionBytes == SELF.propositionBytes})
        |val letsOutputs = OUTPUTS.filter({(b:Box) => b.propositionBytes == SELF.propositionBytes})
        |
        |// receiver box will contain same amount of ergs.
        |val receiver = outBalance > inBalance && OUTPUTS(index).value == SELF.value
        |val totalInBalance = letsInputs.map({(b:Box) => b.R4[Long].get}).fold(0L, {(l:Long, r:Long) => l + r})
        |val totalOutBalance = letsOutputs.map({(b:Box) => b.R4[Long].get}).fold(0L, {(l:Long, r:Long) => l + r})
        |
        |// negative LETS balance must be backed by ergs
        |val correctErgs = outBalance > 0 || OUTPUTS(index).value == outBalance * rate
        |
        |SELF.tokens(0)._1 == letsTokenID &&
        |OUTPUTS(index).tokens(0)._1 == letsTokenID &&
        |validRate && totalInBalance == totalOutBalance &&
        |letsInputs.size == 2 && letsOutputs.size == 2 &&
        |OUTPUTS(index).propositionBytes == SELF.propositionBytes &&
        |OUTPUTS(index).R5[SigmaProp].get == pubKey &&
        |(receiver || (pubKey && correctErgs))
        |}""".stripMargin
    ).asSigmaProp

    val tokenBoxEnv = Map(
      ScriptNameProp -> "tokenBoxEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "memberBoxScriptHash" -> Blake2b256(memberBoxScript.treeWithSegregation.bytes)
    )

    val tokenScript = compile(tokenBoxEnv,
      """{
        |val tokenBox = OUTPUTS(0) // first output should contain remaining LETS tokens
        |def isLets(b:Box) = {
        |   // A LETS box must have 1 membership token in tokens(0)
        |   b.tokens(0)._1 == letsTokenID && b.tokens(0)._2 == 1 &&
        |   blake2b256(b.propositionBytes) == memberBoxScriptHash &&
        |   SELF.R4[Long].get == 0 // start box with zero LETS balance
        |}
        |val numLetsBoxes = OUTPUTS.filter({(b:Box) => isLets(b)}).size
        |tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |tokenBox.tokens(0)._2 == SELF.tokens(0)._2 - numLetsBoxes
        |}
      """.stripMargin).asSigmaProp

    val tokenBoxCreationHeight = 70
    val tokenAmount = 10      // LongConstant(10)

    val tokenBoxOutput = ErgoBox(tokenAmount, tokenScript, tokenBoxCreationHeight)

  }

}
