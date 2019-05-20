
package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigmastate.Values.{ByteArrayConstant, ByteConstant, IntConstant, SigmaPropConstant}
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._
import sigmastate.utxo._

class TrustlessLETS extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  /** XOR game:

     Alice creates a XOR game of "playAmount" ergs by creating a Half-game UTXO called the "halfGameOutput" output below.
     Another player (Bob) then sends a transaction spending Alice's UTXO and creating another output called "fullGameOutput" (a "Full game" UTXO).
     After Alice opens her commitment (see below), the fullGameOutput can be spent by the winner.
     The transactions encode the following protocol.

     protocol:
       Step 1: Alice commits to secret bit a as follows:
                  Generate random s and compute h = Hash(s||a)
                  h is the commitment to a
               Alice also selects the "play amount", the amount each player must spend to participate.
               She generates a halfGameOutput encoding h and some spending condition given below by halfGameScript
       Step 2: Bob chooses random bit b (public) and creates a new tx spending Alice's UTXO along with
               some others and creating one output that has the spending conditions given by fullGameScript.
               (one of the conditions being that the amount of that output is >= twice the play amount.)
       Step 3: Alice reveals (s, a) to open her commitment and wins if a == b. Otherwise Bob wins.
               If Alice fails to open her commitment before some deadline then Bob automatically wins.

    For simplicity, we will use following bytes to designate bits
        0x00 = false
        0x01 = true

    */
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

    // Note that below script allows Alice to spend the half-game output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-game output
    // before some minimum height.
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

    /////////////////////////////////////////////////////////
    //// Alice starts creating a Half-Game
    /////////////////////////////////////////////////////////

    // she creates a transaction that outputs a box with halfGame script.
    // In the example, we don't create the transaction; we just create a box below

    val tokenBoxCreationHeight = 70
    val tokenAmount = 10      // LongConstant(10)

    val tokenBoxOutput = ErgoBox(tokenAmount, tokenScript, tokenBoxCreationHeight)

  }

}
