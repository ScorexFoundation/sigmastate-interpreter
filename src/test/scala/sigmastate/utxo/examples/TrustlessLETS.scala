
package sigmastate.utxo.examples

import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._

class TrustlessLETS1 extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  property("Evaluation - LETS1 Example") {

    val rateTokenID = Blake2b256("rate")
    val letsTokenID = Blake2b256("lets")
    val minErgsToJoin = 100L // to prevent spam and DDoS attacks
    val minWithdrawTime = 1000 // Ergs locked till 1000 blocks
    val memberBoxEnv = Map(
      ScriptNameProp -> "memberBoxScriptEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "minWithdrawTime" -> minWithdrawTime
    )

    val memberBoxScript = compile(memberBoxEnv,
      """{
        |val validRateOracle = CONTEXT.dataInputs(0).tokens(0)._1 == rateTokenID
        |val rate = CONTEXT.dataInputs(0).R4[Int].get
        |val inBalance = SELF.R4[Long].get
        |val pubKey = SELF.R5[SigmaProp].get
        |val createdAt = SELF.R6[Long].get
        |
        |val index = getVar[Int](0).get
        |val outBalance = OUTPUTS(index).R4[Long].get
        |
        |val isMemberBox = {(b:Box) => b.propositionBytes == SELF.propositionBytes}
        |val letsInputs = INPUTS.filter(isMemberBox)
        |val letsOutputs = OUTPUTS.filter(isMemberBox)
        |
        |// receiver box will contain same amount of ergs.
        |val receiver = outBalance > inBalance && OUTPUTS(index).value == SELF.value
        |val getLetsBalance = {(b:Box) => b.R4[Long].get}
        |
        |val letsBalIn = letsInputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |val letsBalOut = letsOutputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |
        |// sender box can contain less amount of ergs (sender may withdraw ergs provided that any
        |// negative LETS balance of sender is backed by sufficient ergs
        |// for receiver, we don't touch the erg balance, since a receiver is not actively involved
        |// in the transaction
        |
        |val correctErgs = OUTPUTS(index).value >= -outBalance * rate && (
        |  OUTPUTS(index).value >= SELF.value || SELF.R6[Long].get + minWithdrawTime > HEIGHT
        |)
        |
        |inBalance != outBalance && // some transaction should occur
        |SELF.tokens(0)._1 == letsTokenID &&
        |OUTPUTS(index).tokens(0)._1 == letsTokenID &&
        |validRateOracle &&
        |letsBalIn == letsBalOut &&
        |letsInputs.size == 2 && letsOutputs.size == 2 &&
        |OUTPUTS(index).propositionBytes == SELF.propositionBytes &&
        |OUTPUTS(index).R5[SigmaProp].get == pubKey &&
        |OUTPUTS(index).R6[Long].get == SELF.R6[Long].get && // creation height
        |(receiver || (pubKey && correctErgs))
        |}""".stripMargin
    ).asSigmaProp

    val tokenBoxEnv = Map(
      ScriptNameProp -> "tokenBoxEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "minErgsToJoin" -> minErgsToJoin,
      "memberBoxScriptHash" -> Blake2b256(memberBoxScript.treeWithSegregation.bytes)
    )

    val tokenScript = compile(tokenBoxEnv,
      """{
        |val tokenBox = OUTPUTS(0) // first output should contain remaining LETS tokens
        |def isLets(b:Box) = {
        |   // A LETS box must have 1 membership token in tokens(0)
        |   b.tokens(0)._1 == letsTokenID && b.tokens(0)._2 == 1 &&
        |   blake2b256(b.propositionBytes) == memberBoxScriptHash &&
        |   SELF.R4[Long].get == 0 && // start box with zero LETS balance
        |   b.value >= minErgsToJoin &&
        |	  b.R6[Long].get <= HEIGHT // store the creation height in R6
        |}
        |val numLetsBoxes = OUTPUTS.filter({(b:Box) => isLets(b)}).size
        |tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |tokenBox.tokens(0)._2 == SELF.tokens(0)._2 - numLetsBoxes &&
        |tokenBox.propositionBytes == SELF.propositionBytes
        |}
      """.stripMargin).asSigmaProp

    val tokenBoxCreationHeight = 70
    val tokenAmount = 10      // LongConstant(10)

    val tokenBoxOutput = ErgoBox(tokenAmount, tokenScript, tokenBoxCreationHeight)

  }

}

class TrustlessLETS2 extends SigmaTestingCommons {
  // LETS2
  //  Non-refundable ergs
  //  Zero sum
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  property("Evaluation - LETS2 Example") {

    val rateTokenID = Blake2b256("rate")
    val letsTokenID = Blake2b256("lets")
    val minErgsToJoin = 100L // to prevent spam and DDoS attacks

    val memberBoxEnv = Map(
      ScriptNameProp -> "memberBoxScriptEnv",
      "letsTokenID" -> letsTokenID,
      "maxNegativeBalance" -> -10000
    )

    val memberBoxScript = compile(memberBoxEnv,
      """{
        |val inBalance = SELF.R4[Long].get
        |val pubKey = SELF.R5[SigmaProp].get
        |
        |val index = getVar[Int](0).get
        |val outBalance = OUTPUTS(index).R4[Long].get
        |
        |val isMemberBox = {(b:Box) => b.propositionBytes == SELF.propositionBytes}
        |val letsInputs = INPUTS.filter(isMemberBox)
        |val letsOutputs = OUTPUTS.filter(isMemberBox)
        |
        |// receiver box will contain same amount of ergs.
        |val receiver = outBalance > inBalance && OUTPUTS(index).value == SELF.value // there may be some ergs in receiver box
        |val getLetsBalance = {(b:Box) => b.R4[Long].get}
        |
        |val letsBalIn = letsInputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |val letsBalOut = letsOutputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |
        |inBalance != outBalance && // some transaction should occur
        |outBalance >= maxNegativeBalance && // cap negative to a limit
        |SELF.tokens(0)._1 == letsTokenID &&
        |OUTPUTS(index).tokens(0)._1 == letsTokenID &&
        |letsBalIn == letsBalOut &&
        |letsInputs.size == 2 && letsOutputs.size == 2 &&
        |OUTPUTS(index).propositionBytes == SELF.propositionBytes &&
        |OUTPUTS(index).R5[SigmaProp].get == pubKey &&
        |receiver || pubKey
        |}""".stripMargin
    ).asSigmaProp

    val trustedProver = new ContextEnrichingTestProvingInterpreter
    val trustedPubKey = trustedProver.dlogSecrets.head.publicImage

    val tokenBoxEnv = Map(
      ScriptNameProp -> "tokenBoxEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "minErgsToJoin" -> minErgsToJoin,
      "trustedPubKey" -> trustedPubKey,
      "memberBoxScriptHash" -> Blake2b256(memberBoxScript.treeWithSegregation.bytes)
    )

    val tokenScript = compile(tokenBoxEnv,
      """{
        |val tokenBox = OUTPUTS(0) // first output should contain remaining LETS tokens and joining fee added
        |val letsBox = OUTPUTS(1) // second output contains membership box that is created
        |val ergsAdded = tokenBox.value - SELF.value
        |
        |val validLetsBox = {
        |  // A LETS box must have 1 membership token in tokens(0)
        |  letsBox.tokens(0)._1 == letsTokenID &&
        |  letsBox.tokens(0)._2 == 1 &&
        |  blake2b256(letsBox.propositionBytes) == memberBoxScriptHash &&
        |  letsBox.R4[Long].get == 0 // start box with zero LETS balance
        |}
        |
        |val memberSpend = {
        |  tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |  tokenBox.tokens(0)._2 == SELF.tokens(0)._2 - 1 && // only one LETS box created at a time
        |  tokenBox.propositionBytes == SELF.propositionBytes &&
        |  validLetsBox &&
        |  ergsAdded >= minErgsToJoin
        |}
        |
        |val trustedPubKeySpend = {
        |  tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |  tokenBox.tokens(0)._2 == SELF.tokens(0)._2 && // no tokens transferred. Only for sweeping ergs
        |  tokenBox.propositionBytes == SELF.propositionBytes &&
        |  trustedPubKey
        |}
        |
        |memberSpend || trustedPubKeySpend
        |}
      """.stripMargin).asSigmaProp

    val tokenBoxCreationHeight = 70
    val tokenAmount = 10      // LongConstant(10)

    val tokenBoxOutput = ErgoBox(tokenAmount, tokenScript, tokenBoxCreationHeight)

  }

}


class TrustlessLETS3 extends SigmaTestingCommons {
  // LETS3
  //  time-locked ergs
  //  Positive sum

  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  property("Evaluation - LETS3 Example") {

    val rateTokenID = Blake2b256("rate")
    val letsTokenID = Blake2b256("lets")
    val minErgsToJoin = 100L // to prevent spam and DDoS attacks
    val minWithdrawTime = 1000 // Ergs locked till 1000 blocks

    val memberBoxEnv = Map(
      ScriptNameProp -> "memberBoxScriptEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "minWithdrawTime" -> minWithdrawTime
    )

    val memberBoxScript = compile(memberBoxEnv,
      """{
        |val validRateOracle = CONTEXT.dataInputs(0).tokens(0)._1 == rateTokenID
        |val rate = CONTEXT.dataInputs(0).R4[Int].get
        |val inBalance = SELF.R4[Long].get
        |val pubKey = SELF.R5[SigmaProp].get
        |val createdAt = SELF.R6[Long].get
        |
        |val index = getVar[Int](0).get
        |val outBalance = OUTPUTS(index).R4[Long].get
        |
        |val isMemberBox = {(b:Box) => b.propositionBytes == SELF.propositionBytes}
        |val letsInputs = INPUTS.filter(isMemberBox)
        |val letsOutputs = OUTPUTS.filter(isMemberBox)
        |
        |// receiver box will contain same amount of ergs.
        |val receiver = outBalance > inBalance && OUTPUTS(index).value == SELF.value
        |val getLetsBalance = {(b:Box) => b.R4[Long].get}
        |
        |val letsBalIn = letsInputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |val letsBalOut = letsOutputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |
        |// sender box can contain less amount of ergs (sender may withdraw ergs provided that any
        |// negative LETS balance of sender is backed by sufficient ergs
        |// for receiver, we don't touch the erg balance, since a receiver is not actively involved
        |// in the transaction
        |
        |val destroyedLetsBalance = letsBalIn - letsBalOut
        |
        |val correctErgs = SELF.value - OUTPUTS(index).value >= destroyedLetsBalance * rate && (
        |  OUTPUTS(index).value >= SELF.value || SELF.R6[Long].get + minWithdrawTime > HEIGHT
        |)
        |
        |inBalance != outBalance && // some transaction should occur
        |outBalance >= 0 && // only positive LETS balance permitted in LETS-3
        |SELF.tokens(0)._1 == letsTokenID &&
        |OUTPUTS(index).tokens(0)._1 == letsTokenID &&
        |validRateOracle &&
        |letsInputs.size == 2 && letsOutputs.size == 2 &&
        |OUTPUTS(index).propositionBytes == SELF.propositionBytes &&
        |OUTPUTS(index).R5[SigmaProp].get == pubKey &&
        |OUTPUTS(index).R6[Long].get == SELF.R6[Long].get && // creation height
        |(receiver || (pubKey && correctErgs))
        |}""".stripMargin
    ).asSigmaProp

    val trustedProver = new ContextEnrichingTestProvingInterpreter
    val trustedPubKey = trustedProver.dlogSecrets.head.publicImage

    val tokenBoxEnv = Map(
      ScriptNameProp -> "tokenBoxEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "minErgsToJoin" -> minErgsToJoin,
      "trustedPubKey" -> trustedPubKey,
      "memberBoxScriptHash" -> Blake2b256(memberBoxScript.treeWithSegregation.bytes)
    )

    val tokenScript = compile(tokenBoxEnv,
      """{
        |val validRateOracle = CONTEXT.dataInputs(0).tokens(0)._1 == rateTokenID
        |val rate = CONTEXT.dataInputs(0).R4[Int].get
        |val tokenBox = OUTPUTS(0) // first output should contain remaining LETS tokens and joining fee added
        |val letsBox = OUTPUTS(1) // second output contains membership box that is created
        |
        |val validLetsBox = {
        |  // A LETS box must have 1 membership token in tokens(0)
        |  letsBox.tokens(0)._1 == letsTokenID &&
        |  letsBox.tokens(0)._2 == 1 &&
        |  blake2b256(letsBox.propositionBytes) == memberBoxScriptHash &&
        |  SELF.R4[Long].get <= letsBox.value / rate && // start box with LETS balance based on ergsAdded
        |  letsBox.value >= minErgsToJoin &&
        |	 letsBox.R6[Long].get <= HEIGHT // store the creation height in R6
        |}
        |
        |val validTokenBox = {
        |  tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |  tokenBox.tokens(0)._2 == SELF.tokens(0)._2 - 1 && // only one LETS box created at a time
        |  tokenBox.propositionBytes == SELF.propositionBytes
        |}
        |
        |validLetsBox && validTokenBox
        |}
      """.stripMargin).asSigmaProp

    val tokenBoxCreationHeight = 70
    val tokenAmount = 10      // LongConstant(10)

    val tokenBoxOutput = ErgoBox(tokenAmount, tokenScript, tokenBoxCreationHeight)

  }

}

class TrustlessLETS4 extends SigmaTestingCommons {
  // LETS4
  //  Non-refundable ergs
  //  Positive sum
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  property("Evaluation - LETS4 Example") {

    val rateTokenID = Blake2b256("rate")
    val letsTokenID = Blake2b256("lets")
    val minErgsToJoin = 100L // to prevent spam and DDoS attacks

    val memberBoxEnv = Map(
      ScriptNameProp -> "memberBoxScriptEnv",
      "letsTokenID" -> letsTokenID
    )

    val memberBoxScript = compile(memberBoxEnv,
      """{
        |val inBalance = SELF.R4[Long].get
        |val pubKey = SELF.R5[SigmaProp].get
        |
        |val index = getVar[Int](0).get
        |val outBalance = OUTPUTS(index).R4[Long].get
        |
        |val isMemberBox = {(b:Box) => b.propositionBytes == SELF.propositionBytes}
        |val letsInputs = INPUTS.filter(isMemberBox)
        |val letsOutputs = OUTPUTS.filter(isMemberBox)
        |
        |// receiver box will contain same amount of ergs.
        |val receiver = outBalance > inBalance && OUTPUTS(index).value == SELF.value // there may be some ergs in receiver box
        |val getLetsBalance = {(b:Box) => b.R4[Long].get}
        |
        |val letsBalIn = letsInputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |val letsBalOut = letsOutputs.map(getLetsBalance).fold(0L, {(l:Long, r:Long) => l + r})
        |
        |inBalance != outBalance && // some transaction should occur
        |outBalance >= 0 && // only positive LETS balance permitted in LETS-4
        |SELF.tokens(0)._1 == letsTokenID &&
        |OUTPUTS(index).tokens(0)._1 == letsTokenID &&
        |letsBalIn == letsBalOut &&
        |letsInputs.size == 2 && letsOutputs.size == 2 &&
        |OUTPUTS(index).propositionBytes == SELF.propositionBytes &&
        |OUTPUTS(index).R5[SigmaProp].get == pubKey &&
        |receiver || pubKey
        |}""".stripMargin
    ).asSigmaProp

    val trustedProver = new ContextEnrichingTestProvingInterpreter
    val trustedPubKey = trustedProver.dlogSecrets.head.publicImage

    val tokenBoxEnv = Map(
      ScriptNameProp -> "tokenBoxEnv",
      "rateTokenID" -> rateTokenID,
      "letsTokenID" -> letsTokenID,
      "minErgsToJoin" -> minErgsToJoin,
      "trustedPubKey" -> trustedPubKey,
      "memberBoxScriptHash" -> Blake2b256(memberBoxScript.treeWithSegregation.bytes)
    )

    val tokenScript = compile(tokenBoxEnv,
      """{
        |val validRateOracle = CONTEXT.dataInputs(0).tokens(0)._1 == rateTokenID
        |val rate = CONTEXT.dataInputs(0).R4[Int].get
        |val tokenBox = OUTPUTS(0) // first output should contain remaining LETS tokens and joining fee added
        |val letsBox = OUTPUTS(1) // second output contains membership box that is created
        |val ergsAdded = tokenBox.value - SELF.value
        |
        |val validLetsBox = {
        |  // A LETS box must have 1 membership token in tokens(0)
        |  letsBox.tokens(0)._1 == letsTokenID &&
        |  letsBox.tokens(0)._2 == 1 &&
        |  blake2b256(letsBox.propositionBytes) == memberBoxScriptHash &&
        |  letsBox.R4[Long].get <= ergsAdded / rate // start box with LETS balance based on ergsAdded
        |}
        |
        |val memberSpend = {
        |  tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |  tokenBox.tokens(0)._2 == SELF.tokens(0)._2 - 1 && // only one LETS box created at a time
        |  tokenBox.propositionBytes == SELF.propositionBytes &&
        |  validLetsBox &&
        |  ergsAdded >= minErgsToJoin
        |}
        |
        |val trustedPubKeySpend = {
        |  tokenBox.tokens(0)._1 == SELF.tokens(0)._1 &&
        |  tokenBox.tokens(0)._2 == SELF.tokens(0)._2 && // no tokens transferred. Only for sweeping ergs
        |  tokenBox.propositionBytes == SELF.propositionBytes &&
        |  trustedPubKey
        |}
        |
        |memberSpend || trustedPubKeySpend
        |}
      """.stripMargin).asSigmaProp

    val tokenBoxCreationHeight = 70
    val tokenAmount = 10      // LongConstant(10)

    val tokenBoxOutput = ErgoBox(tokenAmount, tokenScript, tokenBoxCreationHeight)

  }

}

