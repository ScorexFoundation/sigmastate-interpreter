package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform._
import sigmastate.AvlTreeData
import sigmastate.Values.{IntConstant, LongConstant, SigmaPropConstant}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter

import scala.util.Failure


class ColdWalletContractExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  property("Evaluation - ColdWallet Contract Example") {

    val alice = new ErgoLikeTestProvingInterpreter // private key controlling hot-wallet funds
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ErgoLikeTestProvingInterpreter // private key controlling hot-wallet funds
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val blocksIn24h = 500
    val percent = 1
    val minSpend = 100

    val env = Map(
      ScriptNameProp -> "env",
      "alice" -> alicePubKey,
      "bob" -> bobPubKey,
      "blocksIn24h" -> IntConstant(blocksIn24h),
      "percent" -> IntConstant(percent),
      "minSpend" -> IntConstant(minSpend)
    )

    // assume that person topping up address creates the box correctly... i.e., sets the correct value of start (in R4)
    val script = compileWithCosting(env,
      """{
        |  val lastMinBal = SELF.R5[Long].get // min balance needed in this period
        |  val depth = HEIGHT - SELF.creationInfo._1 // number of confirmations
        |  val lastStart = min(depth, SELF.R4[Int].get) // height at which period started
        |  val notExpired = HEIGHT - lastStart <= blocksIn24h
        |
        |  val toKeep = max(SELF.value - max(SELF.value * percent / 100, minSpend), 0L)
        |
        |  val newStart:Int = if (notExpired) lastStart else HEIGHT
        |  val newMinBal:Long = if (notExpired) lastMinBal else toKeep
        |
        |  // to avoid the case of spending multiple boxes and creating only one output, we ensure INPUTS.size == 1
        |  val isValidOut = INPUTS.size == 1 && OUTPUTS.exists({(out:Box) =>
        |    out.propositionBytes == SELF.propositionBytes &&
        |    out.value >= newMinBal &&
        |    out.R4[Int].get >= newStart &&
        |    out.R5[Long].get == newMinBal
        |  })
        |
        |  (alice && bob) || (
        |    lastMinBal >= toKeep && // topup should keep lastMinBal > toKeep else UTXO becomes unspendable by (Alice OR Bob)
        |    (alice || bob) &&
        |    (newMinBal == 0 || isValidOut)
        |  )
        |}""".stripMargin).asSigmaProp

    val address = Pay2SHAddress(script)

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below
    val depositAmount = 100000L // 100k
    val depositHeight = 50
    val min = depositAmount - depositAmount * percent/100 // should be 99000 (99k)

    val depositOutput = ErgoBox(depositAmount, address.script, depositHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // can keep any value in R4 initially
        R5 -> LongConstant(min) // keeping it below min will make UTXO unspendable
      )
    )

    val carol = new ErgoLikeTestProvingInterpreter // paying to carol, some arbitrary user
    val carolPubKey = carol.dlogSecrets.head.publicImage

    val firstWithdrawHeight = depositHeight + 1 // quickly withdraw (before expiry)

    val spendEnv = Map(ScriptNameProp -> "spendEnv")

    // Both Alice ane Bob withdraw
    val withdrawAmountFull = depositAmount // full amount is withdrawn

    val withdrawOutputAliceAndBob = ErgoBox(withdrawAmountFull, carolPubKey, firstWithdrawHeight)

    val withdrawTxAliceAndBob = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(withdrawOutputAliceAndBob))

    val withdrawContextAliceandBob = ErgoLikeContext(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTxAliceAndBob,
      self = depositOutput
    )

    val proofAliceAndBobWithdraw = alice.withSecrets(bob.dlogSecrets).prove(spendEnv, script, withdrawContextAliceandBob, fakeMessage).get.proof

    val verifier = new ErgoLikeTestInterpreter

    verifier.verify(spendEnv, script, withdrawContextAliceandBob, proofAliceAndBobWithdraw, fakeMessage).get._1 shouldBe true

    // One of Alice or Bob withdraws (1% max)
    val firstWithdrawAmount = depositAmount * percent / 100     // less than or eqaul to percent (1000)
    val firstChangeAmount = depositAmount - firstWithdrawAmount // 99000

    val firstChangeOutput = ErgoBox(firstChangeAmount, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start) = 50
        R5 -> LongConstant(min) // newMin (= old min) = 99000
      )
    )
    val firstWithdrawOutput = ErgoBox(firstWithdrawAmount, carolPubKey, firstWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val firstWithdrawTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(firstChangeOutput, firstWithdrawOutput))

    val firstWithdrawContext = ErgoLikeContext(
      currentHeight = firstWithdrawHeight, // 51
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = firstWithdrawTx,
      self = depositOutput
    )

    val proofAliceWithdraw = alice.prove(spendEnv, script, firstWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext, proofAliceWithdraw, fakeMessage).get._1 shouldBe true

    val proofBobWithdraw = bob.prove(env, script, firstWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext, proofBobWithdraw, fakeMessage).get._1 shouldBe true

    // invalid (amount greater than allowed)
    val withdrawAmountInvalid = depositAmount * percent / 100 + 1 // more than percent
    val changeAmountInvalid = depositAmount - withdrawAmountInvalid
    val changeOutputInvalid = ErgoBox(changeAmountInvalid, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start)
        R5 -> LongConstant(min) // newMin (= old min)
      )
    )
    val withdrawOutputInvalid = ErgoBox(withdrawAmountInvalid, carolPubKey, firstWithdrawHeight)

    // normally this transaction would be invalid, but we're not checking it in this test
    val withdrawTxInvalid = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(changeOutputInvalid, withdrawOutputInvalid))

    val withdrawContextInvalid = ErgoLikeContext(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTxInvalid,
      self = depositOutput
    )

    an [AssertionError] should be thrownBy (
      alice.prove(spendEnv, script, withdrawContextInvalid, fakeMessage).get
    )
    an [AssertionError] should be thrownBy (
      bob.prove(spendEnv, script, withdrawContextInvalid, fakeMessage).get
    )

    // second withdraw (valid case)
    val secondWithdrawHeight = depositHeight + blocksIn24h + 1

    val secondWithdrawAmount = firstChangeAmount * percent / 100 // less than or equal to percent
    val secondChangeAmount = firstChangeAmount - secondWithdrawAmount
    val secondMin = firstChangeAmount - firstChangeAmount * percent/100

    val secondChangeOutput = ErgoBox(secondChangeAmount, address.script, secondWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(secondWithdrawHeight), // newStart
        R5 -> LongConstant(secondMin) // newMin
      )
    )
    val secondWithdrawOutput = ErgoBox(secondWithdrawAmount, carolPubKey, secondWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val secondWithdrawTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(secondChangeOutput, secondWithdrawOutput))

    val secondWithdrawContext = ErgoLikeContext(
      currentHeight = secondWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(firstChangeOutput),
      spendingTransaction = secondWithdrawTx,
      self = firstChangeOutput
    )

    val proofAliceSecondWithdraw = alice.prove(spendEnv, script, secondWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, secondWithdrawContext, proofAliceSecondWithdraw, fakeMessage).get._1 shouldBe true

    val proofBobSecondWithdraw = bob.prove(spendEnv, script, secondWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, secondWithdrawContext, proofBobSecondWithdraw, fakeMessage).get._1 shouldBe true

  }
}
