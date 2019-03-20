package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform._
import sigmastate.AvlTreeData
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._


class ColdWalletAdvContractExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  property("Evaluation - ColdWallet Advanced Contract Example") {

    val alice = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val carol = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val carolPubKey = carol.dlogSecrets.head.publicImage

    val blocksIn24h = 500
    val percent1Key = 1
    val percent2Key = 10
    val minSpend = 100

    val env = Map(
      ScriptNameProp -> "env",
      "user1" -> alicePubKey,
      "user2" -> bobPubKey,
      "user3" -> carolPubKey,
      "blocksIn24h" -> IntConstant(blocksIn24h),
      "percent1Key" -> IntConstant(percent1Key),
      "percent2Key" -> IntConstant(percent2Key),
      "minSpend" -> IntConstant(minSpend)
    )

    // assume that person topping up this address creates the box correctly... i.e., sets the correct values of R4 and R5
    // To Do: example that allows spending multiple Boxes with the same script
    //        idea is as follows:
    //           1. make global avbl1Key as sum of all avbl1Key's, where each avbl1Key is computed via its own start
    //           2. make global avbl2Key as sum of all avbl2Key's, where each avbl2Key is computed via its own start
    //           3. make start of output box as max of all starts of the inputs
    //           4. output must contain at least one box of same type with min bal based on avbl1Key/avbl2Key

    val script = compile(env,
      """{
        |  val depth = HEIGHT - SELF.creationInfo._1 // number of confirmations
        |  val start = min(depth, SELF.R4[Int].get) // height at which period started
        |
        |  val notExpired = HEIGHT - start <= blocksIn24h
        |  val newStart:Int = if (notExpired) start else HEIGHT
        |
        |  // available for one user to spend in this period
        |  val avbl1Key = if (notExpired)
        |                  SELF.R5[Long].get
        |                else
        |                  max(SELF.value * percent1Key / 100, minSpend)
        |
        |  // available for two users to spend in this period
        |  val avbl2Key = if (notExpired)
        |                  SELF.R6[Long].get
        |                else
        |                  max(SELF.value * percent2Key / 100, minSpend)
        |
        |  val out = OUTPUTS(0) // change output
        |
        |  // to do: double check if negatives values of avbl1Key create any problem
        |  val isValid1Key = INPUTS.size == 1 && out.propositionBytes == SELF.propositionBytes &&
        |    out.R4[Int].get >= newStart && out.value >= SELF.value - avbl1Key &&
        |    out.value - out.R5[Long].get == SELF.value - avbl1Key && out.R6[Long].get == SELF.R6[Long].get
        |
        |  val isValid2Key = INPUTS.size == 1 && out.propositionBytes == SELF.propositionBytes &&
        |    out.R4[Int].get >= newStart && out.value >= SELF.value - avbl2Key &&
        |    out.value - out.R6[Long].get == SELF.value - avbl2Key && out.R5[Long].get == SELF.R5[Long].get
        |
        |  allOf(Coll(user1, user2, user3)) || (
        |    (anyOf(Coll(user1, user2, user3)) && (SELF.value <= avbl1Key || isValid1Key)) ||
        |    (atLeast(2, Coll(user1, user2, user3)) && (SELF.value <= avbl2Key || isValid2Key))
        |  )
        |}""".stripMargin).asSigmaProp

    val address = Pay2SHAddress(script)

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below
    val depositAmount = 100000L
    val depositHeight = 100
    val avbl1Key = depositAmount * percent1Key/100
    val avbl2Key = depositAmount * percent2Key/100

    val depositOutput = ErgoBox(depositAmount, address.script, depositHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // can keep value in R4 initially
        R5 -> LongConstant(avbl1Key), // keeping it below min will make UTXO unspendable
        R6 -> LongConstant(avbl2Key) // keeping it below min will make UTXO unspendable
      )
    )

    val dave = new ErgoLikeTestProvingInterpreter // paying to dave, some arbitrary user
    val davePubKey = dave.dlogSecrets.head.publicImage

    val firstWithdrawHeight = depositHeight + 1 //

    val spendEnv = Map(ScriptNameProp -> "spendEnv")

    // One of Alice, Bob  or Carol withdraws
    val firstWithdrawAmount1Key = depositAmount * percent1Key / 100 // less than or equal to percent
    val firstChangeAmount1Key = depositAmount - firstWithdrawAmount1Key

    val firstChangeOutput1Key = ErgoBox(firstChangeAmount1Key, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start)
        R5 -> LongConstant(avbl1Key - firstWithdrawAmount1Key), // new avbl1Key (= 0)
        R6 -> LongConstant(avbl2Key) // new avbl2Key (= old avbl2Key)
      )
    )
    val firstWithdrawOutput1Key = ErgoBox(firstWithdrawAmount1Key, carolPubKey, firstWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val firstWithdrawTx1Key = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(firstChangeOutput1Key, firstWithdrawOutput1Key))

    val firstWithdrawContext1Key = ErgoLikeContext(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = firstWithdrawTx1Key,
      self = depositOutput
    )

    val verifier = new ErgoLikeTestInterpreter

    val proofAliceWithdraw = alice.prove(spendEnv, script, firstWithdrawContext1Key, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext1Key, proofAliceWithdraw, fakeMessage).get._1 shouldBe true

    val proofBobWithdraw = bob.prove(env, script, firstWithdrawContext1Key, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext1Key, proofBobWithdraw, fakeMessage).get._1 shouldBe true

    val proofCarolWithdraw = carol.prove(env, script, firstWithdrawContext1Key, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext1Key, proofCarolWithdraw, fakeMessage).get._1 shouldBe true

    // any two of Alice, Bob  or Carol withdraws
    val firstWithdrawAmount2Key = depositAmount * percent2Key / 100 // less than or equal to percent
    val firstChangeAmount2Key = depositAmount - firstWithdrawAmount2Key

    val firstChangeOutput2Key = ErgoBox(firstChangeAmount2Key, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start)
        R5 -> LongConstant(avbl1Key), // new avbl1Key (= 0)
        R6 -> LongConstant(avbl2Key - firstWithdrawAmount2Key) // new avbl2Key (= old avbl2Key)
      )
    )
    val firstWithdrawOutput2Key = ErgoBox(firstWithdrawAmount2Key, carolPubKey, firstWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val firstWithdrawTx2Key = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(firstChangeOutput2Key, firstWithdrawOutput2Key))

    val firstWithdrawContext2Key = ErgoLikeContext(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = firstWithdrawTx2Key,
      self = depositOutput
    )

    val proofAliceBobWithdraw = alice.withSecrets(bob.dlogSecrets).prove(spendEnv, script, firstWithdrawContext2Key, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext2Key, proofAliceBobWithdraw, fakeMessage).get._1 shouldBe true

  }
}
