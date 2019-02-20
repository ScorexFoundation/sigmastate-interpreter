package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform._
import sigmastate.AvlTreeData
import sigmastate.Values.{IntConstant, LongConstant}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter


class ColdWalletAdvContractExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  property("Evaluation - ColdWallet Advanced Contract Example") {

    val alice = new ErgoLikeTestProvingInterpreter // private key controlling hot-wallet funds
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ErgoLikeTestProvingInterpreter // private key controlling hot-wallet funds
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val carol = new ErgoLikeTestProvingInterpreter // private key controlling hot-wallet funds
    val carolPubKey = carol.dlogSecrets.head.publicImage

    val blocksIn24h = 500
    val percent = 1
    val minSpend = 100

    val env = Map(
      ScriptNameProp -> "env",
      "user1" -> alicePubKey,
      "user2" -> bobPubKey,
      "user3" -> carolPubKey,
      "blocksIn24h" -> IntConstant(blocksIn24h),
      "percent" -> IntConstant(percent),
      "minSpend" -> IntConstant(minSpend)
    )

    val script = compileWithCosting(env,
      """{
        |  val min = SELF.R5[Long].get // min balance needed in this period
        |  val depth = HEIGHT - SELF.creationInfo._1 // number of confirmations
        |  val start = min(depth, SELF.R4[Int].get) // height at which period started
        |  val notExpired = HEIGHT - start <= blocksIn24h
        |
        |  val ours:Long = SELF.value - SELF.value * percent / 100
        |  val keep = if (ours > minSpend) ours else 0L
        |
        |  val newStart:Int = if (notExpired) start else HEIGHT
        |  val newMin:Long = if (notExpired) min else keep
        |
        |  val isValid = {(someMin:Long) =>
        |   OUTPUTS.exists({(out:Box) =>
        |    out.propositionBytes == SELF.propositionBytes &&
        |    out.value >= someMin &&
        |    out.R4[Int].get >= newStart &&
        |    out.R5[Long].get == someMin
        |   })
        |  }
        |
        |  allOf(Coll(user1, user2, user3)) || (
        |    min >= keep && // topup should keep min > keep to ensure spendable by anyOf(user1, user2, user3)
        |    anyOf(Coll(user1, user2, user3)) &&
        |    (newMin == 0 || isValid(newMin)) // error here
        |    //(newMin == 0 || isValid(9900L)) // works
        |  )
        |}""".stripMargin).asBoolValue

    val address = Pay2SHAddress(script)

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below
    val depositAmount = 10000L
    val depositHeight = 100
    val min = depositAmount - depositAmount * percent/100

    val depositOutput = ErgoBox(depositAmount, address.script, depositHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // can keep any value in R4 initially
        R5 -> LongConstant(min) // keeping it below min will make UTXO unspendable
      )
    )

    val dave = new ErgoLikeTestProvingInterpreter // paying to dave, some arbitrary user
    val davePubKey = dave.dlogSecrets.head.publicImage

    val firstWithdrawHeight = depositHeight + 1 //

    val spendEnv = Map(ScriptNameProp -> "spendEnv")

    // One of Alice, Bob  or Carol withdraws
    val firstWithdrawAmount = depositAmount * percent / 100 // less than or eqaul to percent
    val firstChangeAmount = depositAmount - firstWithdrawAmount

    val firstChangeOutput = ErgoBox(firstChangeAmount, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start)
        R5 -> LongConstant(min) // newMin (= old min)
      )
    )
    val firstWithdrawOutput = ErgoBox(firstWithdrawAmount, carolPubKey, firstWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val firstWithdrawTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(firstChangeOutput, firstWithdrawOutput))

    val firstWithdrawContext = ErgoLikeContext(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = firstWithdrawTx,
      self = depositOutput
    )

    val verifier = new ErgoLikeTestInterpreter

    val proofAliceWithdraw = alice.prove(spendEnv, script, firstWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext, proofAliceWithdraw, fakeMessage).get._1 shouldBe true

    val proofBobWithdraw = bob.prove(env, script, firstWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext, proofBobWithdraw, fakeMessage).get._1 shouldBe true

    val proofCarolWithdraw = carol.prove(env, script, firstWithdrawContext, fakeMessage).get.proof
    verifier.verify(env, script, firstWithdrawContext, proofCarolWithdraw, fakeMessage).get._1 shouldBe true

  }
}
