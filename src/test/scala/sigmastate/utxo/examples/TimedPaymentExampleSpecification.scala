package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform._
import sigmastate.Values.{ByteArrayConstant, ByteConstant, IntConstant}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.utxo._


class TimedPaymentExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  property("Evaluation - Timed payment Tx Example") {

    val alice = new ErgoLikeTestProvingInterpreter // customer at coffee shop
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ErgoLikeTestProvingInterpreter // owner of coffee shop (or payment address of coffee shop)
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val env = Map(
      ScriptNameProp -> "env",
      "alice" -> alicePubKey
    )

    val script = compileWithCosting(env,
      """{ alice && HEIGHT <= getVar[Int](1).get }""".stripMargin
    ).asSigmaProp

    val address = Pay2SHAddress(script)
    // The above is a "timed address".
    // Payments sent from this wallet are must be confirmed within a certain height (given in the first output's R4)

    val depositAmount = 10
    val depositHeight = 50

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below


    val depositOutput = ErgoBox(depositAmount, address.script, depositHeight)

    // Now Alice wants to give Bob (coffee shop owner) some amount from the wallet in a "timed" way.

    val withdrawAmount = 10
    val withdrawHeight = 100
    val confDeadline = 110

    val timedWithdrawOutput = ErgoBox(withdrawAmount, bobPubKey, withdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val withdrawTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(timedWithdrawOutput))

    val withdrawContext = ErgoLikeContext(
      currentHeight = 109,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTx,
      self = depositOutput,
      ContextExtension(Map(1.toByte -> IntConstant(confDeadline)))
    )

    val proofWithdraw = alice.prove(env, script, withdrawContext, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter

    verifier.verify(env, script, withdrawContext, proofWithdraw, fakeMessage).get._1 shouldBe true

    val withdrawContextBad = ErgoLikeContext(
      currentHeight = 111,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTx,
      self = depositOutput,
      ContextExtension(Map(1.toByte -> IntConstant(confDeadline - 20)))
    )

    an[InterpreterException] should be thrownBy (alice.prove(env, script, withdrawContextBad, fakeMessage).get.proof)

    verifier.verify(env, script, withdrawContextBad, proofWithdraw, fakeMessage).get._1 shouldBe false
  }
}
