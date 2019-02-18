package sigmastate.utxo.examples

import sigmastate.interpreter.Interpreter.ScriptNameProp
import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{IntConstant, SigmaPropConstant}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.utxo._


class ReversibleTxExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  /**
    * Reversible Transaction example.
    *
    * Often lack of reversible payments is considered a drawback in Bitcoin. ErgoScript allows us to easily design
    * reversible payments.
    *
    * assume Alice (alicePubKey) is the (hot) wallet of mining pool or an exchange.
    *
    * Bob (bobPubKey) is an account holder with the pool or exchange. He may legitimately withdraw funds anytime.
    * The funds will be sent from a box controlled by alicePubKey. This is the normal scenario
    *
    * However, since alicePubKey is from a hot wallet, it could get compromised. Once compromised, any funds sent
    * from alicePubKey to some bobPubKey are not normal withdrawals and should be invalidated (aborted), provided
    * that the breach is discovered within a certain time (example 24 hours).
    *
    * In the abort scenario, we require that all withdraws are reversed and those funds are sent to a different
    * secure address (unrelated to alicePubKey).
    *
    * The abort scenario would (and should) be performed by a key different from alicePubKey
    * Assume that abort can be done by Carol (carolPubKey) only.
    *
    * The protocol is as follows:
    * Alice creates a script encoding the "reversible" logic. Lets call this the withdrawScript
    *
    * She then creates a wallet address using a script called walletScript, which requires that the
    * spending condition generate a single box protected by withdrawScript
    *
    */
  property("Evaluation - Reversible Tx Example") {

    val alice = new ErgoLikeTestProvingInterpreter
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ErgoLikeTestProvingInterpreter
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val carol = new ErgoLikeTestProvingInterpreter
    val carolPubKey = carol.dlogSecrets.head.publicImage

    val withdrawEnv = Map(
      ScriptNameProp -> "withdrawEnv",
      "carolPubKey" -> carolPubKey // this pub key can reverse payments
    )

    val withdrawScript = compileWithCosting(withdrawEnv,
      """{
        |  val bobPubKey   = SELF.R4[SigmaProp].get     // Bob's key (or script) that Alice sent money to
        |  val bobDeadline = SELF.R5[Int].get           // after this height, Bob gets to spend unconditionally
        |
        |  (bobPubKey && HEIGHT > bobDeadline) ||
        |  (carolPubKey && HEIGHT <= bobDeadline)       // carolPubKey hardwired via withdrawEnv
        |}""".stripMargin).asBoolValue

    val walletEnv = Map(
      ScriptNameProp -> "walletEnv",
      "alicePubKey" -> alicePubKey,
      "withdrawScriptHash" -> Blake2b256(withdrawScript.bytes)
    )

    val walletScript = compileWithCosting(walletEnv,
      """{
        |  alicePubKey &&
        |  OUTPUTS.size == 1 &&
        |  blake2b256(OUTPUTS(0).propositionBytes) == withdrawScriptHash &&
        |  OUTPUTS(0).R5[Int].get >= HEIGHT + 30       // bobDeadline stored in R5. after this height, Bob gets to spend unconditionally
        |}""".stripMargin
    ).asBoolValue

    val walletAddress = Pay2SHAddress(walletScript)
    // The above is a "reversible wallet" address.
    // Payments sent from this wallet are all reversible for a certain time

    val depositAmount = 10
    val depositHeight = 100

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below


    val depositOutput = ErgoBox(depositAmount, walletAddress.script, depositHeight)

    // Now Alice wants to give Bob some amount from the wallet in a "reversible" way.

    val withdrawAmount = 10
    val withdrawHeight = 101
    val bobDeadline = 150

    val reversibleWithdrawOutput = ErgoBox(withdrawAmount, withdrawScript, withdrawHeight, Nil,
      Map(
        R4 -> SigmaPropConstant(bobPubKey),
        R5 -> IntConstant(bobDeadline)
      )
    )

    //normally this transaction would be invalid (why?), but we're not checking it in this test
    val withdrawTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(reversibleWithdrawOutput))

    val withdrawContext = ErgoLikeContext(
      currentHeight = withdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTx,
      self = depositOutput
    )

    val proofWithdraw = alice.prove(walletEnv, walletScript, withdrawContext, fakeMessage).get.proof

    val verifier = new ErgoLikeTestInterpreter

    verifier.verify(walletEnv, walletScript, withdrawContext, proofWithdraw, fakeMessage).get._1 shouldBe true

    // Possibility 1: Normal scenario
    // Bob spends after bobDeadline. He sends to Dave

    val dave = new ErgoLikeTestProvingInterpreter
    val davePubKey = dave.dlogSecrets.head.publicImage

    val bobSpendAmount = 10
    val bobSpendHeight = 151

    val bobSpendOutput = ErgoBox(bobSpendAmount, davePubKey, bobSpendHeight)

    //normally this transaction would be invalid (why?), but we're not checking it in this test
    val bobSpendTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(bobSpendOutput))

    // val fakeSelf: ErgoBox = createBox(0, TrueLeaf)

    val bobSpendContext = ErgoLikeContext(
      currentHeight = bobSpendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(reversibleWithdrawOutput),
      spendingTransaction = bobSpendTx,
      self = reversibleWithdrawOutput
    )

    val proofBobSpend = bob.prove(withdrawEnv, withdrawScript, bobSpendContext, fakeMessage).get.proof

    verifier.verify(withdrawEnv, withdrawScript, bobSpendContext, proofBobSpend, fakeMessage).get._1 shouldBe true

    // Possibility 2: Abort scenario
    // carol spends before bobDeadline

    val carolSpendAmount = 10
    val carolSpendHeight = 131

    // Carol sends to Dave
    val carolSpendOutput = ErgoBox(carolSpendAmount, davePubKey, carolSpendHeight)

    //normally this transaction would be invalid (why?), but we're not checking it in this test
    val carolSpendTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(carolSpendOutput))

    // val fakeSelf: ErgoBox = createBox(0, TrueLeaf)

    val carolSpendContext = ErgoLikeContext(
      currentHeight = carolSpendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(reversibleWithdrawOutput),
      spendingTransaction = carolSpendTx,
      self = reversibleWithdrawOutput
    )

    val proofCarolSpend = carol.prove(withdrawEnv, withdrawScript, carolSpendContext, fakeMessage).get.proof

    verifier.verify(withdrawEnv, withdrawScript, carolSpendContext, proofCarolSpend, fakeMessage).get._1 shouldBe true
  }

}
