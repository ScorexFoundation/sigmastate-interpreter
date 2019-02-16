package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{IntConstant, SigmaPropConstant}
import sigmastate._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
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
    * Use-case:
    *
    *  Consider the hot-wallet of a mining pool or an exchange. Funds withdrawn by customers originate from this hot-wallet.
    *
    *  Since its a hot-wallet, its private key can get compromised and unauthorized withdraws can occur.
    *
    *  We want to ensure that in the event of such a compromise, we are able to "save" all funds stored in this wallet by
    *  moving them to a secure address, provided that the breach is discovered within 24 hours of the first unauthorized withdraw.
    *
    *  In order to achieve this, we require that all coins sent via the hot-wallet (both legitimate and by the attacker)
    *  have a 24 hour cooling off period, during which the created UTXOs are "locked" and can only be spent by a trusted
    *  private key (which is different from the hot-wallet private key)
    *  Once this period is over, those coins become normal and can only be spent by the customer who withdrew.
    *
    *  This is achieved by storing the hot-wallet funds in a <b>Reversible Address</b>, a special type of address.
    *
    *  The reversible address is a P2SH address created using a script that encodes our spending condition.
    *  The script requires that any UTXO created by spending this box can only be spent by the trusted party during the
    *  locking period. Thus, all funds sent from such addresses have a temporary lock.
    *
    *  Note that reversible addresses are designed for storing large amount of funds needed for automated withdraws
    *  (such as an exchange hot-wallet). They are NOT designed for storing funds for personal use (such as paying for a coffee).
    *
    *  We use the following notation in the code:
    *
    *  Alice is the hot-wallet with public key alicePubKey
    *
    *  Bob with public key bobPubKey is a customer withdrawing from Alice. This is the normal scenario
    *
    *  Carol with public key carolPubKey is the trusted party who can spend during the locking period (i.e., she can reverse payments)
    *
    *  Once alicePubKey is compromised (i.e., a transaction spending from this key is found to be unauthorized), an "abort procedure"
    *  is triggered. After this, all locked UTXOs sent from alicePubKey are suspect and should be aborted by Carol.
    *
    *  A reversible address is created by Alice as follows:
    *
    *  1. Alice creates a script encoding the "reversible" logic. Lets call this the withdrawScript
    *  2. She then creates a script called depositScript which requires that all created boxes be protected by withdrawScript.
    *  3. She a deposit a P2SH address for topping up the hot-wallet using depositScript.
    *
    */
  property("Evaluation - Reversible Tx Example") {

    val alice = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ContextEnrichingTestProvingInterpreter // private key of customer whose withdraws are sent from hot-wallet
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val carol = new ContextEnrichingTestProvingInterpreter // private key of trusted party who can abort withdraws
    val carolPubKey = carol.dlogSecrets.head.publicImage

    val withdrawEnv = Map(
      ScriptNameProp -> "withdrawEnv",
      "carol" -> carolPubKey // this pub key can reverse payments
    )

    val withdrawScript = compile(withdrawEnv,
      """{
        |  val bob         = SELF.R4[SigmaProp].get     // Bob's key (or script) that Alice sent money to
        |  val bobDeadline = SELF.R5[Int].get           // after this height, Bob gets to spend unconditionally
        |
        |  (bob && HEIGHT > bobDeadline) || (carol && HEIGHT <= bobDeadline)
        |}""".stripMargin).asSigmaProp

    val depositEnv = Map(
      ScriptNameProp -> "depositEnv",
      "alice" -> alicePubKey,
      "withdrawScriptHash" -> Blake2b256(withdrawScript.bytes)
    )

    val depositScript = compile(depositEnv,
      """{
        |  alice && OUTPUTS.forall({(out:Box) =>
        |    out.R5[Int].get >= HEIGHT + 30 &&
        |    blake2b256(out.propositionBytes) == withdrawScriptHash
        |  })
        |}""".stripMargin
    ).asSigmaProp
    // Note: in above bobDeadline is stored in R5. After this height, Bob gets to spend unconditionally

    val depositAddress = Pay2SHAddress(depositScript)
    // The above is a "reversible wallet" address.
    // Payments sent from this wallet are all reversible for a certain time

    val depositAmount = 10
    val depositHeight = 100

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below


    val depositOutput = ErgoBox(depositAmount, depositAddress.script, depositHeight)

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
    val withdrawTx = createTransaction(reversibleWithdrawOutput)

    val withdrawContext = ErgoLikeContext(
      currentHeight = withdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTx,
      self = depositOutput
    )

    val proofWithdraw = alice.prove(depositEnv, depositScript, withdrawContext, fakeMessage).get.proof

    val verifier = new ErgoLikeTestInterpreter

    verifier.verify(depositEnv, depositScript, withdrawContext, proofWithdraw, fakeMessage).get._1 shouldBe true

    // Possibility 1: Normal scenario
    // Bob spends after bobDeadline. He sends to Dave

    val dave = new ContextEnrichingTestProvingInterpreter
    val davePubKey = dave.dlogSecrets.head.publicImage

    val bobSpendAmount = 10
    val bobSpendHeight = 151

    val bobSpendOutput = ErgoBox(bobSpendAmount, davePubKey, bobSpendHeight)

    //normally this transaction would be invalid (why?), but we're not checking it in this test
    val bobSpendTx = createTransaction(bobSpendOutput)

    val bobSpendContext = ErgoLikeContext(
      currentHeight = bobSpendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(reversibleWithdrawOutput),
      spendingTransaction = bobSpendTx,
      self = reversibleWithdrawOutput
    )

    val spendEnv = Map(ScriptNameProp -> "spendEnv")

    val proofBobSpend = bob.prove(spendEnv, withdrawScript, bobSpendContext, fakeMessage).get.proof

    verifier.verify(spendEnv, withdrawScript, bobSpendContext, proofBobSpend, fakeMessage).get._1 shouldBe true

    // Possibility 2: Abort scenario
    // carol spends before bobDeadline

    val carolSpendAmount = 10
    val carolSpendHeight = 131

    // Carol sends to Dave
    val carolSpendOutput = ErgoBox(carolSpendAmount, davePubKey, carolSpendHeight)

    //normally this transaction would be invalid (why?), but we're not checking it in this test
    val carolSpendTx = createTransaction(carolSpendOutput)

    val carolSpendContext = ErgoLikeContext(
      currentHeight = carolSpendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(reversibleWithdrawOutput),
      spendingTransaction = carolSpendTx,
      self = reversibleWithdrawOutput
    )

    val proofCarolSpend = carol.prove(spendEnv, withdrawScript, carolSpendContext, fakeMessage).get.proof

    verifier.verify(spendEnv, withdrawScript, carolSpendContext, proofCarolSpend, fakeMessage).get._1 shouldBe true

  }
}
