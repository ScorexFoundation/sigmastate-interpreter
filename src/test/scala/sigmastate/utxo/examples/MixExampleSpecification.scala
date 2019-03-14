
package sigmastate.utxo.examples

import java.math.BigInteger

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.Values.{ByteConstant, GroupElementConstant, IntConstant, SigmaPropConstant}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{DiffieHellmanTupleProverInput, ProveDHTuple}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._

class MixExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR = new TestingIRContext

  property("Evaluation - Mix Example") {
    import CryptoConstants.dlogGroup

    val g = dlogGroup.generator

    // Alice is first player, who initiates the mix
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage

    val x:BigInteger = alice.dlogSecrets.head.w // x is Alice's private key

    val g_x = alicePubKey.h // g_x is Alice's public key (g_x = g^x)
    // Alternative 1:
    //      val g_x = alicePubKey.value
    // Alternative 2:
    //    Generate x ourselves (e.g., val x = BigInt(randomBytes).bigInteger)
    //    To generate g_x from a BigInteger x, and to generate alicePubKey as ProveDlog type from g_x, use the following:
    //      val g_x:EcPointType = dlogGroup.exponentiate(g, x)
    //      val alicePubKey:ProveDlog = ProveDlog(g_x)

    val fullMixEnv = Map(
      ScriptNameProp -> "fullMixEnv",
      "g" -> g,
      "g_x" -> g_x
    )

    ProveDlog(g_x) shouldBe alicePubKey

    // y is Bob's secret key and h = g^y is kind of like his "public key"
    // The Diffie-Hellman solution is g_xy = g_y^x = g_x^y = g^xy.
    val fullMixScript = compile(fullMixEnv,
      """{
        |  val e = SELF.R4[GroupElement].get
        |  val f = SELF.R5[GroupElement].get
        |  proveDlog(f) ||            // either f is g^y
        |  proveDHTuple(g, e, g_x, f) // or f is u^y = g^xy
        |}""".stripMargin
    ).asSigmaProp

    val halfMixEnv = Map(
      ScriptNameProp -> "halfMixEnv",
      "g" -> g,
      "g_x" -> g_x,
      "fullMixScriptHash" -> Blake2b256(fullMixScript.bytes)
    )

    // Note that below script allows Alice to spend the half-mix output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-mix output
    // before some minimum height.

    // The proveDHTuple instruction takes parameters (g, h, u, v) where g, h are generators (discrete log bases)
    // with u = g^x and v = h^x. Note that y = log_g(h), where y is Bob's secret.

    val halfMixScript = compile(halfMixEnv,
      """{
        |  val c = OUTPUTS(0).R4[GroupElement].get
        |  val d = OUTPUTS(0).R5[GroupElement].get
        |
        |  OUTPUTS.size == 2 &&
        |  OUTPUTS(0).value == SELF.value &&
        |  OUTPUTS(1).value == SELF.value &&
        |  blake2b256(OUTPUTS(0).propositionBytes) == fullMixScriptHash &&
        |  blake2b256(OUTPUTS(1).propositionBytes) == fullMixScriptHash &&
        |  OUTPUTS(1).R4[GroupElement].get == d &&
        |  OUTPUTS(1).R5[GroupElement].get == c && {
        |    proveDHTuple(g, g_x, c, d) ||
        |    proveDHTuple(g, g_x, d, c)
        |  }
        |}""".stripMargin
    ).asSigmaProp


    /////////////////////////////////////////////////////////
    //// Alice starts creating a Half-Mix box
    /////////////////////////////////////////////////////////

    // she creates a transaction that outputs a box with halfGame script.
    // In the example, we don't create the transaction; we just create a box below

    val halfMixCreationHeight = 70
    val mixAmount = 10

    val halfMixOutput = ErgoBox(mixAmount, halfMixScript, halfMixCreationHeight)
    // above halMixOutput is a Half-Mix box created by Alice.

    // a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter

    /////////////////////////////////////////////////////////
    //// Bob picks Alice's Half-Mix box and creates a Full-Mix box
    ///////////////////////////////////////////////////////////

    // If Alice wants to abort the mix, she can take Bob's role and spend her Half-Mix output

    val bob = new ContextEnrichingTestProvingInterpreter
    val bobPubKey:ProveDlog = bob.dlogSecrets.head.publicImage

    val y:BigInteger = bob.dlogSecrets.head.w // y is Bob's private key

    val g_y = GroupElementConstant(bobPubKey.h) // g^y
    val g_y_alt = GroupElementConstant(dlogGroup.exponentiate(g, y))

    g_y shouldBe g_y_alt

    // To Do: Extract below g_x from halfMixOutput
    val g_xy = GroupElementConstant(dlogGroup.exponentiate(g_x, y))
    val g_xy_alt = GroupElementConstant(dlogGroup.exponentiate(g_y, x))

    g_xy shouldBe g_xy_alt

    val randomBit = scala.util.Random.nextBoolean
    // randomBit is interpreted as follows
    //     0 is false
    //     1 is true

    val (c0, c1) = if (randomBit) (g_xy, g_y) else (g_y, g_xy)

    val fullMixCreationHeight = 80

    // if randomBit is 0 (i.e., false) below box is spendable by Alice, else by Bob
    val fullMixOutput0 = ErgoBox(mixAmount, fullMixScript, fullMixCreationHeight, Nil,
      Map(
        R4 -> c0,
        R5 -> c1
      )
    )

    // if randomBit is 1 (i.e., true) below box is spendable by Alice, else by Bob
    val fullMixOutput1 = ErgoBox(mixAmount, fullMixScript, fullMixCreationHeight, Nil,
      Map(
        R4 -> c1,
        R5 -> c0
      )
    )

    // normally this transaction would be invalid, but we're not checking it in this test
    val fullMixTx = createTransaction(IndexedSeq(fullMixOutput0, fullMixOutput1))

    val fullMixContext = ErgoLikeContext(
      currentHeight = fullMixCreationHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(halfMixOutput),
      spendingTransaction = fullMixTx,
      self = halfMixOutput
    )

    // bob (2nd player) is generating a proof and it is passing verification
    // To Do: Extract below g_x from halfMixOutput
    val dhtBob = DiffieHellmanTupleProverInput(y, ProveDHTuple(g, g_x, g_y, g_xy))

    val proofFullMix = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtBob)
    ).prove(halfMixEnv, halfMixScript, fullMixContext, fakeMessage).get.proof

    verifier.verify(halfMixEnv, halfMixScript, fullMixContext, proofFullMix, fakeMessage).get._1 shouldBe true

    //////////////////////////////////////////////
    //// Setup for spending the above created outputs (fullMixOutput0, fullMixOutput1)
    //////////////////////////////////////////////

    // some 3rd person that will be paid
    val carol = new ContextEnrichingTestProvingInterpreter
    val carolPubKey:ProveDlog = carol.dlogSecrets.head.publicImage

    val spendHeight = 90
    val carolOutput = ErgoBox(mixAmount, carolPubKey, spendHeight)

    // normally this transaction would be invalid, but we're not checking it in this test
    val spendingTx = createTransaction(carolOutput)

    //////////////////////////////////////////////
    //// Alice spending her output
    //////////////////////////////////////////////

    val fullMixOutput0_R4 = fullMixOutput0.additionalRegisters(R4).v
    val fullMixOutput0_R5 = fullMixOutput0.additionalRegisters(R5).v

    fullMixOutput0_R4 shouldBe c0
    fullMixOutput0_R5 shouldBe c1

    val r4_x = dlogGroup.exponentiate(fullMixOutput0_R4.asInstanceOf[GroupElementConstant], x) // R4^x

    // if R4^x == R5 then this fullMixOutput0 is Alice's output else its Bob's output.
    val (aliceAnonBox, bobAnonBox) = if (r4_x == fullMixOutput0_R5.asInstanceOf[GroupElementConstant].value) {
      println("First output is Alice's")
      (fullMixOutput0, fullMixOutput1)
    } else {
      println("First output is Bob's")
      dlogGroup.exponentiate(fullMixOutput0_R5.asInstanceOf[GroupElementConstant], x) shouldBe fullMixOutput0_R4.asInstanceOf[GroupElementConstant].value
      (fullMixOutput1, fullMixOutput0)
    }

    val aliceSpendContext = ErgoLikeContext(
      currentHeight = spendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(aliceAnonBox),
      spendingTransaction = spendingTx,
      self = aliceAnonBox
    )

    // To Do: Extract below g_y, g_xy from fullMixOutputs registers
    val dhtAlice = DiffieHellmanTupleProverInput(x, ProveDHTuple(g, g_y, g_x, g_xy))

    val proofAliceSpend = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtAlice)
    ).prove(fullMixEnv, fullMixScript, aliceSpendContext, fakeMessage).get.proof

    verifier.verify(fullMixEnv, fullMixScript, aliceSpendContext, proofAliceSpend, fakeMessage).get._1 shouldBe true

    //////////////////////////////////////////////
    //// Bob spending his output
    //////////////////////////////////////////////

    val bobSpendContext = ErgoLikeContext(
      currentHeight = spendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(bobAnonBox),
      spendingTransaction = spendingTx,
      self = bobAnonBox
    )

    val proofBobSpend = bob.prove(fullMixEnv, fullMixScript, bobSpendContext, fakeMessage).get.proof

    verifier.verify(fullMixEnv, fullMixScript, bobSpendContext, proofBobSpend, fakeMessage).get._1 shouldBe true

  }

}