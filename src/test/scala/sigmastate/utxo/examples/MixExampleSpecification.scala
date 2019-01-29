
package sigmastate.utxo.examples

import java.math.BigInteger

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.Values.{ByteConstant, GroupElementConstant, IntConstant, SigmaPropConstant}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter

class MixExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR = new TestingIRContext

  property("Evaluation - Mix Example") {
    import CryptoConstants.dlogGroup

    val g = dlogGroup.generator

    // Alice is first player, who initiates the mix
    val alice = new ErgoLikeTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage

    val x:BigInteger = alice.dlogSecrets.head.w // x is Alice's private key

    val u = alicePubKey.h // u is Alice's public key (u = g^x)
    // Alternative 1:
    //      val u = alicePubKey.value // u is Alice's public key (u = g^x)
    // Alternative 2:
    //    Generate x ourselves (e.g., val x = BigInt(randomBytes).bigInteger)
    //    To generate u from a BigInteger x, and to generate alicePubKey as ProveDlog type from u, use the following:
    //      val u:EcPointType = dlogGroup.exponentiate(dlogGroup.generator, x)
    //      val alicePubKey:ProveDlog = ProveDlog(u)

    val fullMixEnv = Map(
      ScriptNameProp -> "fullMixEnv",
      "g" -> g,
      "u" -> u
    )

    ProveDlog(u) shouldBe alicePubKey

    // y is Bob's secret key and h = g^y is kind of like his "public key"
    // The Diffie-Hellman solution is v = h^x = u^y = g^xy.
    val fullMixScript = compileWithCosting(fullMixEnv,
      """{
        |  val e = SELF.R4[GroupElement].get
        |  val f = SELF.R5[GroupElement].get
        |  proveDlog(f) ||          // either f is g^y
        |  proveDHTuple(g, e, u, f) // or f is u^y = g^xy
        |}""".stripMargin
    ).asBoolValue

    val halfMixEnv = Map(
      ScriptNameProp -> "halfMixEnv",
      "g" -> g,
      "u" -> u,
      "fullMixScriptHash" -> Blake2b256(fullMixScript.bytes)
    )

    // Note that below script allows Alice to spend the half-mix output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-mix output
    // before some minimum height.

    // The proveDHTuple instruction takes parameters (g, h, u, v) where g, h are generators (discrete log bases)
    // with u = g^x and v = h^x. Note that y = log_g(h), where y is Bob's secret.

    val halfMixScript = compileWithCosting(halfMixEnv,
      """{
        |  val h = OUTPUTS(0).R4[GroupElement].get
        |  val v = OUTPUTS(0).R5[GroupElement].get
        |
        |  proveDHTuple(g, u, h, v)
        |}""".stripMargin
    ).asBoolValue

    val halfMixScriptActual = compileWithCosting(halfMixEnv,
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
        |    proveDHTuple(g, c, u, d) ||
        |    proveDHTuple(g, d, u, c)
        |  }
        |}""".stripMargin
    ).asBoolValue


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

    val bob = new ErgoLikeTestProvingInterpreter
    val bobPubKey:ProveDlog = bob.dlogSecrets.head.publicImage

    val y:BigInteger = bob.dlogSecrets.head.w // y is Bob's private key

    val h:GroupElementConstant = GroupElementConstant(bobPubKey.h) // g^y
    val hAlt = GroupElementConstant(dlogGroup.exponentiate(g, y))

    h shouldBe hAlt

    val v:GroupElementConstant = GroupElementConstant(dlogGroup.exponentiate(u, y))
    val vAlt = GroupElementConstant(dlogGroup.exponentiate(h, x))

    v shouldBe vAlt

    val randomBit = scala.util.Random.nextBoolean
    // randomBit is interpreted as follows
    //     0 is false
    //     1 is true

    val (c0, c1) = if (randomBit) (v, h) else (h, v)

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
    val fullMixTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(fullMixOutput0, fullMixOutput1))

    val fullMixContext = ErgoLikeContext(
      currentHeight = fullMixCreationHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(halfMixOutput),
      spendingTransaction = fullMixTx,
      self = halfMixOutput
    )

    // bob (2nd player) is generating a proof and it is passing verification
    val proofFullMix = bob.prove(halfMixEnv, halfMixScript, fullMixContext, fakeMessage).get.proof

//    verifier.verify(halfMixEnv, halfMixScript, fullMixContext, proofFullMix, fakeMessage).get._1 shouldBe true

  }

}