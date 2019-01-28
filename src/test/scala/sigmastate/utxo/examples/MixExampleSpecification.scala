
package sigmastate.utxo.examples

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform._
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.Random
import sigmastate.Values.{ByteArrayConstant, ByteConstant, GroupElementConstant, IntConstant, SigmaPropConstant}
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.dlogGroup
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._
import sigmastate.utxo._

class MixExampleSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

  property("Evaluation - Mix Example") {
    import CryptoConstants.dlogGroup

    val g = dlogGroup.generator

    // Alice is first player, who initiates the mix
    val alice = new ErgoLikeTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage

    val x:BigInteger = alice.dlogSecrets.head.w // x is Alice's private key

    val u = alicePubKey.value // u is Alice's public key (u = g^x)
    // Alternative:
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
        |  proveDHTuple(g, u, e, f) // or f is u^y = g^xy
        |}""".stripMargin
    ).asBoolValue

    val halfMixEnv = Map(
      ScriptNameProp -> "halfMixEnv",
      "g" -> g,
      "u" -> u,
      "alicePubKey" -> alicePubKey,
      "fullMixScriptHash" -> Blake2b256(fullMixScript.bytes)
    )

    // Note that below script allows Alice to spend the half-mix output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-mix output
    // before some minimum height.

    // The proveDHTuple instruction takes parameters (g, h, u, v) where g, h are generators (discrete log bases)
    // with u = g^x and v = h^x. Note that y = log_g(h), where y is Bob's secret.

    val halfMixScript = compileWithCosting(halfMixEnv,
      """{
        |  // proveDlog(u) || {
        |  alicePubKey || {
        |    val c = OUTPUTS(0).R4[GroupElement].get
        |    val d = OUTPUTS(0).R5[GroupElement].get
        |
        |    OUTPUTS.size == 2 &&
        |    OUTPUTS(0).value == SELF.value &&
        |    OUTPUTS(1).value == SELF.value &&
        |    blake2b256(OUTPUTS(0).propositionBytes) == fullMixScriptHash &&
        |    blake2b256(OUTPUTS(1).propositionBytes) == fullMixScriptHash &&
        |    OUTPUTS(1).R4[GroupElement].get == d &&
        |    OUTPUTS(1).R5[GroupElement].get == c && {
        |      proveDHTuple(g, c, u, d) ||
        |      proveDHTuple(g, d, u, c)
        |    }
        |  }
        |}""".stripMargin
    ).asBoolValue


    /////////////////////////////////////////////////////////
    //// Alice starts creating a Half-Mix
    /////////////////////////////////////////////////////////

    // she creates a transaction that outputs a box with halfGame script.
    // In the example, we don't create the transaction; we just create a box below

    val halfMixCreationHeight = 70
    val mixAmount = 10

    val halfMixOutput = ErgoBox(mixAmount, halfMixScript, halfMixCreationHeight)

    // above halMixOutput is a Half-Mix "box" created by Alice.

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter
    /////////////////////////////////////////////////////////
    //// Scenario 1. Alice aborts the mix before full mix is created
    /////////////////////////////////////////////////////////

    // Alice pays to Carol. Protocol ends here
    val carol = new ErgoLikeTestProvingInterpreter
    val carolPubKey:ProveDlog = carol.dlogSecrets.head.publicImage

    val abortHalfMixHeight = halfMixCreationHeight + 10 // can be anything

    val abortHalfMixOutput = ErgoBox(mixAmount, carolPubKey, abortHalfMixHeight, Nil,
      Map(
        R4 -> GroupElementConstant(g), // dummy data. Has to be given, even though not needed as per halfGameScript
        R5 -> GroupElementConstant(g) // dummy statement
      )
    )

//    val abortHalfMixOutput2 = ErgoBox(mixAmount, carolPubKey, abortHalfMixHeight, Nil,
//      Map(
//        R4 -> GroupElementConstant(g), // dummy data. Has to be given, even though not needed as per halfGameScript
//        R5 -> GroupElementConstant(g) // dummy statement
//      )
//    )

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val abortHalfMixTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(abortHalfMixOutput, abortHalfMixOutput))
    val abortHalfMixTxBad = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(abortHalfMixOutput))

    val abortHalfMixContext = ErgoLikeContext(
      currentHeight = abortHalfMixHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(halfMixOutput),
      spendingTransaction = abortHalfMixTx,
      self = halfMixOutput // what is the use of self?
    )
    val abortHalfMixContextBad = ErgoLikeContext(
      currentHeight = abortHalfMixHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(halfMixOutput),
      spendingTransaction = abortHalfMixTxBad,
      self = halfMixOutput // what is the use of self?
    )

    val proofAbortHalfMix = alice.prove(halfMixEnv, halfMixScript, abortHalfMixContext, fakeMessage).get.proof // works
    val proofAbortHalfMixBad = alice.prove(halfMixEnv, halfMixScript, abortHalfMixContextBad, fakeMessage).get.proof // gives error

    verifier.verify(halfMixEnv, halfMixScript, abortHalfMixContext, proofAbortHalfMix, fakeMessage).get._1 shouldBe true



    /////////////////////////////////////////////////////////
    //// Scenario 2. Bob accepts Alice's
    /////////////////////////////////////////////////////////


//    val bob = new ErgoLikeTestProvingInterpreter

  }

}
