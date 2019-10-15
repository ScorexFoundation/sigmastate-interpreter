
package sigmastate.utxo.examples

import java.math.BigInteger

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import sigmastate.AvlTreeData
import sigmastate.Values.GroupElementConstant
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{DiffieHellmanTupleProverInput, ProveDHTuple}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._
import sigmastate.eval._

class DHTupleExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR = new TestingIRContext
  /**
    * let Alice's secret be x and Bob's be y
    *
    * Then
    * (g, g^y, g^x, g^xy) forms a DH tuple for Alice and
    * (g, g^x, g^y, g^xy) forms a DH tuple for Bob.
    *
    */
  property("Evaluation - DH Example") {
    import CryptoConstants.dlogGroup

    val g = dlogGroup.generator

    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage

    val x:BigInteger = alice.dlogSecrets.head.w // x is Alice's private key

    val g_x = alicePubKey.h // g_x is Alice's public key (g_x = g^x)

    val env = Map(
      ScriptNameProp -> "env",
      "g" -> g,
      "g_x" -> g_x
    )

    val script = compile(env,
      """{
        |  val g_y = OUTPUTS(0).R4[GroupElement].get
        |  val g_xy = OUTPUTS(0).R5[GroupElement].get
        |
        |  proveDHTuple(g, g_x, g_y, g_xy) || // for bob
        |  proveDHTuple(g, g_y, g_x, g_xy)    // for alice
        |}""".stripMargin
    ).asSigmaProp

    val inBox = ErgoBox(10, script, 50)

    // a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter

    val bob = new ContextEnrichingTestProvingInterpreter

    val y:BigInteger = bob.dlogSecrets.head.w // y is Bob's private key

   val bobPubKey: ProveDlog = bob.dlogSecrets.head.publicImage
   val g_y = GroupElementConstant(bobPubKey.h) // Bob's public key g^y

    val g_xy = GroupElementConstant(dlogGroup.exponentiate(g_x, y)) // g^xy

    val carol = new ContextEnrichingTestProvingInterpreter
    val carolPubKey:ProveDlog = carol.dlogSecrets.head.publicImage

    val outBox = ErgoBox(10, carolPubKey, 70, Nil,
      Map(
        R4 -> g_y,
        R5 -> g_xy
      )
    )

    val tx = createTransaction(IndexedSeq(outBox))

    val context = ErgoLikeContextTesting(
      currentHeight = 70,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inBox),
      spendingTransaction = tx,
      self = inBox
    )
    val dhtBob = DiffieHellmanTupleProverInput(y, ProveDHTuple(g, g_x, g_y, g_xy))

    val proofBob = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtBob)
    ).prove(env, script, context, fakeMessage).get.proof

    verifier.verify(env, script, context, proofBob, fakeMessage).get._1 shouldBe true

    val dhtAlice = DiffieHellmanTupleProverInput(x, ProveDHTuple(g, g_y, g_x, g_xy))

    val proofAlice = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtAlice)
    ).prove(env, script, context, fakeMessage).get.proof

    verifier.verify(env, script, context, proofAlice, fakeMessage).get._1 shouldBe true

    val dhtBad = DiffieHellmanTupleProverInput(BigInt(10).bigInteger, ProveDHTuple(g, g_y, g_x, g_xy))

    val proofBad = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtBad)
    ).prove(env, script, context, fakeMessage).get.proof

    verifier.verify(env, script, context, proofBad, fakeMessage).get._1 shouldBe false

  }

}
