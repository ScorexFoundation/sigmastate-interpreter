
package sigmastate.utxo.examples

import java.math.BigInteger

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import sigmastate.AvlTreeData
import sigmastate.Values.GroupElementConstant
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoLikeTestInterpreter

class DHTupleExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR = new TestingIRContext

  property("Evaluation - DH Example") {
    import CryptoConstants.dlogGroup

    val g = dlogGroup.generator

    val alice = new ErgoLikeTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage

    val u = alicePubKey.h // u is Alice's public key (u = g^x)

    val aliceEnv = Map(
      ScriptNameProp -> "aliceEnv",
      "g" -> g,
      "u" -> u
    )

    val aliceScript = compileWithCosting(aliceEnv,
      """{
        |  val h = OUTPUTS(0).R4[GroupElement].get
        |  val v = OUTPUTS(0).R5[GroupElement].get
        |
        |  proveDHTuple(g, u, h, v)
        |}""".stripMargin
    ).asBoolValue

    val aliceOutput = ErgoBox(10, aliceScript, 50)

    // a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter

    val bob = new ErgoLikeTestProvingInterpreter
    val bobPubKey:ProveDlog = bob.dlogSecrets.head.publicImage

    val y:BigInteger = bob.dlogSecrets.head.w // y is Bob's private key

    val h = GroupElementConstant(bobPubKey.h) // g^y

    val v = GroupElementConstant(dlogGroup.exponentiate(u, y)) // g^xy

    val output = ErgoBox(10, bobPubKey, 70, Nil,
      Map(
        R4 -> h,
        R5 -> v
      )
    )

    val bobTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(output))

    val bobContext = ErgoLikeContext(
      currentHeight = 70,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(aliceOutput),
      spendingTransaction = bobTx,
      self = aliceOutput
    )

    // bob (2nd player) is generating a proof and it is passing verification
    val proofBob = bob.prove(aliceEnv, aliceScript, bobContext, fakeMessage).get.proof

    verifier.verify(aliceEnv, aliceScript, bobContext, proofBob, fakeMessage).get._1 shouldBe true

  }

}