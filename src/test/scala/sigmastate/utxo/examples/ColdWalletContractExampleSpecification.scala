package sigmastate.utxo.examples

import org.ergoplatform._
import sigmastate.Values.IntConstant
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.lang.Terms._


class ColdWalletContractExampleSpecification extends SigmaTestingCommons {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  property("Evaluation - ColdWallet Contract Example") {

    val alice = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val env = Map(
      ScriptNameProp -> "env",
      "alice" -> alicePubKey,
      "bob" -> bobPubKey,
      "blocksIn24h" -> IntConstant(500),
      "percent" -> IntConstant(1),
      "minSpend" -> IntConstant(100)
    )

    val script = compileWithCosting(env,
      """{
        |  val r4 = SELF.R4[Int].get // height at which period started
        |  val min = SELF.R5[Long].get // min Balance needed in this period
        |  val depth = HEIGHT - SELF.creationInfo._1
        |  val start = if (depth < r4) depth else r4
        |  val notExpired = HEIGHT - start <= blocksIn24h
        |
        |  val newStart:Int = if (notExpired) start else HEIGHT
        |  val toKeep:Long = SELF.value - SELF.value * percent / 100
        |  val newMin:Long = if (notExpired) min else {if (toKeep > minSpend) toKeep else 0L}
        |
        |  val isValid = {(out:Box) =>
        |    if (newMin == 0) true else {
        |      out.propositionBytes == SELF.propositionBytes &&
        |      out.value >= newMin &&
        |      out.R4[Int].get >= newStart &&
        |      out.R5[Long].get == newMin
        |    }
        |  }
        |  (alice && bob) || ((alice || bob) && OUTPUTS.exists(isValid))}""".stripMargin).asSigmaProp

    val address = Pay2SHAddress(script)

    val carol = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val carolPubKey = carol.dlogSecrets.head.publicImage

  }
}
