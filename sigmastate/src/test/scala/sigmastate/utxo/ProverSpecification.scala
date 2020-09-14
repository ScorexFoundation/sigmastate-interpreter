package sigmastate.utxo

import sigmastate.{CAND, NodePosition}
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.FirstDLogProverMessage
import sigmastate.basics.SecP256K1
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}

class ProverSpecification extends SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("generateCommitments") {

    val prover = new ErgoLikeTestProvingInterpreter

    val pk: SigmaBoolean = prover.publicKeys(0)
    val pk2: SigmaBoolean = prover.publicKeys(1)

    prover.generateCommitmentsFor(pk, Seq(pk2)).hints.isEmpty shouldBe true

    val pk3 = CAND(Seq(pk, pk2))
    prover.generateCommitmentsFor(pk, Seq(pk3)).hints.isEmpty shouldBe true
    prover.generateCommitmentsFor(pk3, Seq(pk3)).hints.isEmpty shouldBe true

    val h = prover.generateCommitmentsFor(pk, Seq(pk))
    h.hints.nonEmpty shouldBe true
    h.realCommitments.head.position shouldBe NodePosition.CryptoTreePrefix
    h.ownCommitments.head.position shouldBe NodePosition.CryptoTreePrefix

    h.realCommitments.head.commitment shouldBe h.ownCommitments.head.commitment

    val a = h.realCommitments.head.commitment.asInstanceOf[FirstDLogProverMessage]
    val r = h.ownCommitments.head.secretRandomness

    // g^r == a
    SecP256K1.exponentiate(SecP256K1.generator, r) shouldBe a.ecData

    val h2 = prover.generateCommitmentsFor(pk3, Seq(pk))
    h2.hints.size shouldBe 2

    h2.realCommitments.head.position shouldBe NodePosition(Seq(0,0))
    h2.ownCommitments.head.position shouldBe NodePosition(Seq(0,0))
  }

}
