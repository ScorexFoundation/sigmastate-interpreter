package sigmastate.utxo

import sigmastate.{CAND, CAndUnproven, NodePosition, UnprovenSchnorr}
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

  property("setPositions") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage

    val parentPos = NodePosition(Seq(0,0))
    val child0 = UnprovenSchnorr(pk0, None, None, None, false, NodePosition(Seq(1)))
    val child1 = UnprovenSchnorr(pk1, None, None, None, false, NodePosition(Seq(0)))

    val c0 = CAndUnproven(CAND(Seq(pk0, pk1)), None, false, Seq(child0, child1), parentPos)
    val c1 = prover.setPositions(c0)

    c1.children.head.asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 0))

    c1.children(1).asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 1))
  }

}
