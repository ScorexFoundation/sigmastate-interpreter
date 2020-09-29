package sigmastate.utxo

import sigmastate.{NodePosition, CTHRESHOLD, COR, COrUnproven, CAndUnproven, UnprovenSchnorr, CAND, CThresholdUnproven}
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.FirstDLogProverMessage
import sigmastate.basics.{FirstDiffieHellmanTupleProverMessage, SecP256K1}
import sigmastate.eval.IRContextFactoryImpl
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}

class ProverSpecification extends SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext
  implicit lazy val irFactory = new IRContextFactoryImpl(IR)

  property("generateCommitments") {

    val prover = new ErgoLikeTestProvingInterpreter

    val pk: SigmaBoolean = prover.dlogSecrets.head.publicImage
    val pk2: SigmaBoolean = prover.dhSecrets.head.publicImage

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

    //DH
    val h3 = prover.generateCommitmentsFor(pk2, Seq(pk2))
    h3.hints.nonEmpty shouldBe true
    h3.realCommitments.head.position shouldBe NodePosition.CryptoTreePrefix
    h3.ownCommitments.head.position shouldBe NodePosition.CryptoTreePrefix

    h3.realCommitments.head.commitment shouldBe h3.ownCommitments.head.commitment

    h3.realCommitments.head.commitment.isInstanceOf[FirstDiffieHellmanTupleProverMessage] shouldBe true
  }

  property("setPositions - and") {
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

  property("setPositions - or") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage

    val parentPos = NodePosition(Seq(0,0))
    val child0 = UnprovenSchnorr(pk0, None, None, None, false, NodePosition(Seq(1)))
    val child1 = UnprovenSchnorr(pk1, None, None, None, false, NodePosition(Seq(0)))

    val c0 = COrUnproven(COR(Seq(pk0, pk1)), None, false, Seq(child0, child1), parentPos)
    val c1 = prover.setPositions(c0)

    c1.children.head.asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 0))

    c1.children(1).asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 1))
  }

  property("setPositions - threshold") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage

    val parentPos = NodePosition(Seq(0,0))
    val child0 = UnprovenSchnorr(pk0, None, None, None, false, NodePosition(Seq(1)))
    val child1 = UnprovenSchnorr(pk1, None, None, None, false, NodePosition(Seq(0)))

    val c0 = CThresholdUnproven(CTHRESHOLD(1, Seq(pk0, pk1)), None, false, 1, Seq(child0, child1), None, parentPos)
    val c1 = prover.setPositions(c0)

    c1.children.head.asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 0))

    c1.children(1).asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 1))
  }

}
