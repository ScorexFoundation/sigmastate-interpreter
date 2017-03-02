package sigmastate

import java.math.BigInteger

import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{Secret, SecretCompanion}
import scorex.crypto.encode.Base58
import DLogProposition._
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData}
import scapi.sigma.rework.Challenge
import scapi.sigma.rework.DLogProtocol._
import scorex.crypto.hash.Blake2b256
import sigmastate.Proof.Challenge
import sigmastate.SigmaProposition.PropositionCode

import scala.util.Try

object EcPointFunctions {
  def decodeBigIntPair(bytes: Array[Byte]): Try[(BigInteger, BigInteger)] = Try {
    val xSize = bytes.head
    val ySize = bytes.tail.head

    assert(bytes.length == 2 + xSize + ySize)

    val xBytes = bytes.slice(2, xSize + 2)
    val yBytes = bytes.slice(xSize + 2, xSize + ySize + 2)

    val x = new BigInteger(1, xBytes)
    val y = new BigInteger(1, yBytes)

    x -> y
  }


  def decodeBigIntTriple(bytes: Array[Byte]): Try[(BigInteger, BigInteger, BigInteger)] = Try {
    val xSize = bytes.head
    val ySize = bytes.tail.head
    val zSize = bytes.tail.tail.head

    assert(bytes.length == 3 + xSize + ySize + zSize)

    val xBytes = bytes.slice(3, xSize + 3)
    val yBytes = bytes.slice(xSize + 3, xSize + ySize + 3)
    val zBytes = bytes.slice(xSize + ySize + 3, xSize + ySize + zSize + 3)

    val x = new BigInteger(1, xBytes)
    val y = new BigInteger(1, yBytes)
    val z = new BigInteger(1, zBytes)

    (x, y, z)
  }
}

class DLogSecret extends Secret {
  override type S = DLogSecret
  override type PK = DLogProposition

  override def companion: SecretCompanion[DLogSecret] = ???

  override def publicImage: DLogProposition = ???

  override lazy val bytes: KeyPair = ???

  override def serializer: Serializer[M] = ???
}


case class DLogProposition(public: PublicKey) extends SigmaProofOfKnowledgeProposition[DLogSecret] {
  override val code = DLogProposition.Code

  override lazy val bytes: PublicKey = public

  //todo: refactor
  def pubKeyAsECPoint(dlog: DlogGroup) = {
    val (x, y) = EcPointFunctions.decodeBigIntPair(bytes).get
    val xy = new ECElementSendableData(x, y)
    dlog.reconstructElement(true, xy)
  }

  override def toString = s"DLogKnowledge(${Base58.encode(public)})"
}

object DLogProposition {
  val Code = 102: Byte

  type PublicKey = Array[Byte]
  type PrivateKey = Array[Byte]
  type KeyPair = Array[Byte]
}

/**
  * TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures
  * https://tools.ietf.org/html/rfc8032#page-9
  *
  * @param signature
  */
case class SchnorrSignature(signature: Array[Byte])(implicit val dlogGroup: DlogGroup) extends Proof[DLogProposition] {
  override val propCode: PropositionCode = DLogProposition.Code

  val soundness = 256

  val hf = Blake2b256

  override def verify(proposition: DLogProposition, message: Proof.Challenge): Boolean = {
    //signature is g^r as a pair of points, and z
    val (grx, gry, zb) = EcPointFunctions.decodeBigIntTriple(signature).get
    val gr = new ECElementSendableData(grx, gry)

    //h = g^w is a pubkey
    val h = proposition.pubKeyAsECPoint(dlogGroup)
    val x: DLogCommonInput = DLogCommonInput(dlogGroup, h, soundness)

    val a: FirstDLogProverMessage = FirstDLogProverMessage(gr)

    val z: SecondDLogProverMessage = SecondDLogProverMessage(zb)

    val sigmaTranscript = DLogTranscript(x, a, Challenge(hf(message)), z)
    sigmaTranscript.accepted
  }

  override type M = this.type

  override def serializer: Serializer[M] = ???
}

object SchnorrSignature {
  val soundness = 256

  val hf = Blake2b256

  implicit val dlogGroup: DlogGroup = new BcDlogECFp()

  def proposition(secret: BigInteger): DLogProposition = {
    val g = dlogGroup.getGenerator
    val gw = dlogGroup.exponentiate(g, secret).generateSendableData().asInstanceOf[ECElementSendableData]

    val gwx = gw.getX.toByteArray
    val gwy = gw.getY.toByteArray

    DLogProposition(Array(gwx.length.toByte, gwy.length.toByte) ++ gwx ++ gwy)
  }

  def sign(secret: BigInteger, message: Array[Byte]): SchnorrSignature = {
    val proverInput = DLogProverInput(secret)

    val g = dlogGroup.getGenerator
    val gw = dlogGroup.exponentiate(g, secret)
    val commonInput = DLogCommonInput(dlogGroup, gw, soundness)

    val prover = new DLogProver(commonInput, proverInput)

    val fm = prover.firstMessage

    val sm = prover.secondMessage(Challenge(hf(message)))


    val grec = fm.ecData
    val z = sm.z

    val grxb = grec.getX.toByteArray
    val gryb = grec.getY.toByteArray
    val zb = z.toByteArray

    val sb = Array(grxb.length.toByte, gryb.length.toByte, zb.length.toByte) ++ grxb ++ gryb ++ zb
    SchnorrSignature(sb)
  }
}

case class CAndProof(proofs: Proof[SigmaProposition]*) extends Proof[CAnd]  {
  override val propCode: PropositionCode = CAnd.Code

  override def verify(proposition: CAnd, challenge: Proof.Challenge): Boolean =
    proofs.zip(proposition.statements).forall { case (proof, prop) =>
      proof.verify(prop, challenge)
    }

  override type M = this.type

  override def serializer: Serializer[M] = ???
}