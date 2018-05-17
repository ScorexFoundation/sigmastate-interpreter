package sigmastate

import org.bouncycastle.util.BigIntegers
import org.bouncycastle.util.test.FixedSecureRandom.BigInteger
import scapi.sigma.{Challenge, NonInteractiveProver, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import scapi.sigma.DLogProtocol._
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.Value

import scala.annotation.tailrec


case class SchnorrSigner(override val publicInput: ProveDlog, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, ProveDlog, UncheckedSchnorr] {

  def prove(challenge: Array[Byte]): UncheckedSchnorr = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = if (privateInputOpt.isDefined) {
      val firstMsg = prover.firstMessage
      val e = Blake2b256(firstMsg.ecData.getEncoded(true) ++ challenge)
      firstMsg -> prover.secondMessage(Challenge(e))
    } else {
      prover.simulate(Challenge(challenge))
    }

    //val sb = SchnorrSigner.serialize(fm, sm)

    UncheckedSchnorr(publicInput, Some(fm), challenge, sm)
  }
}


object SigSerializer {
  val hashSize = 32
  val order = 48 //bytes

  def toBytes(tree: UncheckedTree) = {

    def traverseNode(node: UncheckedTree, acc: Array[Byte]): Array[Byte] = node match {
      case dl: UncheckedSchnorr =>
        acc ++ dl.challenge ++ BigIntegers.asUnsignedByteArray(order, dl.secondMessage.z.bigInteger)
      case dh: UncheckedDiffieHellmanTuple =>
        acc ++ dh.challenge ++ BigIntegers.asUnsignedByteArray(order, dh.secondMessage.z)
      case and: CAndUncheckedNode =>
        and.leafs.foldLeft(acc) { case (ba, leaf) =>
          traverseNode(leaf.asInstanceOf[UncheckedTree], ba)
        }
      case or: COrUncheckedNode =>
        or.children.foldLeft(acc) { case (ba, leaf) =>
          traverseNode(leaf.asInstanceOf[UncheckedTree], ba)
        }
    }

    tree match {
      case NoProof => Array.emptyByteArray
      case _ => traverseNode (tree, Array[Byte] () )
    }
  }


  def parse(exp: Value[SBoolean.type], bytes: Array[Byte]): UncheckedTree = {

    def traverseNode(exp: Value[SBoolean.type], bytes: Array[Byte], pos: Int): (UncheckedTree, Int) = exp match {
      case dl: ProveDlog =>
        val e = bytes.slice(pos, pos + hashSize)
        val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + hashSize, pos + hashSize + order))
        UncheckedSchnorr(dl, None, e, SecondDLogProverMessage(z)) -> (hashSize + order)
      case dh: ProveDiffieHellmanTuple =>
        val e = bytes.slice(pos, pos + hashSize)
        val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + hashSize, pos + hashSize + order))
        UncheckedDiffieHellmanTuple(dh, None, e, SecondDiffieHellmanTupleProverMessage(z)) -> (hashSize + order)
      case and: CAND =>
        val (seq, finalPos) = and.sigmaBooleans.foldLeft(Seq[UncheckedTree]() -> pos){case ((s, p), leaf) =>
          val (rewrittenLeaf, consumed) = traverseNode(leaf, bytes, p)
          (s :+ rewrittenLeaf, p + consumed)
        }
        CAndUncheckedNode(and, None, Seq(), seq) -> (finalPos - pos)
      case or: COR =>
        val (seq, finalPos) = or.sigmaBooleans.foldLeft(Seq[UncheckedTree]() -> pos){case ((s, p), leaf) =>
          val (rewrittenLeaf, consumed) = traverseNode(leaf, bytes, p)
          (s :+ rewrittenLeaf, p + consumed)
        }
        COrUncheckedNode(or, None, Seq(), seq) -> (finalPos - pos)
    }

    if(bytes.isEmpty) NoProof else traverseNode(exp, bytes, 0)._1
  }
}