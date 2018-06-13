package sigmastate

import org.bouncycastle.util.BigIntegers
import scapi.sigma.DLogProtocol.{ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.Value
import sigmastate.interpreter.CryptoConstants


object SigSerializer {
  val hashSize = CryptoConstants.soundnessBits / 8
  val order = CryptoConstants.groupSize

  def toBytes(tree: UncheckedTree): Array[Byte] = {

    def traverseNode(node: UncheckedTree,
                     acc: Array[Byte],
                     writingChallenge: Boolean = true): Array[Byte] = node match {
      case dl: UncheckedSchnorr =>
        acc ++
          (if (writingChallenge) dl.challenge else Array.emptyByteArray) ++
          BigIntegers.asUnsignedByteArray(order, dl.secondMessage.z.bigInteger)
      case dh: UncheckedDiffieHellmanTuple =>
        acc ++
          (if (writingChallenge) dh.challenge else Array.emptyByteArray) ++
          BigIntegers.asUnsignedByteArray(order, dh.secondMessage.z)
      case and: CAndUncheckedNode =>
        val leafs = and.children
        val challenge = leafs.find(pt => pt match{
          case _: UncheckedLeaf[_] => true
          case _ => false
        }).map(_.asInstanceOf[UncheckedLeaf[_]].challenge).getOrElse(Array.emptyByteArray)

        leafs.foldLeft(acc ++ challenge) { case (ba, leaf) =>
          traverseNode(leaf.asInstanceOf[UncheckedTree], ba, writingChallenge = false)
        }
      case or: COrUncheckedNode =>
        or.children.foldLeft(acc) { case (ba, leaf) =>
          traverseNode(leaf.asInstanceOf[UncheckedTree], ba)
        }
    }

    tree match {
      case NoProof => Array.emptyByteArray
      case _ => traverseNode(tree, Array[Byte]())
    }
  }


  def parse(exp: Value[SBoolean.type], bytes: Array[Byte]): UncheckedTree = {

    def traverseNode(exp: Value[SBoolean.type],
                     bytes: Array[Byte],
                     pos: Int,
                     challengeOpt: Option[Array[Byte]] = None): (UncheckedTree, Int) = exp match {
      case dl: ProveDlog =>
        val (e, hp) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + hp, pos + hp + order))
        UncheckedSchnorr(dl, None, e, SecondDLogProverMessage(z)) -> (hp + order)
      case dh: ProveDiffieHellmanTuple =>
        val (e, hp) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + hp, pos + hp + order))
        UncheckedDiffieHellmanTuple(dh, None, e, SecondDiffieHellmanTupleProverMessage(z)) -> (hp + order)
      case and: CAND =>
        val (challenge, cc) = if(and.sigmaBooleans.exists(!_.isInstanceOf[COR]))
          Some(bytes.slice(pos, pos + hashSize)) -> hashSize else None -> 0

        val (seq, finalPos) = and.sigmaBooleans.foldLeft(Seq[UncheckedTree]() -> (pos + cc)) { case ((s, p), leaf) =>
          val (rewrittenLeaf, consumed) = traverseNode(leaf, bytes, p, challenge)
          (s :+ rewrittenLeaf, p + consumed)
        }
        CAndUncheckedNode(None, Seq(), seq) -> (finalPos - pos)
      case or: COR =>
        val (seq, finalPos) = or.sigmaBooleans.foldLeft(Seq[UncheckedTree]() -> pos) { case ((s, p), leaf) =>
          val (rewrittenLeaf, consumed) = traverseNode(leaf, bytes, p)
          (s :+ rewrittenLeaf, p + consumed)
        }
        COrUncheckedNode(None, Seq(), seq) -> (finalPos - pos)
    }

    if (bytes.isEmpty) NoProof else traverseNode(exp, bytes, 0)._1
  }
}