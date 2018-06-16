package sigmastate

import org.bouncycastle.util.BigIntegers
import scapi.sigma.DLogProtocol.{ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.Value
import sigmastate.interpreter.CryptoConstants
import sigmastate.utils.Helpers


object SigSerializer {

  val hashSize = CryptoConstants.soundnessBits / 8
  val order = CryptoConstants.groupSize

  def toBytes(tree: UncheckedTree): Array[Byte] = {

    def traverseNode(node: UncheckedTree,
                     acc: Array[Byte],
                     writeChallenge: Boolean = true): Array[Byte] = node match {
      case dl: UncheckedSchnorr =>
        acc ++
          (if (writeChallenge) dl.challenge else Array.emptyByteArray) ++
          BigIntegers.asUnsignedByteArray(order, dl.secondMessage.z.bigInteger)
      case dh: UncheckedDiffieHellmanTuple =>
        acc ++
          (if (writeChallenge) dh.challenge else Array.emptyByteArray) ++
          BigIntegers.asUnsignedByteArray(order, dh.secondMessage.z)
      case and: CAndUncheckedNode =>
        val challenge = and.asInstanceOf[UncheckedConjecture].challengeOpt.get  // todo: what if challengeOpt is empty?
        and.children.foldLeft(acc ++ (if(writeChallenge) challenge else Array.emptyByteArray)) { case (ba, child) =>
          traverseNode(child.asInstanceOf[UncheckedTree], ba, writeChallenge = false)
        }
      case or: COrUncheckedNode =>
        val parentChal = if (writeChallenge) or.challengeOpt.get else Array.emptyByteArray // todo: what if challengeOpt is empty?

        val res = or.children.init.foldLeft(acc ++ parentChal) { case (ba, child) =>
          traverseNode(child.asInstanceOf[UncheckedTree], ba, writeChallenge = true)
        }
        traverseNode(or.children.last.asInstanceOf[UncheckedTree], res, writeChallenge = false)

      // todo: there is a warning that this case list may not be exhaustive because of possibility of UncheckedConjecture

    }

    tree match {
      case NoProof => Array.emptyByteArray
      case _ => traverseNode(tree, Array[Byte](), writeChallenge = true) // always write the root challenge
    }
  }


  def parseAndComputeChallenges(exp: Value[SBoolean.type], bytes: Array[Byte]): UncheckedTree = {

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
        val (e, hp) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val (seq, finalPos) = and.sigmaBooleans.foldLeft(Seq[UncheckedTree]() -> (pos + hp)) { case ((s, p), child) =>
          val (rewrittenChild, consumed) = traverseNode(child, bytes, p, Some(e))
          (s :+ rewrittenChild, p + consumed)
        }
        CAndUncheckedNode(Some(e), Seq(), seq) -> (finalPos - pos)
      case or: COR =>
        val (e, numChalBytes) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val (seq, lastPos, lastChallenge) = or.sigmaBooleans.init.foldLeft((Seq[UncheckedTree](), pos + numChalBytes, e)) {
          case ((s, p, challengeXOR), child) =>
            val (rewrittenChild, consumed) = traverseNode(child, bytes, p, challengeOpt = None)
            (s :+ rewrittenChild, p + consumed,
              Helpers.xor(challengeXOR, rewrittenChild match {
                // todo: this ugliness would go away if we had uniformity of how challenges are treated in uncheckedProof
                  // todo: compilers complains this list may not be exhaustive
                case uc: UncheckedConjecture => uc.challengeOpt.get
                case ul: UncheckedLeaf[_] => ul.challenge
              }))
        }
        val (lastChild, numRightChildBytes) = traverseNode(or.sigmaBooleans.last, bytes, lastPos, Some(lastChallenge))
        COrUncheckedNode(Option(e), Seq(), seq :+ lastChild) -> (lastPos + numRightChildBytes - pos)
    }

    if (bytes.isEmpty)
      NoProof
    else
      traverseNode(exp, bytes, hashSize, Some(bytes.slice(0, hashSize)) )._1 // get the root hash, then call
  }
}