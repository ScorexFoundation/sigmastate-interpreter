package sigmastate

import org.bouncycastle.util.BigIntegers
import scapi.sigma.DLogProtocol.{ProveDlog, SecondDLogProverMessage}
import scapi.sigma.{ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import sigmastate.Values.Value
import sigmastate.interpreter.CryptoConstants
import sigmastate.utils.Helpers


// todo: why are all the .asInstanceOf necessary here? Can we fix the type system to avoid it?
object SigSerializer {
  val hashSize = CryptoConstants.soundnessBits / 8
  val order = CryptoConstants.groupSize

  def toBytes(tree: UncheckedTree): Array[Byte] = {

    def traverseNode(node: UncheckedSigmaTree,
                     acc: Array[Byte],
                     writeChallenge: Boolean = true): Array[Byte] = node match {
      case dl: UncheckedSchnorr =>
        acc ++
          (if (writeChallenge) dl.challenge else Array.emptyByteArray) ++ // todo: what if challengeOpt is empty?
          BigIntegers.asUnsignedByteArray(order, dl.secondMessage.z.bigInteger)
      case dh: UncheckedDiffieHellmanTuple =>
        acc ++
          (if (writeChallenge) dh.challenge else Array.emptyByteArray) ++ // todo: what if challengeOpt is empty?
          BigIntegers.asUnsignedByteArray(order, dh.secondMessage.z)
      case and: CAndUncheckedNode =>
        val parentChal = if (writeChallenge) and.challengeOpt.get else Array.emptyByteArray // todo: what if challengeOpt is empty?
        and.children.foldLeft(acc ++ parentChal) { case (ba, child) =>
          traverseNode(child.asInstanceOf[UncheckedSigmaTree], ba, writeChallenge = false)
        }
      case or: COrUncheckedNode =>
        val parentChal = if (writeChallenge) or.challengeOpt.get else Array.emptyByteArray // todo: what if challengeOpt is empty?

        val res = or.children.init.foldLeft(acc ++ parentChal) { case (ba, child) =>
          traverseNode(child.asInstanceOf[UncheckedSigmaTree], ba)
        }
        traverseNode(or.children.last.asInstanceOf[UncheckedSigmaTree], res, writeChallenge = false)
    }
    // todo: there is a warning that this case list may not be exhaustive because of possibility of UncheckedConjecture

    tree match {
      case NoProof => Array.emptyByteArray
      case _ => traverseNode(tree.asInstanceOf[UncheckedSigmaTree], Array[Byte]())
    }
  }


  def parse(exp: Value[SBoolean.type], bytes: Array[Byte]): UncheckedTree = {

    def traverseNode(exp: Value[SBoolean.type],
                     bytes: Array[Byte],
                     pos: Int,
                     challengeOpt: Option[Array[Byte]] = None): (UncheckedSigmaTree, Int) = exp match {
      // todo: there's a lot of repetitive code here that should be cleaned up
      // todo: the position arithmetic is more complicated than it should be because this returns number of bytes consumed rather than new position.
      case dl: ProveDlog =>
        val (e, numChalBytes) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + numChalBytes, pos + numChalBytes + order))
        UncheckedSchnorr(dl, None, e, SecondDLogProverMessage(z)) -> (numChalBytes + order)
      case dh: ProveDiffieHellmanTuple =>
        val (e, numChalBytes) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + numChalBytes, pos + numChalBytes + order))
        UncheckedDiffieHellmanTuple(dh, None, e, SecondDiffieHellmanTupleProverMessage(z)) -> (numChalBytes + order)
      case and: CAND =>
        val (e, numChalBytes) = if (challengeOpt.isEmpty) {
          bytes.slice(pos, pos + hashSize) -> hashSize
        } else {
          challengeOpt.get -> 0
        }
        val (seq, finalPos) = and.sigmaBooleans.foldLeft(Seq[UncheckedTree]() -> (pos + numChalBytes)) { case ((s, p), child) =>
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
                case uc: UncheckedConjecture => uc.challengeOpt.get
                case ul: UncheckedLeaf[_] => ul.challenge
              }))
        }
        val (lastChild, numRightChildBytes) = traverseNode(or.sigmaBooleans.head, bytes, lastPos, Some(lastChallenge))
        COrUncheckedNode(Some(e), Seq(), seq :+ lastChild) -> (lastPos + numRightChildBytes - pos)
    }

    if (bytes.isEmpty) NoProof else traverseNode(exp, bytes, 0)._1
  }
}