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

    def traverseNode(node: UncheckedSigmaTree,
                     acc: Array[Byte],
                     writeChallenge: Boolean = true): Array[Byte] = {
      val parentChal = (if (writeChallenge) node.challenge else Array.emptyByteArray)
      node match {
        case dl: UncheckedSchnorr =>
          acc ++
            parentChal ++
            BigIntegers.asUnsignedByteArray(order, dl.secondMessage.z.bigInteger)
        case dh: UncheckedDiffieHellmanTuple =>
          acc ++
            parentChal ++
            BigIntegers.asUnsignedByteArray(order, dh.secondMessage.z)
        case and: CAndUncheckedNode =>
          // don't write children's challenges -- they are equal to the challenge of this node
          and.children.foldLeft(acc ++ parentChal) { case (ba, child) =>
            traverseNode(child, ba, writeChallenge = false)
          }
        case or: COrUncheckedNode =>
          // don't write last child's challenge -- it's compute by the verifier via XOR
          val res = or.children.init.foldLeft(acc ++ parentChal) { case (ba, child) =>
            traverseNode(child, ba, writeChallenge = true)
          }
          traverseNode(or.children.last, res, writeChallenge = false)

        case _ => ???
      }
    }

    tree match {
      case NoProof => Array.emptyByteArray
      case t: UncheckedSigmaTree => traverseNode(t, Array[Byte](), writeChallenge = true) // always write the root challenge
    }
  }


  def parseAndComputeChallenges(exp: Value[SBoolean.type], bytes: Array[Byte]): UncheckedTree = {

    def traverseNode(exp: Value[SBoolean.type],
                     bytes: Array[Byte],
                     pos: Int,
                     challengeOpt: Option[Array[Byte]] = None): (UncheckedSigmaTree, Int) = {
      val (challenge, chalLen) = if (challengeOpt.isEmpty) {
        bytes.slice(pos, pos + hashSize) -> hashSize
      } else {
        challengeOpt.get -> 0
      }
      exp match {
        case dl: ProveDlog =>
          val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + chalLen, pos + chalLen + order))
          UncheckedSchnorr(dl, None, challenge, SecondDLogProverMessage(z)) -> (chalLen + order)
        case dh: ProveDiffieHellmanTuple =>
          val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + chalLen, pos + chalLen + order))
          UncheckedDiffieHellmanTuple(dh, None, challenge, SecondDiffieHellmanTupleProverMessage(z)) -> (chalLen + order)
        case and: CAND =>
          // child challenges are equal to this node's challenge
          val (seq, finalPos) = and.sigmaBooleans.foldLeft(Seq[UncheckedSigmaTree]() -> (pos + chalLen)) { case ((s, p), child) =>
            val (rewrittenChild, consumed) = traverseNode(child, bytes, p, Some(challenge))
            (s :+ rewrittenChild, p + consumed)
          }
          CAndUncheckedNode(challenge, seq) -> (finalPos - pos)
        case or: COR =>
          // read all the challenges except the last node's and XOR them all together with the current
          // node's challenge
          val (seq, lastPos, lastChallenge) = or.sigmaBooleans.init.foldLeft((Seq[UncheckedSigmaTree](), pos + chalLen, challenge)) {
            case ((s, p, challengeXOR), child) =>
              val (rewrittenChild, consumed) = traverseNode(child, bytes, p, challengeOpt = None)
              (s :+ rewrittenChild, p + consumed,
                Helpers.xor(challengeXOR, rewrittenChild.challenge))
          }
          // use the computed XOR for last child's challenge
          val (lastChild, numRightChildBytes) = traverseNode(or.sigmaBooleans.last, bytes, lastPos, Some(lastChallenge))
          COrUncheckedNode(challenge, seq :+ lastChild) -> (lastPos + numRightChildBytes - pos)
      }
    }

    if (bytes.isEmpty)
      NoProof
    else
      traverseNode(exp, bytes, hashSize, Some(bytes.slice(0, hashSize)))._1 // get the root hash, then call
  }
}