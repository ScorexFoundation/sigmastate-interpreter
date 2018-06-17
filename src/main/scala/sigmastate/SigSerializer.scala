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

    /**
      * Verifier Step 2: In a top-down traversal of the tree, obtain the challenges for the children of every
      * non-leaf node by reading them from the proof or computing them.
      * Verifier Step 3: For every leaf node, read the response z provided in the proof.
      * @param exp
      * @param bytes
      * @param pos
      * @param challengeOpt if non-empty, then the challenge has been computed for this node by its parent;
      *                     else it needs to be read from the proof
      * @return
      */
    def traverseNode(exp: Value[SBoolean.type],
                     bytes: Array[Byte],
                     pos: Int,
                     challengeOpt: Option[Array[Byte]] = None): (UncheckedSigmaTree, Int) = {
      // Verifier Step 2: Let e_0 be the challenge in the node here (e_0 is called "challenge" in the code)
      val (challenge, chalLen) = if (challengeOpt.isEmpty) {
        bytes.slice(pos, pos + hashSize) -> hashSize
      } else {
        challengeOpt.get -> 0
      }
      exp match {
        case dl: ProveDlog =>
          // Verifier Step 3: For every leaf node, read the response z provided in the proof.
          val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + chalLen, pos + chalLen + order))
          UncheckedSchnorr(dl, None, challenge, SecondDLogProverMessage(z)) -> (chalLen + order)

        case dh: ProveDiffieHellmanTuple =>
          // Verifier Step 3: For every leaf node, read the response z provided in the proof.
          val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + chalLen, pos + chalLen + order))
          UncheckedDiffieHellmanTuple(dh, None, challenge, SecondDiffieHellmanTupleProverMessage(z)) -> (chalLen + order)

        case and: CAND =>
          // Verifier Step 2: If the node is AND, then all of its children get e_$ as the challenge
          val (seq, finalPos) = and.sigmaBooleans.foldLeft(Seq[UncheckedSigmaTree]() -> (pos + chalLen)) { case ((s, p), child) =>
            val (rewrittenChild, consumed) = traverseNode(child, bytes, p, Some(challenge))
            (s :+ rewrittenChild, p + consumed)
          }
          CAndUncheckedNode(challenge, seq) -> (finalPos - pos)

        case or: COR =>
          // Verifier Step 2: If the node is OR, then each of its children except rightmost
          // one gets the challenge given in the proof for that node.
          // The rightmost child gets a challenge computed as an XOR of the challenges of all the other children and e_0.

          // Read all the children but the last and compute the XOR of all the challenges including e_0
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
      // Verifier step 1: Read the root challenge from the proof.
      traverseNode(exp, bytes, 0, challengeOpt = None)._1 // get the root hash, then call
  }
}