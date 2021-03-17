package sigmastate

import org.bouncycastle.util.BigIntegers
import sigmastate.basics.DLogProtocol.{SecondDLogProverMessage, ProveDlog}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.Values.SigmaBoolean
import sigmastate.interpreter.CryptoConstants
import sigmastate.utils.{SigmaByteReader, Helpers}
import Helpers.xor
import gf2t.GF2_192_Poly
import scalan.Nullable
import sigmastate.basics.{SecondDiffieHellmanTupleProverMessage, ProveDHTuple}
import sigmastate.serialization.SigmaSerializer
import spire.syntax.all.cfor

object SigSerializer {

  val hashSize = CryptoConstants.soundnessBits / 8
  val order = CryptoConstants.groupSize

  def toBytes(tree: UncheckedTree): Array[Byte] = {

    def traverseNode(node: UncheckedSigmaTree,
                     acc: Array[Byte],
                     writeChallenge: Boolean = true): Array[Byte] = {
      val parentChal = if (writeChallenge) node.challenge else Array.emptyByteArray
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
          // don't write last child's challenge -- it's computed by the verifier via XOR
          val res = or.children.init.foldLeft(acc ++ parentChal) { case (ba, child) =>
            traverseNode(child, ba, writeChallenge = true)
          }
          traverseNode(or.children.last, res, writeChallenge = false)

        case t: CThresholdUncheckedNode =>
          // write the polynomial, except the zero coefficient
          val poly = t.polynomialOpt.get.toByteArray(false)

          // don't write children's challenges
          t.children.foldLeft(acc ++ parentChal ++ poly) { case (ba, child) =>
            traverseNode(child, ba, writeChallenge = false)
          }

        case _ => ???
      }
    }

    tree match {
      case NoProof => Array.emptyByteArray
      case t: UncheckedSigmaTree => traverseNode(t, Array[Byte](), writeChallenge = true) // always write the root challenge
    }
  }


  def parseAndComputeChallenges(exp: SigmaBoolean, bytes: Array[Byte]): UncheckedTree = {

    /**
      * Verifier Step 2: In a top-down traversal of the tree, obtain the challenges for the children of every
      * non-leaf node by reading them from the proof or computing them.
      * Verifier Step 3: For every leaf node, read the response z provided in the proof.
      *
      * @param exp
      * @param bytes
      * @param pos
      * @param challengeOpt if non-empty, then the challenge has been computed for this node by its parent;
      *                     else it needs to be read from the proof
      * @return
      */
    def traverseNode(exp: SigmaBoolean,
                     bytes: Array[Byte],
                     pos: Int,
                     challengeOpt: Option[Challenge] = None): (UncheckedSigmaTree, Int) = {
      // Verifier Step 2: Let e_0 be the challenge in the node here (e_0 is called "challenge" in the code)
      val (challenge, chalLen) = if (challengeOpt.isEmpty) {
        (Challenge @@ bytes.slice(pos, pos + hashSize)) -> hashSize
      } else {
        challengeOpt.get -> 0
      }
      exp match {
        case dl: ProveDlog =>
          // Verifier Step 3: For every leaf node, read the response z provided in the proof.
          val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + chalLen, pos + chalLen + order))
          UncheckedSchnorr(dl, None, challenge, SecondDLogProverMessage(z)) -> (chalLen + order)

        case dh: ProveDHTuple =>
          // Verifier Step 3: For every leaf node, read the response z provided in the proof.
          val z = BigIntegers.fromUnsignedByteArray(bytes.slice(pos + chalLen, pos + chalLen + order))
          UncheckedDiffieHellmanTuple(dh, None, challenge, SecondDiffieHellmanTupleProverMessage(z)) -> (chalLen + order)

        case and: CAND =>
          // Verifier Step 2: If the node is AND, then all of its children get e_0 as the challenge
          val (seq, finalPos) = and.children.foldLeft(Seq[UncheckedSigmaTree]() -> (pos + chalLen)) { case ((s, p), child) =>
            val (rewrittenChild, consumed) = traverseNode(child, bytes, p, Some(challenge))
            (s :+ rewrittenChild, p + consumed)
          }
          CAndUncheckedNode(challenge, seq) -> (finalPos - pos)

        case or: COR =>
          // Verifier Step 2: If the node is OR, then each of its children except rightmost
          // one gets the challenge given in the proof for that node.
          // The rightmost child gets a challenge computed as an XOR of the challenges of all the other children and e_0.

          // Read all the children but the last and compute the XOR of all the challenges including e_0
          val (seq, lastPos, lastChallenge) = or.children.init.foldLeft((Seq[UncheckedSigmaTree](), pos + chalLen, challenge)) {
            case ((s, p, challengeXOR), child) =>
              val (rewrittenChild, consumed) = traverseNode(child, bytes, p, challengeOpt = None)
              val ch = Challenge @@ xor(challengeXOR, rewrittenChild.challenge)
              (s :+ rewrittenChild, p + consumed, ch)
          }
          // use the computed XOR for last child's challenge
          val (lastChild, numRightChildBytes) = traverseNode(or.children.last, bytes, lastPos, Some(lastChallenge))
          COrUncheckedNode(challenge, seq :+ lastChild) -> (lastPos + numRightChildBytes - pos)

        case t: CTHRESHOLD =>
          // Verifier Step 2: If the node is THRESHOLD,
          // evaluate the polynomial Q(x) at points 1, 2, ..., n to get challenges for child 1, 2, ..., n, respectively.

          // Read the polynomial -- it has n-k coefficients
          val endPolyPos = pos + chalLen + hashSize * (t.children.length - t.k)
          val polynomial = GF2_192_Poly.fromByteArray(challenge, bytes.slice(pos + chalLen, endPolyPos))


          val (seq, finalPos, _) = t.children.foldLeft((Seq[UncheckedSigmaTree](), endPolyPos, 1)) {
            case ((s, p, childIndex), child) =>
              val (rewrittenChild, consumed) = traverseNode(child, bytes, p, Some(Challenge @@ polynomial.evaluate(childIndex.toByte).toByteArray))
              (s :+ rewrittenChild, p + consumed, childIndex + 1)
          }

          // Verifier doesn't need the polynomial anymore -- hence pass in None
          CThresholdUncheckedNode(challenge, seq, t.k, Some(polynomial)) -> (finalPos - pos)
      }
    }

    if (bytes.isEmpty)
      NoProof
    else {
    // Verifier step 1: Read the root challenge from the proof.
      val res = traverseNode(exp, bytes, 0, challengeOpt = None)._1 // get the root hash, then call
      val r = SigmaSerializer.startReader(bytes)
      val resNew = parseAndComputeChallenges(exp, r, Nullable.None)
      assert(res == resNew)
      res
    }
  }

  /** HOTSPOT: don't beautify the code
    * Note, Nullable is used instead of Option to avoid allocations. */
  def parseAndComputeChallenges(exp: SigmaBoolean,
                                r: SigmaByteReader,
                                challengeOpt: Nullable[Challenge] = Nullable.None): UncheckedSigmaTree = {
    // Verifier Step 2: Let e_0 be the challenge in the node here (e_0 is called "challenge" in the code)
    val challenge = if (challengeOpt.isEmpty) {
      Challenge @@ r.getBytes(hashSize)
    } else {
      challengeOpt.get
    }

    exp match {
      case dl: ProveDlog =>
        // Verifier Step 3: For every leaf node, read the response z provided in the proof.
        val z = BigIntegers.fromUnsignedByteArray(r.getBytes(order))
        UncheckedSchnorr(dl, None, challenge, SecondDLogProverMessage(z))

      case dh: ProveDHTuple =>
        // Verifier Step 3: For every leaf node, read the response z provided in the proof.
        val z = BigIntegers.fromUnsignedByteArray(r.getBytes(order))
        UncheckedDiffieHellmanTuple(dh, None, challenge, SecondDiffieHellmanTupleProverMessage(z))

      case and: CAND =>
        // Verifier Step 2: If the node is AND, then all of its children get e_0 as the challenge
        val nChildren = and.children.length
        val children = new Array[UncheckedSigmaTree](nChildren)
        cfor(0)(_ < nChildren, _ + 1) { i =>
          children(i) = parseAndComputeChallenges(and.children(i), r, Nullable(challenge))
        }
        CAndUncheckedNode(challenge, children)

      case or: COR =>
        // Verifier Step 2: If the node is OR, then each of its children except rightmost
        // one gets the challenge given in the proof for that node.
        // The rightmost child gets a challenge computed as an XOR of the challenges of all the other children and e_0.

        // Read all the children but the last and compute the XOR of all the challenges including e_0
        val nChildren = or.children.length
        val children = new Array[UncheckedSigmaTree](nChildren)
        val xorBuf = challenge.clone()
        val iLastChild = nChildren - 1
        cfor(0)(_ < iLastChild, _ + 1) { i =>
          val parsedChild = parseAndComputeChallenges(or.children(i), r, Nullable.None)
          children(i) = parsedChild
          Helpers.xorU(xorBuf, parsedChild.challenge) // xor it into buffer
        }
        val lastChild = or.children(iLastChild)

        // use the computed XOR for last child's challenge
        children(iLastChild) = parseAndComputeChallenges(
          lastChild, r, challengeOpt = Nullable(Challenge @@ xorBuf))

        COrUncheckedNode(challenge, children)

      case t: CTHRESHOLD =>
        // Verifier Step 2: If the node is THRESHOLD,
        // evaluate the polynomial Q(x) at points 1, 2, ..., n to get challenges for child 1, 2, ..., n, respectively.

        // Read the polynomial -- it has n-k coefficients
        val nChildren = t.children.length
        val coeffBytes = r.getBytes(hashSize * (nChildren - t.k))
        val polynomial = GF2_192_Poly.fromByteArray(challenge, coeffBytes)

        val children = new Array[UncheckedSigmaTree](nChildren)
        cfor(0)(_ < nChildren, _ + 1) { i =>
          val c = Challenge @@ polynomial.evaluate((i + 1).toByte).toByteArray
          children(i) = parseAndComputeChallenges(t.children(i), r, Nullable(c))
        }

        // Verifier doesn't need the polynomial anymore -- hence pass in None
        CThresholdUncheckedNode(challenge, children, t.k, Some(polynomial))
    }
  }

}