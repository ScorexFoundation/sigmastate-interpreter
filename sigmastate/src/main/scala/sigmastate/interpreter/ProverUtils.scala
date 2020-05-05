package sigmastate.interpreter

import sigmastate.{ProofTree, SigSerializer, UncheckedConjecture, UncheckedLeaf, UncheckedSigmaTree}
import sigmastate.Values.{ErgoTree, SigmaBoolean}
import sigmastate.basics.VerifierMessage.Challenge


trait ProverUtils extends Interpreter {

  /**
    * A method which is extracting partial proofs of secret knowledge for particular secrets with their
    * respective public images given. Useful for distributed signature applications.
    *
    * See DistributedSigSpecification for examples of usage.
    *
    * @param context      - context used to reduce the proposition
    * @param exp          - proposition to reduce
    * @param proof        - proof for reduced proposition
    * @param knownSecrets - public keys of known secrets
    * @return - bag of OtherSecretProven and OtherCommitment hints
    */
  def bagForMultisig(context: CTX,
                     exp: ErgoTree,
                     proof: Array[Byte],
                     knownSecrets: Seq[SigmaBoolean]): HintsBag = {

    val prop = propositionFromErgoTree(exp, context)
    val (propTree, _) = applyDeserializeContext(context, prop)
    val reducedTree = reduceToCrypto(context, propTree).get._1

    val ut = SigSerializer.parseAndComputeChallenges(reducedTree, proof)
    val proofTree = computeCommitments(ut).get.asInstanceOf[UncheckedSigmaTree]

    def traverseNode(tree: ProofTree, propositions: Seq[SigmaBoolean], hintsBag: HintsBag): HintsBag = {
      tree match {
        case inner: UncheckedConjecture =>
          inner.children.foldLeft(hintsBag) { case (hb, c) => traverseNode(c, propositions, hb) }
        case leaf: UncheckedLeaf[_] =>
          if (propositions.contains(leaf.proposition)) {
            val cmtHint = OtherCommitment(leaf.proposition, leaf.commitmentOpt.get)
            val secretHint = OtherSecretProven(leaf.proposition, Challenge @@ leaf.challenge, leaf)
            hintsBag.addHints(cmtHint, secretHint)
          } else hintsBag
      }
    }

    traverseNode(proofTree, knownSecrets, HintsBag.empty)
  }

}
