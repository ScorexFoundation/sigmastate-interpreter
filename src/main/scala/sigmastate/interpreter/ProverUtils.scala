package sigmastate.interpreter

import sigmastate._
import sigmastate.Values.{SigmaBoolean, Value}


trait ProverUtils extends Interpreter {

  /**
    * A method which is extracting partial proofs of secret knowledge for particular secrets with their
    * respective public images given. Useful for multisigs.
    *
    * @param context - context used to reduce the proposition
    * @param proposition - proposition to reduce
    * @param proof - proof for reduced proposition
    * @param knownSecrets - public keys of known secrets
    * @return - bag of OtherSecretProven and OtherCommitment hints
    */
  def bagForMultisig(context: CTX,
                     proposition: Value[SType],
                     proof: Array[Byte],
                     knownSecrets: Seq[SigmaBoolean]): HintsBag = {

    val reducedTree = reduceToCrypto(context, proposition).get._1
    val ut = SigSerializer.parseAndComputeChallenges(reducedTree, proof)
    val proofTree = computeCommitments(ut).get.asInstanceOf[UncheckedSigmaTree]

    def traverseNode(tree: ProofTree, propositions: Seq[SigmaBoolean], hintsBag: HintsBag): HintsBag = {
      tree match {
        case leaf: UncheckedLeaf[_] =>
          if(propositions.contains(leaf.proposition)){
            val h = OtherSecretProven(leaf.proposition, leaf)
            hintsBag.addHint(h).addHint(OtherCommitment(leaf.proposition, leaf.commitmentOpt.get))
          } else hintsBag
        case inner: UncheckedConjecture =>
          inner.children.foldLeft(hintsBag){case (hb, c) => traverseNode(c, propositions, hb)}
      }
    }

    traverseNode(proofTree, knownSecrets, HintsBag.empty)
  }

}
