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
    * @param realSecretsToExtract - public keys of secrets with real proofs
    * @param simulatedSecretsToExtract - public keys of secrets with simulated proofs
    * @return - bag of OtherSecretProven and OtherCommitment hints
    */
  def bagForMultisig(context: CTX,
                     exp: ErgoTree,
                     proof: Array[Byte],
                     realSecretsToExtract: Seq[SigmaBoolean],
                     simulatedSecretsToExtract: Seq[SigmaBoolean] = Seq.empty): HintsBag = {

    val prop = propositionFromErgoTree(exp, context.validationSettings)
    val (propTree, _) = applyDeserializeContext(context, prop)
    val reducedTree = reduceToCrypto(context, propTree).get._1

    val ut = SigSerializer.parseAndComputeChallenges(reducedTree, proof)
    val proofTree = computeCommitments(ut).get.asInstanceOf[UncheckedSigmaTree]

    def traverseNode(tree: ProofTree,
                     realPropositions: Seq[SigmaBoolean],
                     simulatedPropositions: Seq[SigmaBoolean],
                     hintsBag: HintsBag): HintsBag = {
      tree match {
        case inner: UncheckedConjecture =>
          inner.children.foldLeft(hintsBag) { case (hb, c) =>
            traverseNode(c, realPropositions, simulatedPropositions, hb)
          }
        case leaf: UncheckedLeaf[_] =>
          val realFound = realPropositions.contains(leaf.proposition)
          val simulatedFound = simulatedPropositions.contains(leaf.proposition)
          if (realFound || simulatedFound) {
            val hints = if(realFound) {
              Seq(
                RealCommitment(leaf.proposition, leaf.commitmentOpt.get),
                RealSecretProof(leaf.proposition, Challenge @@ leaf.challenge, leaf)
              )
            } else {
              Seq(
                SimulatedCommitment(leaf.proposition, leaf.commitmentOpt.get),
                SimulatedSecretProof(leaf.proposition, Challenge @@ leaf.challenge, leaf)
              )
            }
            hintsBag.addHints(hints :_*)
          } else hintsBag
      }
    }

    traverseNode(proofTree, realSecretsToExtract, simulatedSecretsToExtract, HintsBag.empty)
  }

}
