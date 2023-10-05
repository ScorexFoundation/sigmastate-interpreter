package sigmastate.eval

import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.data.CAvlTree
import sigma.{AvlTree, Coll}

/** Implements operations of AVL tree verifier based on
  * [[scorex.crypto.authds.avltree.batch.BatchAVLVerifier]].
  *
  * @see BatchAVLVerifier
  */
class AvlTreeVerifier private(
    startingDigest: ADDigest,
    proof: SerializedAdProof,
    override val keyLength: Int,
    override val valueLengthOpt: Option[Int])
    extends BatchAVLVerifier[Digest32, Blake2b256.type](
      startingDigest, proof, keyLength, valueLengthOpt) {
  def treeHeight: Int = rootNodeHeight

  /** Override default logging which outputs stack trace to the console. */
  override protected def logError(t: Throwable): Unit = {}
}

object AvlTreeVerifier {
  /** Create an instance of [[AvlTreeVerifier]] for the given tree and proof.
    * Both tree and proof are immutable.
    *
    * @param tree  represents a tree state to verify
    * @param proof proof of tree operations leading to the state digest in the tree
    * @return a new verifier instance
    */
  def apply(tree: AvlTree, proof: Coll[Byte]): AvlTreeVerifier = {
    val treeData = tree.asInstanceOf[CAvlTree].treeData
    val adProof  = SerializedAdProof @@ proof.toArray
    val bv       = new AvlTreeVerifier(
      ADDigest @@ treeData.digest.toArray, adProof, treeData.keyLength, treeData.valueLengthOpt)
    bv
  }
}