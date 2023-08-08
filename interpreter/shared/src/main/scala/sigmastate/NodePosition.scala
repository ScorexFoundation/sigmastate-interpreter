package sigmastate

/**
  * Data type which encodes position of a node in a tree.
  *
  * Position is encoded like following (the example provided is for CTHRESHOLD(2, Seq(pk1, pk2, pk3 && pk4)) :
  *
  *      0
  *    / | \
  *   /  |  \
  * 0-0 0-1 0-2
  *         /|
  *        / |
  *       /  |
  *      /   |
  *  0-2-0  0-2-1
  *
  * So a hint associated with pk1 has a position "0-0", pk4 - "0-2-1" .
  *
  * Please note that "0" prefix is for a crypto tree. There are several kinds of trees during evaluation.
  * Initial mixed tree (ergoTree) would have another prefix.
  *
  * @param positions - positions from root (inclusive) in top-down order
  */
case class NodePosition(positions: Seq[Int]) {
  def child(childIdx: Int): NodePosition = NodePosition(positions :+ childIdx)
  def ++(path: Seq[Int]): NodePosition = NodePosition(positions ++ path)
  override def toString: String = positions.mkString("-")
}

object NodePosition {
  /**
    * Prefix to encode node positions in a crypto tree.
    */
  val CryptoTreePrefix = NodePosition(Seq(0))

  /**
    * Prefix to encode node positions in an ErgoTree instance.
    */
  val ErgoTreePrefix = NodePosition(Seq(1))
}