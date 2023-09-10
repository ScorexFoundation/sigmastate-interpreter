package org.ergoplatform.dsl

import sigma.Coll
import sigmastate.eval.{CAvlTree, CostingSigmaDslBuilder}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigma.AvlTree
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import CostingSigmaDslBuilder.Colls
import sigma.data.{AvlTreeData, AvlTreeFlags}

object AvlTreeHelpers {

  /** Create authenticated dictionary with given allowed operations and key-value entries. */
  def createAvlTree(flags: AvlTreeFlags, entries: (ADKey, ADValue)*): (AvlTree, BatchAVLProver[Digest32, Blake2b256.type]) = {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val ok = entries.forall { case (key, value) =>
      avlProver.performOneOperation(Insert(key, value)).isSuccess
    }
    if (!ok) throw new Exception("Test tree generation failed")
    val _ = avlProver.generateProof()
    val digest = avlProver.digest
    val treeData = new AvlTreeData(Colls.fromArray(digest), flags, 32, None)
    (CAvlTree(treeData), avlProver)
  }

  implicit class ADKeyArrayOps(arr: Array[ADKey]) {
    def toColl: Coll[Coll[Byte]] = Colls.fromArray(arr.map(x => Colls.fromArray(x)))
  }

  implicit class ADKeyValueArrayOps(arr: Array[(ADKey, ADValue)]) {
    def toColl: Coll[(Coll[Byte], Coll[Byte])] = {
      val kvs = arr.map { case (k, v) => (Colls.fromArray(k), Colls.fromArray(v)) }
      Colls.fromArray(kvs)
    }
  }

}
