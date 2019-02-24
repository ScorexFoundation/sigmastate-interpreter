package org.ergoplatform.dsl

import scalan.RType
import special.collection.Coll
import sigmastate.serialization.OperationSerializer
import sigmastate.eval.{CAvlTree, CostingSigmaDslBuilder}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.{AvlTreeData, AvlTreeFlags}
import special.sigma.AvlTree
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Operation, Insert}

object AvlTreeHelpers {

  /** Create authenticated dictionary with given allowed operations and key-value entries. */
  def createAvlTree(flags: AvlTreeFlags, entries: (ADKey, ADValue)*): (AvlTree, BatchAVLProver[Digest32, Blake2b256.type]) = {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val ok = entries.forall { case (key, value) =>
      avlProver.performOneOperation(Insert(key, value)).isSuccess
    }
    val proof = avlProver.generateProof()
    val digest = avlProver.digest
    val treeData = new AvlTreeData(digest, flags, 32, None)
    (CAvlTree(treeData), avlProver)
  }

  def serializeOperations(avlProver: BatchAVLProver[Digest32, Blake2b256.type], operations: Seq[Operation]): Coll[Byte] = {
    val serializer = new OperationSerializer(avlProver.keyLength, avlProver.valueLengthOpt)
    val opsBytes: Array[Byte] = serializer.serializeSeq(operations)
    CostingSigmaDslBuilder.Colls.fromArray(opsBytes)
  }

  implicit class ArrayOps[T: RType](arr: Array[T]) {
    def toColl: Coll[T] = CostingSigmaDslBuilder.Colls.fromArray(arr)
  }
}
