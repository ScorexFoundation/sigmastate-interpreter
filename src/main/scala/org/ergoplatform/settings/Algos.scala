package org.ergoplatform.settings

import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._
import special.collection.Coll

import scala.util.Try

// TODO: use it in ergo (delete ergo's own)
object Algos extends ScorexEncoding {

  type HF = Blake2b256.type

  val hash: HF = Blake2b256

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())

  @inline def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  @inline def encode(bytes: Coll[Byte]): String = encoder.encode(bytes.toArray)

  @inline def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  @inline def decodeUnsafe(str: String): Array[Byte] = decode(str).get

  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash
}
