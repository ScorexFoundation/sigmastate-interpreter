package sigmastate.utxo.examples

import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.ValueSerializer
import org.ergoplatform.ErgoBox.R3
import org.ergoplatform._
import sigmastate.utxo._

import scala.util.Random

class MASTExampleSpecification extends SigmaTestingCommons {

  /**
    * Example of Merklized Abstract Syntax Tree (MAST).
    * MAST are a proposed addition to Bitcoin that allows for smaller transaction sizes, more privacy, and larger smart
    * contracts. In this case script creator build Merkle tree on top of all script branches, and put script like
    * `provide a correct script and it's path to predefined tree root hash` into blockchain. When someone spend coins,
    * it provide a script, a Merkle path to script and required additional data. Thus, unsued branches of a script
    * remain unrevealed, providing more privacy and saving space in a blockchain.
    * See more at https://bitcointechtalk.com/what-is-a-bitcoin-merklized-abstract-syntax-tree-mast-33fdf2da5e2f
    *
    * In the provided example there are 5 different branches of a tree, each one require to reveal some secret.
    *
    */
  property("Merklized Abstract Syntax Tree") {
    val scriptId = 21.toByte
    val proofId = 22.toByte
    val secretId = 23.toByte

    val allSecrets = (0 until 5).map(_ => Random.nextString(32).getBytes("UTF-8"))
    val scriptBranches = allSecrets.map(s => EQ(ByteArrayConstant(s), TaggedByteArray(secretId)))
    val scriptBranchesBytes = scriptBranches.map(b => ValueSerializer.serialize(b))
    val treeElements = scriptBranchesBytes.map(s => (ADKey @@ Blake2b256(s), ADValue @@ s))
    val knownSecretTreeKey = treeElements.head._1
    val knownSecret = ByteArrayConstant(allSecrets.head)

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    treeElements.foreach(s => avlProver.performOneOperation(Insert(s._1, s._2)))
    avlProver.generateProof()
    val treeData = new AvlTreeData(avlProver.digest, 32, None)

    val merklePathToScript = IsMember(ExtractRegisterAs(Self, R3),
      CalcBlake2b256(TaggedByteArray(scriptId)),
      TaggedByteArray(proofId))
    val scriptIsCorrect = DeserializeContext[SBoolean.type](scriptId)
    val prop = AND(merklePathToScript, scriptIsCorrect)

    val recipientProposition = new ErgoLikeProvingInterpreter().dlogSecrets.head.publicImage
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition))),
      self = ErgoBox(20, TrueLeaf, Map(R3 -> AvlTreeConstant(treeData))))

    avlProver.performOneOperation(Lookup(knownSecretTreeKey))
    val knownSecretPathProof = avlProver.generateProof()
    val usedBranch = scriptBranchesBytes.head
    val prover = new ErgoLikeProvingInterpreter()
      .withContextExtender(secretId, knownSecret)
      .withContextExtender(scriptId, ByteArrayConstant(usedBranch))
      .withContextExtender(proofId, ByteArrayConstant(knownSecretPathProof))
    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }
}
