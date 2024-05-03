package sigmastate.eval

import org.ergoplatform.{HeaderWithoutPow, HeaderWithoutPowSerializer}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.bytesToId
import sigma.data.SigmaConstants
import sigma.pow.Autolykos2PowValidation
import sigma.{AvlTree, BigInt, Coll, Colls, GroupElement, Header}

/** A default implementation of [[Header]] interface.
  *
  * @see [[Header]] for detailed descriptions
  */
case class CHeader(
    id: Coll[Byte],
    version: Byte,
    parentId: Coll[Byte],
    ADProofsRoot: Coll[Byte],
    stateRoot: AvlTree,
    transactionsRoot: Coll[Byte],
    timestamp: Long,
    nBits: Long,
    height: Int,
    extensionRoot: Coll[Byte],
    minerPk: GroupElement,
    powOnetimePk: GroupElement,
    powNonce: Coll[Byte],
    powDistance: BigInt,
    votes: Coll[Byte],
    unparsedBytes: Coll[Byte]
) extends Header {

  override def serializeWithoutPoW: Coll[Byte] = {
    val headerWithoutPow = HeaderWithoutPow(version, bytesToId(parentId.toArray), Digest32 @@ ADProofsRoot.toArray,
      ADDigest @@ stateRoot.digest.toArray, Digest32 @@ transactionsRoot.toArray, timestamp,
      nBits, height, Digest32 @@ extensionRoot.toArray, votes.toArray, unparsedBytes.toArray)
    Colls.fromArray(HeaderWithoutPowSerializer.toBytes(headerWithoutPow))
  }

  override def checkPow: Boolean = {
    if (version == 1) {
      throw new Exception("Autolykos v1 is not supported") //todo: more specific exception?
    } else {
      Autolykos2PowValidation.checkPoWForVersion2(this)
    }
  }

}

object CHeader {
  /** Size of of Header.votes array. */
  val VotesSize: Int = SigmaConstants.VotesArraySize.value

  /** Size of nonce array from Autolykos POW solution in Header.powNonce array. */
  val NonceSize: Int = SigmaConstants.AutolykosPowSolutionNonceArraySize.value
}