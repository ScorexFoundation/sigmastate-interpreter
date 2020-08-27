package org.ergoplatform

import java.util

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import scorex.crypto.hash.{Digest32, Blake2b256}
import scorex.util.encode.Base58
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.DLogProtocol.{ProveDlogProp, ProveDlog}
import sigmastate.lang.exceptions.SigmaException
import sigmastate.serialization._
import sigmastate.utxo.{DeserializeContext, Slice}
import special.collection.Coll

import scala.util.Try

/**
  * An address is a short string corresponding to some script used to protect a box. Unlike (string-encoded) binary
  * representation of a script, an address has some useful characteristics:
  *
  * - Integrity of an address could be checked., as it is incorporating a checksum.
  * - A prefix of address is showing network and an address type.
  * - An address is using an encoding (namely, Base58) which is avoiding similarly l0Oking characters, friendly to
  * double-clicking and line-breaking in emails.
  *
  *
  *
  * An address is encoding network type, address type, checksum, and enough information to watch for a particular scripts.
  *
  * Possible network types are:
  * Mainnet - 0x00
  * Testnet - 0x10
  *
  * Address types are, semantics is described below:
  * 0x01 - Pay-to-PublicKey(P2PK) address
  * 0x02 - Pay-to-Script-Hash(P2SH)
  * 0x03 - Pay-to-Script(P2S)
  *
  * For an address type, we form content bytes as follows:
  *
  * P2PK - serialized (compressed) public key
  * P2SH - first 192 bits of the Blake2b256 hash of serialized script bytes
  * P2S  - serialized script
  *
  * Address examples for testnet:
  *
  * 3   - P2PK (3WvsT2Gm4EpsM9Pg18PdY6XyhNNMqXDsvJTbbf6ihLvAmSb7u5RN)
  * ?   - P2SH (rbcrmKEYduUvADj9Ts3dSVSG27h54pgrq5fPuwB)
  * ?   - P2S (Ms7smJwLGbUAjuWQ)
  *
  * for mainnet:
  *
  * 9  - P2PK (9fRAWhdxEsTcdb8PhGNrZfwqa65zfkuYHAMmkQLcic1gdLSV5vA)
  * ?  - P2SH (8UApt8czfFVuTgQmMwtsRBZ4nfWquNiSwCWUjMg)
  * ?  - P2S (4MQyML64GnzMxZgm, BxKBaHkvrTvLZrDcZjcsxsF7aSsrN73ijeFZXtbj4CXZHHcvBtqSxQ)
  *
  *
  * Prefix byte = network type + address type
  *
  * checksum = blake2b256(prefix byte ++ content bytes)
  *
  * address = prefix byte ++ content bytes ++ checksum
  *
  */
sealed trait ErgoAddress {
  /** Address type code used to differentiate between pay-to-public-key, pay-to-script,
    * pay-to-script-hash addresses.
    *
    * NOTE: Network type code is defined by [[ErgoAddressEncoder]] attached to each ErgoAddress
    * instance and it is not included in this value.
    *
    * @see [[P2PKAddress]], [[Pay2SAddress]], [[Pay2SHAddress]]
    */
  val addressTypePrefix: Byte

  /** Serialized bytes of the address content (depending on the address type).
    * Doesn't include network type and address type prefix byte.
    * @see [[P2PKAddress]], [[Pay2SAddress]], [[Pay2SHAddress]]
    */
  val contentBytes: Array[Byte]

  /** ErgoTree which corresponds to the address (depending on the address type).
    * @see [[P2PKAddress]], [[Pay2SAddress]], [[Pay2SHAddress]]
    */
  val script: ErgoTree

  /** Network type code to be used in address encoding. */
  def networkPrefix: NetworkPrefix
}

/** Implementation of pay-to-public-key [[ErgoAddress]]. */
class P2PKAddress(val pubkey: ProveDlog,
                  val pubkeyBytes: Array[Byte])
                 (implicit val encoder: ErgoAddressEncoder) extends ErgoAddress {

  override val addressTypePrefix: Byte = P2PKAddress.addressTypePrefix

  override val contentBytes: Array[Byte] = pubkeyBytes

  override val script: ErgoTree = {
    // NOTE: we don't segregate constants because the whole tree is single constant
    // and we want different addresses of this type to have different `script` values
    ErgoTree(ErgoTree.DefaultHeader, ErgoTree.EmptyConstants, SigmaPropConstant(pubkey))
  }

  override def networkPrefix: NetworkPrefix = encoder.networkPrefix

  override def equals(obj: Any): Boolean = obj match {
    case p2pk: P2PKAddress => util.Arrays.equals(pubkeyBytes, p2pk.pubkeyBytes)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(pubkeyBytes)

  override def toString: String = encoder.toString(this)
}

object P2PKAddress {
  /** Value added to the prefix byte in the serialized bytes of an encoded P2PK address.
    * @see [[ErgoAddressEncoder.toString]]
    */
  val addressTypePrefix: Byte = 1: Byte

  /** Constructs [[P2PKAddress]] instance using the public key of the given [[ProveDlog]]. */
  def apply(pubkey: ProveDlog)(implicit encoder: ErgoAddressEncoder): P2PKAddress = {
    val bs = GroupElementSerializer.toBytes(pubkey.h)
    new P2PKAddress(pubkey, bs)
  }
}

/** Implementation of pay-to-script-hash [[ErgoAddress]]. */
class Pay2SHAddress(val scriptHash: Array[Byte])(implicit val encoder: ErgoAddressEncoder) extends ErgoAddress {
  override val addressTypePrefix: Byte = Pay2SHAddress.addressTypePrefix

  override val contentBytes: Array[Byte] = scriptHash

  override def networkPrefix: NetworkPrefix = encoder.networkPrefix

  import Pay2SHAddress._

  /** The proposition which checks that `contextVar(1)` has original script
    * (whose hash equals to this [[scriptHash]]) which evaluates to true.
    *
    * Assumes the context variable is accessed as `getVar[Coll[Byte]](1).get`
    * and contains serialized original script bytes.
    *
    * NOTE: This script is not stored in [[contentBytes]] of the address.
    * So the address doesn't depend on this script which means this specific script can be
    * changed without breaking the addresses.
    *
    * NOTE: The ErgoTree is created without segregation of the constants.
    *
    * @see ErgoLikeInterpreterSpecification."P2SH - 160 bits" test
    */
  override val script = {
    val hashEquals = EQ(
      Slice(CalcBlake2b256(GetVarByteArray(scriptId).get), IntConstant(0), IntConstant(24)),
      scriptHash
    )
    val scriptIsCorrect = DeserializeContext(scriptId, SSigmaProp)
    ErgoTree.withoutSegregation(SigmaAnd(hashEquals.toSigmaProp, scriptIsCorrect))
  }

  override def equals(obj: Any): Boolean = obj match {
    case p2sh: Pay2SHAddress => util.Arrays.equals(scriptHash, p2sh.scriptHash)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(scriptHash)

  override def toString: String = encoder.toString(this)
}

object Pay2SHAddress {
  /** An id of the context variable used in pay-to-script-hash address script.
    * @see [[Pay2SHAddress.script]]
    */
  val scriptId = 1: Byte

  /** Value added to the prefix byte in the serialized bytes of an encoded P2SH address.
    * @see [[ErgoAddressEncoder.toString]]
    */
  val addressTypePrefix: Byte = 2: Byte

  /** Create Pay-to-script-hash address with the given underlying script (ErgoTree).
    *
    * The tree is first transformed to proposition, substituting constants is necessary
    * and then the other constructor is called.
    *
    * @param  script  ErgoTree representation of guarding script
    * @param  encoder address encoder which is used to encode address bytes as String
    */
  def apply(script: ErgoTree)(implicit encoder: ErgoAddressEncoder): Pay2SHAddress = {
    val prop = script.toProposition(replaceConstants = script.isConstantSegregation)
    apply(prop)
  }

  /** Create Pay-to-script-hash address with the given underlying proposition (SigmaPropValue).
    *
    * The proposition is serialized to bytes, then hashed (Blake2b256) and then 24
    * bytes of the hash are taken as contentBytes of the address.
    *
    * @param  prop    Value representation of guarding script (aka proposition)
    * @param  encoder address encoder which is used to encode address bytes as String
    */
  def apply(prop: SigmaPropValue)(implicit encoder: ErgoAddressEncoder): Pay2SHAddress = {
    val sb = ValueSerializer.serialize(prop)
    val sbh = ErgoAddressEncoder.hash192(sb)
    new Pay2SHAddress(sbh)
  }
}

/** Implementation of pay-to-script [[ErgoAddress]]. */
class Pay2SAddress(override val script: ErgoTree,
                   val scriptBytes: Array[Byte])
                  (implicit val encoder: ErgoAddressEncoder) extends ErgoAddress {
  override val addressTypePrefix: Byte = Pay2SAddress.addressTypePrefix

  override val contentBytes: Array[Byte] = scriptBytes

  override def networkPrefix: NetworkPrefix = encoder.networkPrefix

  override def equals(obj: Any): Boolean = obj match {
    case p2s: Pay2SAddress => util.Arrays.equals(scriptBytes, p2s.scriptBytes)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(scriptBytes)

  override def toString: String = encoder.toString(this)
}

object Pay2SAddress {
  /** Value added to the prefix byte in the serialized bytes of an encoded P2S address.
    * @see [[ErgoAddressEncoder.toString()]]
    */
  val addressTypePrefix: Byte = 3: Byte

  /** Create Pay-to-script address with the given underlying script (ErgoTree).
    *
    * The tree is serialized to contentBytes using default ErgoTree serializer.
    *
    * @param  script  ErgoTree representation of guarding script
    * @param  encoder address encoder which is used to encode address bytes as String
    */
  def apply(script: ErgoTree)(implicit encoder: ErgoAddressEncoder): Pay2SAddress = {
    val sb = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(script)
    new Pay2SAddress(script, sb)
  }
}

/** Network-aware encoder for ErgoAddress <-> Base58String conversions.
  * @param networkPrefix network prefix value to be used in address encoding.
  */
case class ErgoAddressEncoder(networkPrefix: NetworkPrefix) {

  import ErgoAddressEncoder._

  /** This value is be used implicitly in the methods below. */
  implicit private val ergoAddressEncoder: ErgoAddressEncoder = this

  /** Converts the given [[ErgoAddress]] to Base58 string. */
  def toString(address: ErgoAddress): String = {
    val withNetworkByte = (networkPrefix + address.addressTypePrefix).toByte +: address.contentBytes

    val checksum = hash256(withNetworkByte).take(ChecksumLength)
    Base58.encode(withNetworkByte ++ checksum)
  }

  /** Returns true if the given `addrHeadByte` is a header byte of a testnet address, false otherwise. */
  def isTestnetAddress(addrHeadByte: Byte): Boolean = addrHeadByte > TestnetNetworkPrefix

  /** Returns true if the given `addrHeadByte` is a header byte of a mainnet address, false otherwise. */
  def isMainnetAddress(addrHeadByte: Byte): Boolean = addrHeadByte < TestnetNetworkPrefix

  /** Converts the given Base58 string to [[ErgoAddress]] or an error packed in Try. */
  def fromString(addrBase58Str: String): Try[ErgoAddress] = Base58.decode(addrBase58Str).flatMap { bytes =>
    Try {
      val headByte = bytes.head
      networkPrefix match {
        case TestnetNetworkPrefix => require(isTestnetAddress(headByte), "Trying to decode mainnet address in testnet")
        case MainnetNetworkPrefix => require(isMainnetAddress(headByte), "Trying to decode testnet address in mainnet")
      }
      val addressType = (headByte - networkPrefix).toByte
      val (withoutChecksum, checksum) = bytes.splitAt(bytes.length - ChecksumLength)

      if (!util.Arrays.equals(hash256(withoutChecksum).take(ChecksumLength), checksum)) {
        throw new Exception(s"Checksum check fails for $addrBase58Str")
      }

      val contentBytes = withoutChecksum.tail

      addressType match {
        case P2PKAddress.addressTypePrefix =>
          val r = SigmaSerializer.startReader(contentBytes)
          val p = GroupElementSerializer.parse(r)
          new P2PKAddress(ProveDlog(p), contentBytes)
        case Pay2SHAddress.addressTypePrefix =>
          if (contentBytes.length != 24) { //192-bits hash used
            throw new Exception(s"Improper content in P2SH script: $addrBase58Str")
          }
          new Pay2SHAddress(contentBytes)
        case Pay2SAddress.addressTypePrefix =>
          val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(contentBytes)
          new Pay2SAddress(tree, contentBytes)
        case _ =>
          throw new Exception(s"Unsupported address type: $addressType")
      }
    }
  }

  /** Pattern recognizer of [[Pay2SHAddress.script]] propositions.
    * If matched extracts the corresponding hash bytes as `Coll[Byte]` value.
    */
  object IsPay2SHAddress {
    def unapply(exp: SigmaPropValue): Option[Coll[Byte]] = exp match {
      case SigmaAnd(Seq(
            BoolToSigmaProp(
              EQ(
                Slice(_: CalcHash, ConstantNode(0, SInt), ConstantNode(24, SInt)),
                ByteArrayConstant(scriptHash))),
            DeserializeContext(Pay2SHAddress.scriptId, SSigmaProp))) => Some(scriptHash)
      case _ => None
    }
  }

  /** Converts the given [[ErgoTree]] to the corresponding [[ErgoAddress]].
    * It is inverse of [[ErgoAddress.script]] such that `fromProposition(addr.script) == addr`
    *
    * @return Failure(ex) if the `proposition` cannot be converted to any type of address.
    */
  def fromProposition(proposition: ErgoTree): Try[ErgoAddress] = Try {
    proposition.root match {
      case Right(SigmaPropConstant(ProveDlogProp(d))) => P2PKAddress(d)
      case Right(IsPay2SHAddress(scriptHash)) => new Pay2SHAddress(scriptHash.toArray)
      case Right(b: Value[SSigmaProp.type]@unchecked) if b.tpe == SSigmaProp => Pay2SAddress(proposition)
      case Left(unparsedErgoTree) =>
        throw new SigmaException(s"Cannot create ErgoAddress form unparsed ergo tree: $unparsedErgoTree")
      case _ =>
        throw new RuntimeException(s"Cannot create ErgoAddress form proposition: $proposition")
    }
  }
}

object ErgoAddressEncoder {
  /** Type of the network prefix value. */
  type NetworkPrefix = Byte

  /** Value of the prefix byte used to encode Mainnet ErgoAddress. */
  val MainnetNetworkPrefix: NetworkPrefix = 0.toByte

  /** Value of the prefix byte used to encode Testnet ErgoAddress. */
  val TestnetNetworkPrefix: NetworkPrefix = 16.toByte

  /** Length of the checksum section of encoded ergo address bytes. */
  val ChecksumLength = 4

  /** Helper method to hash the given array using Blake2b256. */
  def hash256(input: Array[Byte]): Digest32 = Blake2b256(input)

  /** Helper method to hash the given array using Blake2b256 and take first 192 bit (24 bytes). */
  def hash192(input: Array[Byte]): Array[Byte] = hash256(input).take(24)
}