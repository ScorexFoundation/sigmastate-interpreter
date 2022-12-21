package org.ergoplatform.sdk

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.sdk.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.basics.{DLogProtocol, DiffieHellmanTupleProverInput}
import special.sigma.GroupElement

import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer

class ProverBuilder(parameters: ErgoLikeParameters, networkPrefix: NetworkPrefix) {
  private var _masterKey: Option[ExtendedSecretKey] = None

  /** Generated EIP-3 secret keys paired with their derivation path index. */
  private val _eip2Keys =  ArrayBuffer.empty[(Int, ExtendedSecretKey)]

  private val _dhtSecrets = ArrayBuffer.empty[DiffieHellmanTupleProverInput]
  private val _dLogSecrets = ArrayBuffer.empty[DLogProverInput]

  def withMnemonic(
      mnemonicPhrase: SecretString,
      mnemonicPass: SecretString,
      usePre1627KeyDerivation: Boolean
  ): ProverBuilder = {
    _masterKey = Some(JavaHelpers.seedToMasterKey(mnemonicPhrase, mnemonicPass, usePre1627KeyDerivation))
    this
  }

  def withEip3Secret(index: Int): ProverBuilder = {
    require(_masterKey.isDefined, s"Mnemonic is not specified, use withMnemonic method.")
    require(!_eip2Keys.exists(_._1 == index),
      s"Secret key for derivation index $index has already been added.")
    val path = JavaHelpers.eip3DerivationWithLastIndex(index)
    val secretKey = _masterKey.get.derive(path)
    _eip2Keys += (index -> secretKey)
    this
  }

  def withDHTData(
      g: GroupElement,
      h: GroupElement,
      u: GroupElement,
      v: GroupElement,
      x: BigInteger): ProverBuilder = {
    val dht = org.ergoplatform.sdk.JavaHelpers.createDiffieHellmanTupleProverInput(g, h, u, v, x)
    if (_dhtSecrets.contains(dht))
      throw new IllegalStateException("DHTuple secret already exists")
    _dhtSecrets += dht
    this
  }

  def withDLogSecret(x: BigInteger): ProverBuilder = {
    val dLog = new DLogProtocol.DLogProverInput(x)
    if (_dLogSecrets.contains(dLog))
      throw new IllegalStateException("Dlog secret already exists")
    _dLogSecrets += dLog
    this
  }

  def build(): Prover = {
    val secretKeys = _masterKey.toIndexedSeq ++ _eip2Keys.map(_._2)
    val interpreter = new AppkitProvingInterpreter(
      secretKeys,
      _dLogSecrets.toIndexedSeq,
      _dhtSecrets.toIndexedSeq, parameters)
    new Prover(interpreter, networkPrefix)
  }
}

