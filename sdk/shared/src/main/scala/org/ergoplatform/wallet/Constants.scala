package org.ergoplatform.wallet

import org.ergoplatform.wallet.secrets.DerivationPath
import sigmastate.crypto.CryptoFacade

object Constants {
  /** part of the protocol, do not change */
  val SecretKeyLength = 32

  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(CryptoFacade.Encoding)

  /**
    * Pre - EIP3 derivation path
    */
  val preEip3DerivationPath: DerivationPath = DerivationPath.fromEncoded("m/1").get

  /**
    * Post - EIP3 derivation path
    */
  val eip3DerivationPath: DerivationPath = DerivationPath.fromEncoded("m/44'/429'/0'/0/0").get
}
