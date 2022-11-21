package org.ergoplatform.wallet

import org.ergoplatform.wallet.secrets.DerivationPath

object Constants {
  /** part of the protocol, do not change */
  val SecretKeyLength = 32

  val Encoding = "UTF-8"

  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  /**
    * Pre - EIP3 derivation path
    */
  val preEip3DerivationPath: DerivationPath = DerivationPath.fromEncoded("m/1").get

  /**
    * Post - EIP3 derivation path
    */
  val eip3DerivationPath: DerivationPath = DerivationPath.fromEncoded("m/44'/429'/0'/0/0").get
}
