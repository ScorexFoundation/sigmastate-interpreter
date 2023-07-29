package org.ergoplatform.sdk.multisig

import org.ergoplatform.ErgoAddress
import scala.collection.mutable

class AddressBook {
  val signersByMasterAddress: mutable.HashMap[ErgoAddress, Signer] = mutable.HashMap.empty

  val signersByEip3Address: mutable.HashMap[ErgoAddress, Signer] = mutable.HashMap.empty

  def add(signer: Signer): this.type = {
    if (!signersByMasterAddress.contains(signer.masterAddress)) {
      signersByMasterAddress.put(signer.masterAddress, signer)
      signer.eip3Addresses.foreach { eip3Address =>
        signersByEip3Address.put(eip3Address, signer)
      }
    }
    this
  }

  def ++=(signers: Signer*): this.type = {
    signers.foreach(add);
    this
  }

  def get(address: ErgoAddress): Option[Signer] = {
    signersByMasterAddress.get(address).orElse(signersByEip3Address.get(address))
  }
}

object AddressBook {
  def apply(signers: Signer*): AddressBook = {
    new AddressBook ++= (signers: _*)
  }
}