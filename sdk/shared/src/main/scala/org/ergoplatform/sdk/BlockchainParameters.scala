package org.ergoplatform.sdk

/** Blockchain parameters re-adjustable via miners voting and voting-related data.
  * All these fields are included into extension section of a first block of a voting epoch.
  *
  * @param storageFeeFactor cost of storing 1 byte in UTXO for four years, in nanoErgs
  * @param minValuePerByte cost of a transaction output, in computation unit
  * @param maxBlockSize max block size, in bytes
  * @param tokenAccessCost cost of a token contained in a transaction, in computation unit
  * @param inputCost cost of a transaction input, in computation unit
  * @param dataInputCost cost of a transaction data input, in computation unit
  * @param outputCost cost of a transaction output, in computation unit
  * @param maxBlockCost computation units limit per block
  * @param softForkStartingHeight height when voting for a soft-fork had been started
  * @param softForkVotesCollected votes for soft-fork collected in previous epochs
  * @param blockVersion Protocol version activated on the network
  */
case class BlockchainParameters(
    storageFeeFactor: Int,
    minValuePerByte: Int,
    maxBlockSize: Int,
    tokenAccessCost: Int,
    inputCost: Int,
    dataInputCost: Int,
    outputCost: Int,
    maxBlockCost: Int,
    softForkStartingHeight: Option[Int],
    softForkVotesCollected: Option[Int],
    blockVersion: Byte
)
