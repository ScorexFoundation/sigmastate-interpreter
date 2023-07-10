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

/** Global parameters used by SDK */
object BlockchainParameters {
  /** A number of blocks a miner should wait before he/she can spend block reward.
    * This is part of Ergo protocol and cannot be changed.
    */
  val MinerRewardDelay_Mainnet = 720

  val MinerRewardDelay_Testnet = 720

  /** One Erg is 10^9 NanoErg */
  val OneErg: Long = 1000 * 1000 * 1000

  /** Minimum transaction fee in NanoErgs as it is defined in Ergo protocol. */
  val MinFee: Long = 1000 * 1000

  /** Minimum value for a change. It can be used to compute change output value.
    * If computed change is less than this value, it is added to the fee
    * and `change` output in not added to the transaction.
    */
  val MinChangeValue: Long = 1000 * 1000
}