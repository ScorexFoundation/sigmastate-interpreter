package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.{BlockchainContext, BlockchainParameters, ExtendedInputBox, ProverBuilder, ReducedTransaction, SecretString}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SigningSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  val testMainnetParameters = BlockchainParameters(
    storageFeeFactor = 1250000,
    minValuePerByte = 360,
    maxBlockSize = 1271009,
    tokenAccessCost = 100,
    inputCost = 2407,
    dataInputCost = 100,
    outputCost = 184,
    maxBlockCost = 8001091,
    softForkStartingHeight = None,
    softForkVotesCollected = None,
    blockVersion = 3
  )

  def createSigner(secret: String): Signer = Signer(
    ProverBuilder.forMainnet(testMainnetParameters)
        .withMnemonic(SecretString.create(secret), SecretString.empty())
        .build()
  )

  val alice = createSigner("Alice secret")
  val bob = createSigner("Bob secret")
  val carol = createSigner("Carol secret")

  def createRtx(ctx: BlockchainContext, inputs: Seq[ExtendedInputBox]): ReducedTransaction = {
//    val txB = ctx.newTxBuilder()
//    val output = txB.outBoxBuilder()
//        .value(inputs.map(_.getValue).sum - Parameters.MinFee)
//        .contract(truePropContract(ctx)).build()
//    val feeOut = txB.outBoxBuilder()
//        .value(Parameters.MinFee)
//        .contract(ctx.newContract(ErgoTreePredef.feeProposition()))
//        .build()
//    val unsigned = txB
//        .addInputs(inputs: _*)
//        .addOutputs(output, feeOut)
//        .build()
//    val prover = ctx.newProverBuilder.build // NOTE, prover without secrets
//    val reduced = prover.reduce(unsigned, 0)
    ???
  }

  property("Signing workflow") {

  }
}

