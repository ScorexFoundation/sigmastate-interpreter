package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.Extensions.{DoubleOps, HeaderOps}
import org.ergoplatform.sdk.NetworkType.Mainnet
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.ergoplatform.{ErgoAddress, ErgoTreePredef, P2PKAddress}
import org.ergoplatform.sdk.{BlockchainContext, BlockchainParameters, ExtendedInputBox, OutBox, ProverBuilder, ReducedTransaction, SecretString, UnsignedTransactionBuilder}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.util.CollectionUtil.AnyOps
import sigmastate.TestsBase
import sigmastate.Values.{Constant, ErgoTree}
import sigma.SigmaTestingData

class SigningSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers
    with TestsBase
    with SigmaTestingData {

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

  val mockTxId = "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809"

  val ctx = BlockchainContext(Mainnet, testMainnetParameters,
    BlockchainStateContext(
      sigmaLastHeaders = headers,
      previousStateDigest = headers(0).stateRoot.digest,
      sigmaPreHeader = preHeader
    )
  )

  def createSigner(secret: String): Signer = Signer(
    ProverBuilder.forMainnet(testMainnetParameters)
        .withMnemonic(SecretString.create(secret), SecretString.empty())
        .build()
  )

  val alice = createSigner("Alice secret")
  val bob = createSigner("Bob secret")
  val carol = createSigner("Carol secret")
  val david = createSigner("David secret")

  def createRtx(
      ctx: BlockchainContext, inputs: Seq[ExtendedInputBox],
      recepient: ErgoAddress, changeAddress: ErgoAddress): ReducedTransaction = {
    val txB = UnsignedTransactionBuilder(ctx)
    val output = txB.outBoxBuilder
        .value(inputs.map(_.value).sum - BlockchainParameters.MinFee)
        .contract(recepient.script).build()
    val feeOut = txB.outBoxBuilder
        .value(BlockchainParameters.MinFee)
        .contract(ErgoTreePredef.feeProposition())
        .build()
    val unsigned = txB
        .addInputs(inputs: _*)
        .addOutputs(output, feeOut)
        .sendChangeTo(changeAddress)
        .build()
    // create a prover without secrets as they are not needed for reduction
    val prover = ProverBuilder.forMainnet(ctx.parameters).build()
    val reduced = prover.reduce(ctx.stateContext, unsigned, 0)
    reduced
  }

  def createTestOut(
      ctx: BlockchainContext,
      amount: Long,
      contract: ErgoTree,
      registers: Constant[_]*
  ): OutBox = {
    val out = UnsignedTransactionBuilder(ctx).outBoxBuilder
        .value(amount)
        .contract(contract)
        .perform(b => if (registers.isEmpty) b else b.registers(registers: _*))
        .build()
    out
  }

  def createInput(ctx: BlockchainContext, owner: Signer): ExtendedInputBox = {
    val out = createTestOut(ctx, 1.erg, owner.prover.getP2PKAddress.script)
    out.convertToInputWith(mockTxId, 0)
  }

  property("Signing workflow") {
    val aliceInput = createInput(ctx, alice)
    val bobInput = createInput(ctx, bob)
    val carolInput = createInput(ctx, carol)
    val inputs = Seq(aliceInput, bobInput, carolInput)
    val reduced = createRtx(ctx, inputs, alice.masterAddress, david.masterAddress)

    reduced shouldNot be(null)
  }
}

