package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.Extensions.DoubleOps
import org.ergoplatform.sdk.NetworkType.Mainnet
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.ergoplatform.sdk._
import org.ergoplatform.{ErgoAddress, ErgoTreePredef}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalan.util.CollectionUtil.AnyOps
import sigmastate.TestsBase
import sigmastate.Values.{Constant, ErgoTree}
import special.sigma.SigmaTestingData

import scala.collection.mutable

class SigningSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers
    with TestsBase
    with SigmaTestingData {

  val mainnetParameters = BlockchainParameters(
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

  val ctx = BlockchainContext(NetworkType.Mainnet, mainnetParameters,
    BlockchainStateContext(
      sigmaLastHeaders = headers,
      previousStateDigest = headers(0).stateRoot.digest,
      sigmaPreHeader = preHeader
    )
  )

  def createSigner(secret: String): Signer = Signer(
    ProverBuilder.forMainnet(mainnetParameters)
        .withMnemonic(SecretString.create(secret), SecretString.empty())
        .build()
  )

  val alice = createSigner("Alice secret")
  val bob = createSigner("Bob secret")
  val carol = createSigner("Carol secret")
  val david = createSigner("David secret")

  def createReducedTx(
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
        .update(b => if (registers.isEmpty) b else b.registers(registers: _*))
        .build()
    out
  }

  def createInput(ctx: BlockchainContext, owner: Signer): ExtendedInputBox = {
    val out = createTestOut(ctx, 1.erg, owner.masterAddress.script)
    out.convertToInputWith(mockTxId, 0)
  }

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

  class SigningSession(
      val reduced: ReducedTransaction
  )
  object SigningSession {
    def apply(reduced: ReducedTransaction): SigningSession = {
      val inputs = reduced.ergoTx.reducedInputs
      val participants = inputs.zipWithIndex.map { case (reducedInput, i) =>
        val sb = reducedInput.reductionResult.value
        sb
      }
      new SigningSession(reduced)
    }
  }

  class CosigningServer(
      val addressBook: AddressBook
  )
  
  property("Signing workflow") {
    val cosigners = Seq(alice, bob, carol)
    val inputs = cosigners.map(createInput(ctx, _))
    val reduced = createReducedTx(ctx, inputs, alice.masterAddress, david.masterAddress)

    reduced shouldNot be(null)

    // neither of cosigners can sign the transaction
    cosigners.foreach(signer =>
      assertExceptionThrown(
        signer.prover.signReduced(reduced),
        exceptionLike[IllegalArgumentException]("Tree root should be real but was UnprovenSchnorr")
      )
    )

    val addressBook = AddressBook(cosigners :+ david: _*)
    cosigners.foreach(s =>
      addressBook.get(s.masterAddress) shouldBe Some(s)
    )

    val session = new SigningSession(reduced)

  }
}


