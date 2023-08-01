package org.ergoplatform.sdk.multisig

import org.ergoplatform.sdk.Extensions.DoubleOps
import org.ergoplatform.sdk._
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.ergoplatform.{ErgoAddress, ErgoTreePredef}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalan.util.CollectionUtil.AnyOps
import sigmastate.Values.{Constant, ErgoTree}
import sigmastate.interpreter.{HintsBag, OwnCommitment, RealCommitment}
import sigmastate.{PositionedLeaf, TestsBase}
import special.sigma.SigmaTestingData

import scala.collection.immutable.Seq

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

  /** Special signer without secretes */
  val publicSigner = Signer(ProverBuilder.forMainnet(mainnetParameters).build())

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

  property("Signing workflow") {
    val cosigners = Seq(alice, bob, carol)
    val inputs = cosigners.map(createInput(ctx, _))
    val reduced = createReducedTx(ctx, inputs, alice.masterAddress, david.masterAddress)

    reduced shouldNot be(null)

    // none of cosigners can sign the transaction
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

    val server = new CosigningServer

    // anyone can start a session (e.g. Alice)
    val sessionId = server.addSession(alice.startCosigning(reduced))

    // each cosigner generates a commitment and stores it in the session
    cosigners.zipWithIndex.foreach { case (signer, i) =>
      val signerPk = signer.pubkey

      // participants can retrieve related sessions
      val session = server.getSessionsFor(signer.allKeys).head
      session.reduced shouldBe reduced

      // obtain next actions for the current session state
      val actions = signer.getActionsFrom(session)
      val expectedAction = CreateCommitment(signerPk, i, PositionedLeaf.at()(signerPk))
      actions shouldBe Seq(expectedAction)

      // then execute actions to obtain a new session state
      val newSession = signer.execute(actions.head, session)

      val HintsBag(Seq(rc @ RealCommitment(image, _, position))) = newSession.collectedHints(actions.head.inputIndex)
      image shouldBe signerPk
      position shouldBe expectedAction.leaf.position

      // signer should store commitments (including secret random values)
      val proposition = session.reduced.inputPropositions(i)
      val Seq(OwnCommitment(`signerPk`, _, _, `position`), `rc`) = signer.getHintsBag(sessionId, proposition).get.hints

      server.updateSession(newSession)
    }

    // each signer added a commitment to the session
    server.getSession(sessionId).get.collectedHints.size shouldBe cosigners.size


    // each cosigner generates a proof and stores it in the session
    cosigners.zipWithIndex.foreach { case (signer, i) =>
      val signerPk = signer.pubkey
      val session = server.getSessionsFor(signer.allKeys).head
      val actions = signer.getActionsFrom(session)
      
      val expectedAction = CreateSignature(signerPk, i, PositionedLeaf.at()(signerPk))
      actions shouldBe Seq(expectedAction)
      val newSession = signer.execute(actions.head, session)

      server.updateSession(newSession)
    }

    // complete signing of the ready session
    val session = server.getReadySessions.head
    val signedTx = publicSigner.createSignedTransaction(session)

    // send the signed transaction to the network and wait for confirmation
    // then finish the session
    server.finishSession(session.id)
  }
}


