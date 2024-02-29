package org.ergoplatform.sdk.js

import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import org.ergoplatform.sdk.ExtendedInputBox
import org.ergoplatform.sdk.JavaHelpers.UniversalConverter
import org.ergoplatform.sdk.wallet.protocol.context
import scorex.crypto.authds.ADKey
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.Extensions.CollBytesOps
import sigma.ast.syntax.GroupElementConstant
import sigma.ast.{Constant, GroupElementConstant, SType}
import sigma.data.Iso.{isoStringToArray, isoStringToColl}
import sigma.data.{CBigInt, CGroupElement, Digest32Coll, Digest32CollRType, Iso}
import sigma.interpreter.{ContextExtension, ProverResult, SigmaMap}
import sigma.js.{AvlTree, GroupElement}
import sigma.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigma.{Coll, Colls}
import sigmastate.eval.{CHeader, CPreHeader}
import sigmastate.fleetSdkCommon.distEsmTypesBoxesMod.Box
import sigmastate.fleetSdkCommon.distEsmTypesCommonMod.HexString
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.fleetSdkCommon.distEsmTypesTokenMod.TokenAmount
import sigmastate.fleetSdkCommon.distEsmTypesTransactionsMod.{SignedTransaction, UnsignedTransaction}
import sigmastate.fleetSdkCommon.{distEsmTypesBoxesMod => boxesMod, distEsmTypesCommonMod => commonMod, distEsmTypesContextExtensionMod => contextExtensionMod, distEsmTypesInputsMod => inputsMod, distEsmTypesProverResultMod => proverResultMod, distEsmTypesRegistersMod => registersMod, distEsmTypesTokenMod => tokenMod}

import java.math.BigInteger
import scala.collection.immutable.ListMap
import scala.scalajs.js
import scala.scalajs.js.Object

/** Definitions of isomorphisms. */
object Isos {
  val isoStringToGroupElement: Iso[String, sigma.GroupElement] = new Iso[String, sigma.GroupElement] {
    override def to(x: String): sigma.GroupElement = {
      val bytes = Base16.decode(x).get
      ValueSerializer.deserialize(bytes).asInstanceOf[GroupElementConstant].value
    }
    override def from(x: sigma.GroupElement): String = {
      val bytes = ValueSerializer.serialize(GroupElementConstant(x))
      Base16.encode(bytes)
    }
  }

  val isoGroupElement: Iso[GroupElement, sigma.GroupElement] = new Iso[GroupElement, sigma.GroupElement] {
    override def to(x: GroupElement): sigma.GroupElement = {
      CGroupElement(x.point)
    }
    override def from(x: sigma.GroupElement): GroupElement = {
      new GroupElement(x.asInstanceOf[CGroupElement].wrappedValue)
    }
  }

  implicit val isoBoxId: Iso[boxesMod.BoxId, ErgoBox.BoxId] = new Iso[boxesMod.BoxId, ErgoBox.BoxId] {
    override def to(x: boxesMod.BoxId): ErgoBox.BoxId = ADKey @@@ isoStringToArray.to(x)

    override def from(x: ErgoBox.BoxId): boxesMod.BoxId = isoStringToArray.from(x)
  }

  implicit val isoHexStringToConstant: Iso[HexString, Constant[SType]] = new Iso[HexString, Constant[SType]] {
    override def to(x: HexString): Constant[SType] = {
      val bytes = isoStringToArray.to(x)
      val value = ValueSerializer.deserialize(bytes)
      value.asInstanceOf[Constant[SType]]
    }
    override def from(x: Constant[SType]): HexString = {
      val bytes = ValueSerializer.serialize(x)
      isoStringToArray.from(bytes)
    }
  }

  implicit val isoHeader: Iso[Header, sigma.Header] = new Iso[Header, sigma.Header] {
    override def to(a: Header): sigma.Header = {
      CHeader(
        id = isoStringToColl.to(a.id),
        version = a.version,
        parentId = isoStringToColl.to(a.parentId),
        ADProofsRoot = isoStringToColl.to(a.ADProofsRoot),
        stateRoot = AvlTree.isoAvlTree.to(a.stateRoot),
        transactionsRoot = isoStringToColl.to(a.transactionsRoot),
        timestamp = sigma.js.Isos.isoBigIntToLong.to(a.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.to(a.nBits),
        height = a.height,
        extensionRoot = isoStringToColl.to(a.extensionRoot),
        minerPk = isoGroupElement.to(a.minerPk),
        powOnetimePk = isoGroupElement.to(a.powOnetimePk),
        powNonce = isoStringToColl.to(a.powNonce),
        powDistance = sigma.js.Isos.isoBigInt.to(a.powDistance),
        votes = isoStringToColl.to(a.votes)
      )
    }
    override def from(b: sigma.Header): Header = {
      val header = b.asInstanceOf[CHeader]
      new Header(
        id = isoStringToColl.from(header.id),
        version = header.version,
        parentId = isoStringToColl.from(header.parentId),
        ADProofsRoot = isoStringToColl.from(header.ADProofsRoot),
        stateRoot = AvlTree.isoAvlTree.from(header.stateRoot),
        transactionsRoot = isoStringToColl.from(header.transactionsRoot),
        timestamp = sigma.js.Isos.isoBigIntToLong.from(header.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.from(header.nBits),
        height = header.height,
        extensionRoot = isoStringToColl.from(header.extensionRoot),
        minerPk = isoGroupElement.from(header.minerPk),
        powOnetimePk = isoGroupElement.from(header.powOnetimePk),
        powNonce = isoStringToColl.from(header.powNonce),
        powDistance = sigma.js.Isos.isoBigInt.from(header.powDistance),
        votes = isoStringToColl.from(header.votes)
      )
    }
  }

  implicit val isoPreHeader: Iso[PreHeader, sigma.PreHeader] = new Iso[PreHeader, sigma.PreHeader] {
    override def to(a: PreHeader): sigma.PreHeader = {
      CPreHeader(
        version = a.version,
        parentId = isoStringToColl.to(a.parentId),
        timestamp = sigma.js.Isos.isoBigIntToLong.to(a.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.to(a.nBits),
        height = a.height,
        minerPk = isoGroupElement.to(a.minerPk),
        votes = isoStringToColl.to(a.votes)
      )
    }
    override def from(b: sigma.PreHeader): PreHeader = {
      val header = b.asInstanceOf[CPreHeader]
      new PreHeader(
        version = header.version,
        parentId = isoStringToColl.from(header.parentId),
        timestamp = sigma.js.Isos.isoBigIntToLong.from(header.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.from(header.nBits),
        height = header.height,
        minerPk = isoGroupElement.from(header.minerPk),
        votes = isoStringToColl.from(header.votes)
      )
    }
  }

  val isoBlockchainParameters: Iso[BlockchainParameters, sdk.BlockchainParameters] = new Iso[BlockchainParameters, sdk.BlockchainParameters] {
    override def to(a: BlockchainParameters): sdk.BlockchainParameters = {
      sdk.CBlockchainParameters(
        storageFeeFactor = a.storageFeeFactor,
        minValuePerByte = a.minValuePerByte,
        maxBlockSize = a.maxBlockSize,
        tokenAccessCost = a.tokenAccessCost,
        inputCost = a.inputCost,
        dataInputCost = a.dataInputCost,
        outputCost = a.outputCost,
        maxBlockCost = a.maxBlockCost,
        softForkStartingHeight = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).to(a.softForkStartingHeight),
        softForkVotesCollected = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).to(a.softForkVotesCollected),
        blockVersion = a.blockVersion
      )
    }
    override def from(b: sdk.BlockchainParameters): BlockchainParameters = {
      new BlockchainParameters(
        storageFeeFactor = b.storageFeeFactor,
        minValuePerByte = b.minValuePerByte,
        maxBlockSize = b.maxBlockSize,
        tokenAccessCost = b.tokenAccessCost,
        inputCost = b.inputCost,
        dataInputCost = b.dataInputCost,
        outputCost = b.outputCost,
        maxBlockCost = b.maxBlockCost,
        softForkStartingHeight = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).from(b.softForkStartingHeight),
        softForkVotesCollected = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).from(b.softForkVotesCollected),
        blockVersion = b.blockVersion
      )
    }
  }

  implicit val isoBlockchainStateContext: Iso[BlockchainStateContext, context.BlockchainStateContext] = new Iso[BlockchainStateContext, context.BlockchainStateContext] {
    override def to(a: BlockchainStateContext): context.BlockchainStateContext = {
      context.CBlockchainStateContext(
        sigmaLastHeaders = sigma.js.Isos.isoArrayToColl(isoHeader).to(a.sigmaLastHeaders),
        previousStateDigest = isoStringToColl.to(a.previousStateDigest),
        sigmaPreHeader = isoPreHeader.to(a.sigmaPreHeader)
      )
    }

    override def from(b: context.BlockchainStateContext): BlockchainStateContext = {
      new BlockchainStateContext(
        sigmaLastHeaders = sigma.js.Isos.isoArrayToColl(isoHeader).from(b.sigmaLastHeaders),
        previousStateDigest = isoStringToColl.from(b.previousStateDigest),
        sigmaPreHeader = isoPreHeader.from(b.sigmaPreHeader)
      )
    }
  }

  implicit val isoContextExtension: Iso[contextExtensionMod.ContextExtension, ContextExtension] = new Iso[contextExtensionMod.ContextExtension, ContextExtension] {
    override def to(x: contextExtensionMod.ContextExtension): ContextExtension = {
      var map = new ListMap[Byte, Constant[SType]]()
      val keys = js.Object.keys(x).sorted
      for ( k <- keys ) {
        val id = k.toInt.toByte
        val c = isoHexStringToConstant.to(x.apply(id).get.get)
        map = map + (id -> c)
      }
      ContextExtension(SigmaMap(map))
    }

    override def from(x: ContextExtension): contextExtensionMod.ContextExtension = {
      val res = new Object().asInstanceOf[contextExtensionMod.ContextExtension]
      x.values.iterator.foreach { case (k, v: Constant[_]) =>
        val hex = isoHexStringToConstant.from(v)
        res.update(k, hex) // todo: will be order respected after?
      }
      res
    }
  }

  implicit val isoUnsignedInput: Iso[inputsMod.UnsignedInput, UnsignedInput] = new Iso[inputsMod.UnsignedInput, UnsignedInput] {
    override def to(x: inputsMod.UnsignedInput): UnsignedInput =
      new UnsignedInput(x.boxId.convertTo[ErgoBox.BoxId], isoContextExtension.to(x.extension))

    override def from(x: UnsignedInput): inputsMod.UnsignedInput =
      inputsMod.UnsignedInput(x.boxId.convertTo[boxesMod.BoxId], isoContextExtension.from(x.extension))
  }

  implicit val isoProverResult: Iso[proverResultMod.ProverResult, ProverResult] = new Iso[proverResultMod.ProverResult, ProverResult] {
    override def to(a: proverResultMod.ProverResult): ProverResult = {
      ProverResult(
        proof = isoStringToArray.to(a.proofBytes),
        extension = isoContextExtension.to(a.extension)
      )
    }
    override def from(b: ProverResult): proverResultMod.ProverResult = {
      proverResultMod.ProverResult(
        isoContextExtension.from(b.extension),
        isoStringToArray.from(b.proof)
      )
    }
  }

  implicit val isoSignedInput: Iso[inputsMod.SignedInput, Input] = new Iso[inputsMod.SignedInput, Input] {
    override def to(x: inputsMod.SignedInput): Input =
      Input(x.boxId.convertTo[ErgoBox.BoxId], isoProverResult.to(x.spendingProof))
    override def from(x: Input): inputsMod.SignedInput =
      inputsMod.SignedInput(x.boxId.convertTo[boxesMod.BoxId], isoProverResult.from(x.spendingProof))
  }

  implicit val isoDataInput: Iso[inputsMod.DataInput, DataInput] = new Iso[inputsMod.DataInput, DataInput] {
    override def to(x: inputsMod.DataInput): DataInput = DataInput(x.boxId.convertTo[ErgoBox.BoxId])

    override def from(x: DataInput): inputsMod.DataInput = inputsMod.DataInput(x.boxId.convertTo[boxesMod.BoxId])
  }

  implicit val isoAmount: Iso[commonMod.Amount, Long] = new Iso[commonMod.Amount, Long] {
    override def to(x: commonMod.Amount): Long = x.asInstanceOf[Any] match {
      case s: String => BigInt(s).toLong
      case _ => java.lang.Long.parseLong(x.asInstanceOf[js.BigInt].toString(10))
    }
    override def from(x: Long): commonMod.Amount = x.toString
  }

  implicit val isoToken: Iso[tokenMod.TokenAmount[commonMod.Amount], Token] =
    new Iso[tokenMod.TokenAmount[commonMod.Amount], Token] {
      override def to(x: tokenMod.TokenAmount[commonMod.Amount]): Token =
        (Digest32Coll @@@ Colls.fromArray(Base16.decode(x.tokenId).get), isoAmount.to(x.amount))

      override def from(x: Token): tokenMod.TokenAmount[commonMod.Amount] =
        tokenMod.TokenAmount[commonMod.Amount](isoAmount.from(x._2), x._1.toHex)
    }

  val isoTokenArray: Iso[js.Array[tokenMod.TokenAmount[commonMod.Amount]], Coll[Token]] =
    new Iso[js.Array[tokenMod.TokenAmount[commonMod.Amount]], Coll[Token]] {
      override def to(x: js.Array[tokenMod.TokenAmount[commonMod.Amount]]): Coll[Token] = {
        sigma.js.Isos.isoArrayToColl(isoToken).to(x)
      }
      override def from(x: Coll[Token]): js.Array[tokenMod.TokenAmount[commonMod.Amount]] = {
        sigma.js.Isos.isoArrayToColl(isoToken).from(x)
      }
    }

  val isoNonMandatoryRegisters: Iso[registersMod.NonMandatoryRegisters, AdditionalRegisters] =
    new Iso[registersMod.NonMandatoryRegisters, AdditionalRegisters] {
      override def to(x: registersMod.NonMandatoryRegisters): AdditionalRegisters = {
        val regs = Seq(
          x.R4 -> R4,
          x.R5 -> R5,
          x.R6 -> R6,
          x.R7 -> R7,
          x.R8 -> R8,
          x.R9 -> R9
        ).collect {
          case (regOpt, id) if regOpt.isDefined => id -> isoHexStringToConstant.to(regOpt.get)
        }
        Map(regs:_*)
      }
      override def from(regs: AdditionalRegisters): registersMod.NonMandatoryRegisters = {
        def regHexOpt(t: NonMandatoryRegisterId): Option[HexString] =
          regs.get(t).map(v => isoHexStringToConstant.from(v.asInstanceOf[Constant[SType]]))

        val resRegs = NonMandatoryRegisters()
        regHexOpt(R4).foreach(resRegs.setR4(_))
        regHexOpt(R5).foreach(resRegs.setR5(_))
        regHexOpt(R6).foreach(resRegs.setR6(_))
        regHexOpt(R7).foreach(resRegs.setR7(_))
        regHexOpt(R8).foreach(resRegs.setR8(_))
        regHexOpt(R9).foreach(resRegs.setR9(_))
        resRegs
      }
    }

  implicit val isoBoxCandidate: Iso[boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters], ErgoBoxCandidate] = new Iso[boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters], ErgoBoxCandidate] {
    override def to(x: boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters]): ErgoBoxCandidate = {
      val ergoBoxCandidate = new ErgoBoxCandidate(
        value = isoAmount.to(x.value),
        ergoTree = {
          val bytes = Base16.decode(x.ergoTree).get
          ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
        },
        x.creationHeight.toInt,
        additionalTokens = isoTokenArray.to(x.assets),
        additionalRegisters = isoNonMandatoryRegisters.to(x.additionalRegisters)
      )
      ergoBoxCandidate
    }

    override def from(x: ErgoBoxCandidate): boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters] = {
      val ergoTree = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(x.ergoTree)
      val ergoTreeStr = Base16.encode(ergoTree)
      val assets = isoTokenArray.from(x.additionalTokens)
      boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters](
        ergoTree = ergoTreeStr,
        value = isoAmount.from(x.value),
        assets = assets,
        creationHeight = x.creationHeight,
        additionalRegisters = isoNonMandatoryRegisters.from(x.additionalRegisters)
      )
    }
  }

  val isoBox: Iso[Box[commonMod.Amount, NonMandatoryRegisters], ErgoBox] = new Iso[Box[commonMod.Amount, NonMandatoryRegisters], ErgoBox] {
    override def to(x: Box[commonMod.Amount, NonMandatoryRegisters]): ErgoBox = {
      val ergoBox = new ErgoBox(
        value = isoAmount.to(x.value),
        ergoTree = {
          val bytes = Base16.decode(x.ergoTree).get
          ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
        },
        creationHeight = x.creationHeight.toInt,
        additionalTokens = isoTokenArray.to(x.assets),
        additionalRegisters = isoNonMandatoryRegisters.to(x.additionalRegisters),
        transactionId = ModifierId @@ x.transactionId,
        index = x.index.toShort
      )
      ergoBox
    }

    override def from(x: ErgoBox): Box[commonMod.Amount, NonMandatoryRegisters] = {
      val ergoTree = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(x.ergoTree)
      val ergoTreeStr = Base16.encode(ergoTree)
      val assets = isoTokenArray.from(x.additionalTokens)
      Box[commonMod.Amount, NonMandatoryRegisters](
        boxId = Base16.encode(x.id),
        ergoTree = ergoTreeStr,
        value = isoAmount.from(x.value),
        assets = assets,
        creationHeight = x.creationHeight,
        additionalRegisters = isoNonMandatoryRegisters.from(x.additionalRegisters),
        transactionId = x.transactionId,
        index = x.index
      )
    }
  }

  val isoEIP12UnsignedInput: Iso[inputsMod.EIP12UnsignedInput, ExtendedInputBox] =
    new Iso[inputsMod.EIP12UnsignedInput, ExtendedInputBox] {
      override def to(x: inputsMod.EIP12UnsignedInput): ExtendedInputBox = {
        val box = Box[commonMod.Amount, NonMandatoryRegisters](
          boxId = x.boxId,
          ergoTree = x.ergoTree,
          value = x.value,
          assets = x.assets.asInstanceOf[js.Array[TokenAmount[commonMod.Amount]]],
          creationHeight = x.creationHeight,
          additionalRegisters = x.additionalRegisters,
          transactionId = x.transactionId,
          index = x.index
        )
        val ergoBox = isoBox.to(box)
        val extendedInputBox = ExtendedInputBox(ergoBox, isoContextExtension.to(x.extension))
        extendedInputBox
      }
      override def from(x: ExtendedInputBox): inputsMod.EIP12UnsignedInput = {
        val box = isoBox.from(x.box)
        val ext = isoContextExtension.from(x.extension)
        inputsMod.EIP12UnsignedInput(
          boxId = box.boxId,
          ergoTree = box.ergoTree,
          value = box.value.toString,
          assets = box.assets.asInstanceOf[js.Array[TokenAmount[String]]],
          creationHeight = box.creationHeight,
          additionalRegisters = box.additionalRegisters,
          transactionId = box.transactionId,
          index = box.index,
          extension = ext
        )
      }
    }

  val isoUnsignedTransaction: Iso[UnsignedTransaction, UnsignedErgoLikeTransaction] =
    new Iso[UnsignedTransaction, UnsignedErgoLikeTransaction] {
      override def to(a: UnsignedTransaction): UnsignedErgoLikeTransaction = {
        new UnsignedErgoLikeTransaction(
          inputs = sigma.js.Isos.isoArrayToIndexed(isoUnsignedInput).to(a.inputs),
          dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).to(a.dataInputs),
          outputCandidates = sigma.js.Isos.isoArrayToIndexed(isoBoxCandidate).to(a.outputs),
        )
      }

      override def from(b: UnsignedErgoLikeTransaction): UnsignedTransaction = {
        UnsignedTransaction(
          inputs = sigma.js.Isos.isoArrayToIndexed(isoUnsignedInput).from(b.inputs),
          dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).from(b.dataInputs),
          outputs = sigma.js.Isos.isoArrayToIndexed(isoBoxCandidate).from(b.outputCandidates)
        )
      }
    }

  val isoSignedTransaction: Iso[SignedTransaction, ErgoLikeTransaction] =
    new Iso[SignedTransaction, ErgoLikeTransaction] {
      override def to(a: SignedTransaction): ErgoLikeTransaction = {
        new ErgoLikeTransaction(
          inputs = sigma.js.Isos.isoArrayToIndexed(isoSignedInput).to(a.inputs),
          dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).to(a.dataInputs),
          outputCandidates = sigma.js.Isos.isoArrayToIndexed(isoBox).to(a.outputs),
        )
      }

      override def from(tx: ErgoLikeTransaction): SignedTransaction = {
        val inputs = sigma.js.Isos.isoArrayToIndexed(isoSignedInput).from(tx.inputs)
        val dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).from(tx.dataInputs)
        val outputs = sigma.js.Isos.isoArrayToIndexed(isoBox).from(tx.outputs)
        SignedTransaction(dataInputs, tx.id, inputs, outputs)
      }
    }

}