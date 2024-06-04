package org.ergoplatform.sdk.generators

import org.ergoplatform.sdk._
import org.ergoplatform.validation.ValidationSpecification
import org.scalacheck.Gen
import sigma.ast.{ErgoTree, SType}
import sigma.serialization.generators.{ConcreteCollectionGenerators, TypeGenerators, ObjectGenerators => InterpreterObjectGenerators}
import sigmastate.TestsBase
import sigmastate.interpreter.HintsBag

import scala.util.Random

trait ObjectGenerators extends TypeGenerators
  with ValidationSpecification
  with ConcreteCollectionGenerators
  with TestsBase
  with InterpreterObjectGenerators {

  def contractTemplateNameInTests: String = "TestContractTemplate"

  def contractTemplateDescriptionInTests: String = "TestContractTemplateDescription"

  private def getConstValues(ergoTree: ErgoTree,
                             noDefaultValueIndices: Set[Int]):
  Option[IndexedSeq[Option[SType#WrappedType]]] = {
    val values = ergoTree
      .constants
      .zipWithIndex
      .map(c_i => if (noDefaultValueIndices.contains(c_i._2)) None else Some(c_i._1.value))
    val allNone = values.forall(v => v.isEmpty)
    if (allNone) {
      None
    } else {
      Some(values)
    }
  }

  private def getConstAndParams(ergoTree: ErgoTree): (IndexedSeq[SType], Option[IndexedSeq[Option[SType#WrappedType]]], IndexedSeq[Parameter]) = {
    val paramIndices = ergoTree.constants.indices.filter(_ => (Random.nextDouble() < 0.5)).toSet
    val noDefaultValueIndices = paramIndices.filter(_ => (Random.nextDouble() < 0.5))
    val constTypes = ergoTree.constants.map(c => c.tpe)
    val constValues = getConstValues(ergoTree, noDefaultValueIndices)
    val parameters = paramIndices
      .map(i => Parameter(s"param_for_idx$i", s"description_for_idx$i", i)).toIndexedSeq
    (constTypes, constValues, parameters)
  }

  lazy val contractTemplateGen: Gen[ContractTemplate] = for {
    ergoTree <- Gen.delay(ergoTreeWithSegregationGen)
  } yield {
    val (constTypes, constValues, parameters) = getConstAndParams(ergoTree)
    ContractTemplate(
      contractTemplateNameInTests,
      contractTemplateDescriptionInTests,
      constTypes,
      constValues,
      parameters,
      ergoTree.toProposition(false)
    )
  }


  lazy val hintsBagGen: Gen[HintsBag] = {
    val mockBlockchainParameters = CBlockchainParameters(
      storageFeeFactor = 1000,
      minValuePerByte = 1,
      maxBlockSize = 1000000,
      tokenAccessCost = 100,
      inputCost = 10,
      dataInputCost = 10,
      outputCost = 10,
      maxBlockCost = 1000000,
      softForkStartingHeight = Some(100),
      softForkVotesCollected = Some(50),
      blockVersion = 1
    )
    for {
      mnemonic <- Gen.alphaLowerStr
      p = ProverBuilder.forMainnet(mockBlockchainParameters)
        .withMnemonic(SecretString.create(mnemonic), SecretString.empty())
        .build()
    } yield HintsBag(
      p.secrets
        .map(_.publicImage)
        .flatMap(sb => p.generateCommitments(sb).hints)
    )
  }

  lazy val transactionHintsBagGen: Gen[TransactionHintsBag] = for {
    n <- Gen.choose(1, 10)
    bags <- Gen.listOfN(n, hintsBagGen)
  } yield
    bags.zipWithIndex.foldLeft(TransactionHintsBag.empty) {
      case (acc, (hints, idx)) => acc.addHintsForInput(idx, hints)
    }
}
