package sigmastate.verification.contract

import scalan.{RType => ERType}
import sigmastate.verification
import sigmastate.verification.{AvlTree, Coll, Context, SigmaProp}
import sigmastate.verification.Coll._
import stainless.annotation.{extern, ignore, library, pure}
import stainless.collection._
import stainless.lang._
import Helpers._
import sigmastate.verification.SigmaDsl.api.{Coll, RType}

import scala.language.implicitConversions

@library
object Helpers {

  @extern
  type ContextT = special.sigma.Context

  @extern
  type AvlTreeT = special.sigma.AvlTree
  //  @library @extern
//  implicit def optionToOption[T](@extern opt: scala.Option[T]): Option[T] = ???

//  @extern @pure
//  implicit def listRType[A](implicit cT: RType[A]): RType[List[A]] = ???

  @extern @pure
  implicit def collRType[A](implicit cT: RType[A]): RType[Coll[A]] = ???

  @extern @pure
  implicit def pairRType[A, B](implicit tA: RType[A], tB: RType[B]): RType[(A, B)] = ???

  @extern @pure
  implicit def ByteType: RType[Byte] = ???

  @extern @pure
  implicit def LongType: RType[Long] = ???

  @extern @pure
  implicit def AvlTreeRType: RType[AvlTree] = ???
}

// TODO define via BoxT (see SigmaPropT)
@library
trait Box {
  @library
  def value: Long
  @library
  def id: Coll[Byte]
  @library @pure
  def R4[T](implicit cT: RType[T]): Option[T]
  @library @pure
  def R5[T](implicit cT: RType[T]): Option[T]
  @library
  def tokens: Coll[(Coll[Byte], Long)]
  @library
  def propositionBytes: Coll[Byte]
}

/*

@library
case class CBox(@extern v: EBox) extends Box{

  @extern @pure
  def value: Long = v.value
  @extern @pure
  def id: List[Byte] = v.id
  @extern @pure
  def R4[T]: Option[T] = ???
//  def R4[T](implicit @extern cT: RType[T]): Option[T] = v.R4[T]

  @extern @pure
  def R5[T]: Option[T] = ??? // v.R5[T]
  //  def R5[T](implicit @extern cT: RType[T]): Option[T] = v.R5[T]

  @extern @pure
  def tokens: List[(List[Byte], Long)] = ??? //v.tokens
  @extern @pure
  def propositionBytes: List[Byte] = v.propositionBytes
}

 */

//case class DummyFundingContext(HEIGHT: Int,
//                               INPUTS: List[Box],
//                               OUTPUTS: List[Box],
//                               SELF: Box,
//                               nextStageScriptHash: List[Byte],
//                               feeBytes: List[Byte]) extends FundingContext {
//
//  override def getVar[T](id: Byte): Option[T] = ???
//
//  override def longToByteArray(l: Long): List[Byte] = ???
//
//  override def byteArrayToLong(bytes: List[Byte]): Long = ???
//
//  override def blake2b256(bytes: List[Byte]): List[Byte] = ???
//}

sealed abstract class ICOContract {

  def ICOFundingContract(ctx: Context, nextStageScriptHash: Coll[Byte], feeBytes: Coll[Byte]): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.length > 1 &&
      getVar[Coll[Byte]](1).isDefined &&
      INPUTS.slice(1, INPUTS.length).forall({ (b: Box) => b.R4[Coll[Byte]].isDefined }) &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined
    )

    val selfIndexIsZero = INPUTS(0).id == SELF.id

    val inputsCount = INPUTS.size

    val proof = getVar[Coll[Byte]](1).get
    val toAdd: Coll[(Coll[Byte], Coll[Byte])] = INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      // TODO avoid getOrElse
      val pk = b.R4[Coll[Byte]].getOrElse(Coll.empty[Byte])
      val value = longToByteArray(b.value)
      (pk, value)
    })

    val modifiedTree = SELF.R5[AvlTree].get.insert(toAdd, proof)

    val expectedTree = OUTPUTS(0).R5[AvlTree]

    val properTreeModification = modifiedTree == expectedTree

    val outputsCount = OUTPUTS.size == 2

    val selfOutputCorrect = if (HEIGHT < 2000) {
      OUTPUTS(0).propositionBytes == SELF.propositionBytes
    } else {
      blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash
    }

    val feeOutputCorrect = outputsCount &&
      (OUTPUTS(1).value <= 1) &&
      (OUTPUTS(1).propositionBytes == feeBytes)

    val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect

    selfIndexIsZero && outputsCorrect && properTreeModification
  }

  def proveICOFundingContractForHeightAbove2000(ctx: Context, nextStageScriptHash: Coll[Byte], feeBytes: Coll[Byte]): Boolean = {
    import ctx._
    require(
      HEIGHT > 0 &&
        INPUTS.length > 1 &&
        OUTPUTS.length == 2 &&
        INPUTS(0).id == SELF.id &&
        getVar[Coll[Byte]](1).isDefined &&
        INPUTS.slice(1, INPUTS.length).forall({ (b: Box) => b.R4[Coll[Byte]].isDefined }) &&
        SELF.R5[AvlTree].isDefined &&
        OUTPUTS(0).R5[AvlTree].isDefined &&
        OUTPUTS(0).R5[AvlTree] == SELF.R5[AvlTree].get.insert(INPUTS.slice(1, INPUTS.length).map({ (b: Box) =>
          val pk = b.R4[Coll[Byte]].getOrElse(Coll.empty)
          val value = longToByteArray(b.value)
          (pk, value)
        }), getVar[Coll[Byte]](1).get) &&
        HEIGHT >= 2000 &&
        blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash &&
        OUTPUTS(1).value <= 1 &&
        OUTPUTS(1).propositionBytes == feeBytes
    )

    ICOFundingContract(ctx, nextStageScriptHash, feeBytes)
  }.holds

  def failICOFundingSelfIndexNotZero(ctx: Context, nextStageScriptHash: Coll[Byte], feeBytes: Coll[Byte]): Boolean = {
    import ctx._
    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.length > 1 &&
      getVar[Coll[Byte]](1).isDefined &&
      INPUTS.slice(1, INPUTS.length).forall({ (b: Box) => b.R4[Coll[Byte]].isDefined }) &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined &&

      // reason to fail
      INPUTS(0).id != SELF.id
    )

    ICOFundingContract(ctx, nextStageScriptHash, feeBytes)
  } ensuring (_ == false)


  // TODO restore
  @ignore
  def ICOIssuanceContract(ctx: Context, nextStageScriptHash: Coll[Byte], projectPubKey: SigmaProp): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined &&
      OUTPUTS(0).R4[Coll[Byte]].isDefined
    )

    val openTree = SELF.R5[AvlTree].get

    val closedTree = OUTPUTS(0).R5[AvlTree].get

    val digestPreserved = openTree.digest == closedTree.digest
    val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
    val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
    val treeIsClosed = closedTree.enabledOperations == 4

    val tokenId: Coll[Byte] = INPUTS(0).id

    val outputsCountCorrect = OUTPUTS.size == 3
    val secondOutputNoTokens = outputsCountCorrect &&
      OUTPUTS(0).tokens.size == 1 &&
      OUTPUTS(1).tokens.size == 0 &&
      OUTPUTS(2).tokens.size == 0

    val correctTokensIssued = OUTPUTS(0).tokens.size == 1 && SELF.value == OUTPUTS(0).tokens(0)._2

    val correctTokenId = outputsCountCorrect &&
    OUTPUTS(0).R4[Coll[Byte]].get == tokenId &&
      OUTPUTS(0).tokens.size == 1 &&
      OUTPUTS(0).tokens(0)._1 == tokenId

    val valuePreserved = outputsCountCorrect && secondOutputNoTokens && correctTokensIssued && correctTokenId
    val stateChanged = blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash

    val treeIsCorrect = digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed

    projectPubKey.isValid && treeIsCorrect && valuePreserved && stateChanged
  }

  // TODO restore
  @ignore
  def ICOWithdrawalContract(ctx: Context): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty &&
      getVar[Coll[Byte]](2).isDefined &&
      getVar[Coll[Byte]](3).isDefined &&
      getVar[Coll[Int]](4).isDefined &&
      SELF.R4[Coll[Byte]].isDefined &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined
    )

    val removeProof = getVar[Coll[Byte]](2).get
    val lookupProof = getVar[Coll[Byte]](3).get
    val withdrawIndexes = getVar[Coll[Int]](4).get

    val out0 = OUTPUTS(0)

    val tokenId: Coll[Byte] = SELF.R4[Coll[Byte]].get

    val withdrawals: Coll[(Coll[Byte], Long)] = withdrawIndexes.flatMap({ (idx: Int) =>
      if (idx >= 0 && idx < OUTPUTS.length) {
        val b = OUTPUTS(idx)
        if (b.tokens.nonEmpty && b.tokens(0)._1 == tokenId) {
          Coll((blake2b256(b.propositionBytes), b.tokens(0)._2))
        } else {
          Coll((blake2b256(b.propositionBytes), 0L))
        }
      } else {
        Coll.empty[(Coll[Byte], Long)]
      }
    })

    //val withdrawals = OUTPUTS.slice(1, OUTPUTS.size-1).map(...)

    val withdrawValues = withdrawals.map({ (t: (Coll[Byte], Long)) => t._2 })

    val withdrawTotal = withdrawValues.foldLeft(0L, { (t: (Long, Long)) => t._1 + t._2 })

    val toRemove = withdrawals.map({ (t: (Coll[Byte], Long)) => t._1 })

    val initialTree = SELF.R5[AvlTree].get

    // TODO: proper option handling
    val removedValues = initialTree.getMany(toRemove, lookupProof).map({ (o: Option[Coll[Byte]]) => byteArrayToLong(o.getOrElse(Coll.empty)) })
    val valuesCorrect = removedValues == withdrawValues

    val modifiedTree = initialTree.remove(toRemove, removeProof)

    val expectedTree = out0.R5[AvlTree]

    val selfTokensCorrect = SELF.tokens.nonEmpty && SELF.tokens(0)._1 == tokenId
    val selfOutTokensAmount = if (selfTokensCorrect) SELF.tokens(0)._2 else 0
    val soutTokensCorrect = out0.tokens.nonEmpty && out0.tokens(0)._1 == tokenId
    val soutTokensAmount = if (soutTokensCorrect) out0.tokens(0)._2 else 0

    val tokensPreserved = selfTokensCorrect && soutTokensCorrect && (soutTokensAmount + withdrawTotal == selfOutTokensAmount)

    val properTreeModification = modifiedTree == expectedTree

    val selfOutputCorrect = out0.propositionBytes == SELF.propositionBytes

     properTreeModification && valuesCorrect && selfOutputCorrect && tokensPreserved
  }

  //  def dummyContract(ctx: FundingContext): Boolean = {
//    import ctx._
//    require(INPUTS.nonEmpty)
////    INPUTS.nonEmpty
//    INPUTS(0).id == SELF.id
//    //    blake2b256(List[Byte]()) == List[Byte]()
//  }
//
//  def dummyProp(ctx: FundingContext): Boolean = {
//    import ctx._
//    require(
//      INPUTS.nonEmpty &&
//        INPUTS(0).id != SELF.id
//    )
//    //    val contract = new ICOContract {}
//    //    contract.dummyContract(ctx)
//    dummyContract(ctx)
//  } ensuring (_ == false)
}

case object ICOContractVerification extends ICOContract
