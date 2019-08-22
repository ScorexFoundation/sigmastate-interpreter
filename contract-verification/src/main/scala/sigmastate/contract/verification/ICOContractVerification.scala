package sigmastate.contract.verification

import scalan.RType.GeneralType
import scalan.{RType => ERType}
import sigmastate.contract.verification.Helpers._
import special.collection.Coll
import special.sigma.{Box => EBox, SigmaProp => ESigmaProp}
import stainless.annotation.{extern, ignore, library, opaque, pure}
import stainless.collection._
import stainless.lang._

import scala.reflect.ClassTag

//@library
//case class SigmaProp(@extern v: ESigmaProp) {
//  @extern @pure
//  def isValid: Boolean = v.isValid
//}


@library
trait SigmaProp extends SigmaPropT {
  @extern @pure
  override def isValid: Boolean
}

@library
object Helpers {

  @library @extern
  type RType[T] = ERType[T]

  @library @extern
  type SigmaPropT = ESigmaProp

  @library @extern
  case class ListType[A](tItem: RType[A]) extends RType[List[A]] {
    @library @extern
    val classTag: ClassTag[List[A]] = ClassTag[List[A]](classOf[List[A]])
    override def name: String = s"List[${tItem.name}]"
    override def isConstantSize: Boolean = false
  }

  @library @extern
  implicit def collToList[T](coll: Coll[T]): List[T] = ???
  @library @extern
  implicit def optionToOption[T](@extern opt: scala.Option[T]): Option[T] = ???

  @library @extern @pure @opaque
  implicit def listRType[A](implicit cT: RType[A]): RType[List[A]] = ListType[A](cT)

//  @library @extern
//  implicit def listByteRType: RType[List[Byte]] = ???

  @library @extern @pure @opaque
  implicit val ByteType: RType[Byte] = scalan.RType.ByteType
  @library @extern @pure @opaque
  implicit val AvlTreeRType: RType[AvlTree] = new GeneralType(scala.reflect.classTag[AvlTree]) {
    override def isConstantSize: Boolean = true
  }
}

@library
trait Box {
  @library
  def value: Long
  @library
  def id: List[Byte]
  @library @pure @opaque
  def R4[T]: Option[T]
  //  def R4[T](implicit cT: RType[T]): Option[T]

  @library @pure @opaque
  def R5[T]: Option[T]
//  def R5[T](implicit cT: RType[T]): Option[T]

  @library
  def tokens: List[(List[Byte], Long)]
  @library
  def propositionBytes: List[Byte]
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

trait AvlTree {
  def digest: List[Byte]
  def keyLength: Int
  def valueLengthOpt: Option[Int]
  def enabledOperations: Byte
  def getMany(keys: List[List[Byte]], proof: List[Byte]): List[Option[List[Byte]]]
  def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree]
  def remove(operations: List[List[Byte]], proof: List[Byte]): Option[AvlTree]
}

// TODO remove after getOrElse are removed
case object AvlTree extends AvlTree {
  def apply(): AvlTree = ???
  override def digest: List[Byte] = ???
  override def keyLength: Int = ???
  override def valueLengthOpt: Option[Int] = ???
  override def enabledOperations: Byte = ???
  override def getMany(keys: List[List[Byte]], proof: List[Byte]): List[Option[List[Byte]]] = ???
  override def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree] = ???
  override def remove(operations: List[List[Byte]], proof: List[Byte]): Option[AvlTree] = ???
}

trait Context {
  def OUTPUTS: List[Box]
  def INPUTS: List[Box]
  def SELF: Box
  def HEIGHT: Int

  def getVar[T](id: Byte): Option[T]
  def longToByteArray(l: Long): List[Byte]
  def byteArrayToLong(bytes: List[Byte]): Long
  def blake2b256(bytes: List[Byte]): List[Byte]
}

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

  def ICOFundingContract(ctx: Context, nextStageScriptHash: List[Byte], feeBytes: List[Byte]): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.length > 1 &&
      getVar[List[Byte]](1).isDefined &&
      INPUTS.slice(1, INPUTS.length).forall({ (b: Box) => b.R4[List[Byte]].isDefined }) &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined
    )

    val selfIndexIsZero = INPUTS(0).id == SELF.id

    val inputsCount = INPUTS.size

    val proof = getVar[List[Byte]](1).get

    val toAdd: List[(List[Byte], List[Byte])] = INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      // TODO avoid getOrElse
      val pk = b.R4[List[Byte]].getOrElse(List())
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

  def proveICOFundingContractForHeightAbove2000(ctx: Context, nextStageScriptHash: List[Byte], feeBytes: List[Byte]): Boolean = {
    import ctx._
    require(
      HEIGHT > 0 &&
        INPUTS.length > 1 &&
        OUTPUTS.length == 2 &&
        INPUTS(0).id == SELF.id &&
        getVar[List[Byte]](1).isDefined &&
        INPUTS.slice(1, INPUTS.length).forall({ (b: Box) => b.R4[List[Byte]].isDefined }) &&
        SELF.R5[AvlTree].isDefined &&
        OUTPUTS(0).R5[AvlTree].isDefined &&
        OUTPUTS(0).R5[AvlTree] == SELF.R5[AvlTree].get.insert(INPUTS.slice(1, INPUTS.length).map({ (b: Box) =>
          val pk = b.R4[List[Byte]].getOrElse(List())
          val value = longToByteArray(b.value)
          (pk, value)
        }), getVar[List[Byte]](1).get) &&
        HEIGHT >= 2000 &&
        blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash &&
        OUTPUTS(1).value <= 1 &&
        OUTPUTS(1).propositionBytes == feeBytes
    )
    ICOFundingContract(ctx, nextStageScriptHash, feeBytes)
  }.holds

  def failICOFundingSelfIndexNotZero(ctx: Context, nextStageScriptHash: List[Byte], feeBytes: List[Byte]): Boolean = {
    import ctx._
    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.length > 1 &&
      getVar[List[Byte]](1).isDefined &&
      INPUTS.slice(1, INPUTS.length).forall({ (b: Box) => b.R4[List[Byte]].isDefined }) &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined &&

      // reason to fail
      INPUTS(0).id != SELF.id
    )

    ICOFundingContract(ctx, nextStageScriptHash, feeBytes)
  } ensuring (_ == false)


  // TODO fix
  @ignore
  def ICOIssuanceContract(ctx: Context, nextStageScriptHash: List[Byte], projectPubKey: SigmaProp): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined &&
      OUTPUTS(0).R4[List[Byte]].isDefined
    )

    val openTree = SELF.R5[AvlTree].get

    val closedTree = OUTPUTS(0).R5[AvlTree].get

    val digestPreserved = openTree.digest == closedTree.digest
    val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
    val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
    val treeIsClosed = closedTree.enabledOperations == 4

    val tokenId: List[Byte] = INPUTS(0).id

    val outputsCountCorrect = OUTPUTS.size == 3
    val secondOutputNoTokens = outputsCountCorrect &&
      OUTPUTS(0).tokens.size == 1 &&
      OUTPUTS(1).tokens.size == 0 &&
      OUTPUTS(2).tokens.size == 0

    val correctTokensIssued = OUTPUTS(0).tokens.size == 1 && SELF.value == OUTPUTS(0).tokens(0)._2

    val correctTokenId = outputsCountCorrect &&
    OUTPUTS(0).R4[List[Byte]].get == tokenId &&
      OUTPUTS(0).tokens.size == 1 &&
      OUTPUTS(0).tokens(0)._1 == tokenId

    val valuePreserved = outputsCountCorrect && secondOutputNoTokens && correctTokensIssued && correctTokenId
    val stateChanged = blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash

    val treeIsCorrect = digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed

    projectPubKey.isValid && treeIsCorrect && valuePreserved && stateChanged
  }

  // TODO fix
  @ignore
  def ICOWithdrawalContract(ctx: Context): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty &&
      getVar[List[Byte]](2).isDefined &&
      getVar[List[Byte]](3).isDefined &&
      getVar[List[BigInt]](4).isDefined &&
      SELF.R4[List[Byte]].isDefined &&
      SELF.R5[AvlTree].isDefined &&
      OUTPUTS(0).R5[AvlTree].isDefined
    )

    val removeProof = getVar[List[Byte]](2).get
    val lookupProof = getVar[List[Byte]](3).get
    val withdrawIndexes = getVar[List[BigInt]](4).get

    val out0 = OUTPUTS(0)

    val tokenId: List[Byte] = SELF.R4[List[Byte]].get

    val withdrawals: List[(List[Byte], Long)] = withdrawIndexes.flatMap({ (idx: BigInt) =>
      if (idx >= 0 && idx < OUTPUTS.length) {
        val b = OUTPUTS(idx)
        if (b.tokens.nonEmpty && b.tokens(0)._1 == tokenId) {
          List((blake2b256(b.propositionBytes), b.tokens(0)._2))
        } else {
          List((blake2b256(b.propositionBytes), 0L))
        }
      } else {
        List()
      }
    })

    //val withdrawals = OUTPUTS.slice(1, OUTPUTS.size-1).map(...)

    val withdrawValues = withdrawals.map({ (t: (List[Byte], Long)) => t._2 })

    val withdrawTotal = withdrawValues.foldLeft(0L) { (l1: Long, l2: Long) => l1 + l2 }

    val toRemove = withdrawals.map({ (t: (List[Byte], Long)) => t._1 })

    val initialTree = SELF.R5[AvlTree].get

    // TODO: proper option handling
    val removedValues = initialTree.getMany(toRemove, lookupProof).map({ (o: Option[List[Byte]]) => byteArrayToLong(o.getOrElse(List())) })
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
